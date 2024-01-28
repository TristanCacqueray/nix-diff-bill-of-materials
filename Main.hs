-- |
-- Module      :  Main
-- Copyright   :  Tristan Cacqueray
-- Maintainer  :  tdecacqu@redhat.com
--
-- SPDX-License-Identifier: MIT
--
-- Main entry point for the nix-diff-bill-of-materials application.
module Main where

import Data.Attoparsec.Text.Lazy qualified
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified
import Data.Tree qualified as Tree
import Nix.Derivation qualified
import Options.Applicative qualified as O
import RIO
import Turtle qualified

-------------------------------------------------------------------------------
-- Low level derivation parser: 'DrvInfo'
-------------------------------------------------------------------------------

-- | Raw nix derivations are parsed into a 'DrvInfo'.
data DrvInfo
    = -- | This is a package:
      MaterialInfo
        { name :: Text
        , version :: Text
        , src :: FilePath
        }
    | -- | This is a source fetcher:
      Source
        { out :: FilePath
        , url :: Text
        }

parseMaterial :: Drv -> Maybe DrvInfo
parseMaterial drv = do
    name <- Map.lookup "pname" drv.env
    version <- Map.lookup "version" drv.env
    src <- Text.unpack <$> Map.lookup "src" drv.env
    pure MaterialInfo{name, src, version}

parseSource :: Drv -> Maybe DrvInfo
parseSource drv = do
    -- Question: why some (most?) urls are not using the builtin:fetch?
    -- This hack seems necessary to detect fetchFromGitHub usage.
    out <- Text.unpack <$> Map.lookup "out" drv.env
    url <- Map.lookup "urls" drv.env
    pure Source{out, url}

-- rust crate from crane does not use pname/version
parseRustMaterial :: Drv -> Maybe DrvInfo
parseRustMaterial drv = do
    (name, version) <- getName
    src <- getSrc
    pure MaterialInfo{name, src, version}
  where
    getName = do
        name <- Map.lookup "name" drv.env
        let prefix = "cargo-package-"
        nameVersion <-
            if prefix `Text.isPrefixOf` name
                then pure $ Text.drop (Text.length prefix) name
                else Nothing
        let (name', version) = Text.breakOnEnd "-" nameVersion
        pure (Text.dropEnd 1 name', version)

    getSrc = do
        cmd <- Map.lookup "buildCommand" drv.env
        case drop 5 (Text.words cmd) of
            src : _ -> pure $ Text.unpack src
            _ -> Nothing

parseDrvInfo :: Drv -> Maybe DrvInfo
parseDrvInfo drv = parseMaterial drv <|> parseSource drv <|> parseRustMaterial drv

type Drv = Nix.Derivation.Derivation FilePath Text

readDrv :: FilePath -> IO Drv
readDrv fp = do
    text <- Data.Text.Lazy.IO.readFile fp
    case Data.Attoparsec.Text.Lazy.parse Nix.Derivation.parseDerivation text of
        Data.Attoparsec.Text.Lazy.Done "" drv -> pure drv
        res -> error (show res)

-------------------------------------------------------------------------------
-- High level info: 'Material'
-------------------------------------------------------------------------------

-- | After walking the tree, the 'DrvInfo' are converted into a tree of 'Material'.
data Material = Material
    { name :: Text
    , version :: Text
    , src :: FilePath
    , url :: Maybe Text
    }

type Materials = Tree.Forest Material

showMaterial :: Int -> Material -> Text
showMaterial depth mat =
    mconcat
        [Text.replicate (depth * 2) " ", mat.name, " ", mat.version, " <", fromMaybe "unknown" mat.url, ">"]

showMaterials :: Materials -> Text
showMaterials mats = Text.unlines $ concatMap (showTree 0) mats
  where
    showTree :: Int -> Tree.Tree Material -> [Text]
    showTree depth (Tree.Node mat childs) =
        showMaterial depth mat : concatMap (showTree $ depth + 1) childs

getBOM :: FilePath -> IO Materials
getBOM root = do
    visited <- newIORef mempty
    let
        visitOnce fp act = do
            s <- readIORef visited
            if fp `Set.member` s
                then pure []
                else do
                    modifyIORef' visited $ Set.insert fp
                    act

    -- Store sources url from inputDrvs to lookup for the material
    sources <- newIORef mempty
    let
        go :: FilePath -> IO Materials
        go fp = visitOnce fp do
            drv <- readDrv fp
            childs <- concat <$> traverse go (Map.keys drv.inputDrvs)
            case parseDrvInfo drv of
                Just (MaterialInfo{name, version, src}) -> do
                    url <- Map.lookup src <$> readIORef sources
                    pure [Tree.Node Material{name, version, url, src} childs]
                Just (Source{out, url}) -> do
                    modifyIORef' sources $ Map.insert out url
                    pure childs
                Nothing -> do
                    -- Question: how to filter non package derivation like bootstrap or build env wrapper?
                    -- putStrLn $ fp <> ": unknown"
                    pure childs
    go root

-------------------------------------------------------------------------------
-- Filter implementation
-------------------------------------------------------------------------------

filterMaterials :: (Material -> Bool) -> Materials -> Materials
filterMaterials p = map (Tree.foldTree f)
  where
    f root childs = Tree.Node root (filter (not . p . Tree.rootLabel) childs)

removeDefaults :: Material -> Bool
removeDefaults mat = mat.name `Set.member` defaultIgnore

-- | A list of derivation name we don't want to cover.
-- note: this match any package, how to filter std build env?
defaultIgnore :: Set Text
defaultIgnore =
    Set.fromList $ mconcat [stdenv, glib, gnu, libs, tools, comp, utils, build, rust, haskell]
  where
    stdenv = ["gcc", "xgcc", "clang", "llvm", "compiler-rt-libc", "linux-headers", "glibc", "coreutils", "binutils", "patch", "patchelf", "gmp"]
    glib = ["glibc", "glibc-locales"]
    gnu = ["gnutar", "gnumake", "gnugrep", "gnused", "gawk"]
    libs = ["attr", "acl", "gmp-with-cxx", "gmp-with-cxx-stage4", "libidn2", "libmpc", "pcre", "pcre2", "mpfr", "pcre-light", "isl", "libunistring"]
    comp = ["gzip", "zlib", "bzip2", "xz", "unzip"]
    utils = ["findutils", "diffutils", "patchutils"]
    tools = ["file", "rsync", "curl", "ed", "perl", "zstd", "pkg-config", "libffi", "jq", "openssl"]
    build = ["bison", "bazel", "cmake", "ninja", "python3-minimal", "poetry-core", "sphinx"]
    rust = ["cargo", "rust-docs", "rust-std", "cargo", "rustc", "crane-utils"]
    haskell = ["ghc"]

-------------------------------------------------------------------------------
-- Diff implementation
-------------------------------------------------------------------------------

data Diff
    = Added Material
    | Modified Material Material

diffMaterials :: Materials -> Materials -> [Diff]
diffMaterials src dst = Map.foldMapWithKey mkDiff dstMap
  where
    mkDiff package dstMat = case Map.lookup package srcMap of
        Nothing -> [Added dstMat]
        Just srcMat
            | dstMat.src == srcMat.src -> []
            | otherwise -> [Modified srcMat dstMat]
    srcMap = toMap src
    dstMap = toMap dst
    toMap mats = Map.fromList $ map (\mat -> (mat.name, mat)) $ concatMap Tree.flatten mats

renderDiff :: [Diff] -> IO ()
renderDiff = traverse_ go
  where
    go = \case
        Added mat -> Text.putStrLn $ mconcat ["Added: ", showMaterial 0 mat]
        Modified smat dmat -> Text.putStrLn $ mconcat ["Modified ", smat.name, ": ", smat.version, " -> ", dmat.version]

-------------------------------------------------------------------------------
-- CLI entry point
-------------------------------------------------------------------------------

data Options = Options
    { action :: Action
    , target :: Text
    }

data Action = List | Count | Diff Text

parserInfo :: O.ParserInfo Options
parserInfo =
    O.info
        (O.helper <*> parseOptions)
        (O.fullDesc <> O.header "Inspect or compare the sources of derivations.")
  where
    parseOptions =
        pure Options
            <*> O.subparser (listCommand <> countCommand <> diffCommand)
            <*> O.strArgument (O.metavar "INSTALLABLE")
    listCommand = O.command "list" (O.info (pure List) (O.progDesc "List the materials"))
    countCommand = O.command "count" (O.info (pure Count) (O.progDesc "Count the materials"))
    diffCommand = O.command "diff" (O.info (Diff <$> O.strArgument (O.metavar "INSTALLABLE")) (O.progDesc "Diff with"))

readProc :: Text -> [Text] -> Turtle.Shell Text
readProc cmd args = Turtle.lineToText <$> Turtle.inproc cmd args mempty

getMaterials :: Text -> IO Materials
getMaterials target = filterMaterials removeDefaults <$> (getBOM =<< installableToDrvPath target)
  where
    installableToDrvPath installable = do
        Turtle.procs "nix" ["build", installable] mempty
        Text.unpack <$> Turtle.single (readProc "nix" ["path-info", "--derivation", installable])

main :: IO ()
main = do
    options <- O.execParser parserInfo

    mats <- getMaterials options.target

    case options.action of
        List -> Text.putStrLn $ showMaterials mats
        Count -> print (sum $ length <$> mats)
        Diff target -> do
            omats <- getMaterials target
            renderDiff (diffMaterials omats mats)
