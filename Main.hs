{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

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

data DrvInfo
    = MaterialInfo
        { name :: Text
        , version :: Text
        , src :: FilePath
        }
    | Source
        { out :: FilePath
        , url :: Text
        }

type Drv = Nix.Derivation.Derivation FilePath Text

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

parseMaterial :: Drv -> Maybe DrvInfo
parseMaterial drv =
    parseRustMaterial drv <|> do
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

data Material = Material
    { name :: Text
    , version :: Text
    , url :: Maybe Text
    }

type Materials = Tree.Forest Material

showMaterial :: Int -> Material -> Text
showMaterial depth mat =
    mconcat
        [Text.replicate depth " ", mat.name, " ", mat.version, " <", fromMaybe "unknown" mat.url, ">"]

showMaterials :: Materials -> Text
showMaterials mats = Text.unlines $ concatMap (showTree 0) mats
  where
    showTree :: Int -> Tree.Tree Material -> [Text]
    showTree depth (Tree.Node mat childs) =
        showMaterial depth mat : concatMap (showTree $ depth + 1) childs

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
            let mDrvInfo = parseMaterial drv <|> parseSource drv
            case mDrvInfo of
                Just (MaterialInfo{name}) | name `Set.member` defaultIgnore -> pure mempty
                _ -> goDrv drv mDrvInfo

        goDrv drv mDrvInfo = do
            childs <- concat <$> traverse go (Map.keys drv.inputDrvs)
            case mDrvInfo of
                Just (MaterialInfo{name, version, src}) -> do
                    url <- Map.lookup src <$> readIORef sources
                    pure [Tree.Node Material{name, version, url} childs]
                Just (Source{out, url}) -> do
                    modifyIORef' sources $ Map.insert out url
                    pure childs
                Nothing -> do
                    -- Question: how to filter non package derivation like bootstrap or build env wrapper?
                    -- putStrLn $ fp <> ": unknown"
                    pure childs
    go root

readDrv :: FilePath -> IO Drv
readDrv fp = do
    text <- Data.Text.Lazy.IO.readFile fp
    case Data.Attoparsec.Text.Lazy.parse Nix.Derivation.parseDerivation text of
        Data.Attoparsec.Text.Lazy.Done "" drv -> pure drv
        res -> error (show res)

installableToDrvPath :: Text -> IO FilePath
installableToDrvPath installable = do
    Turtle.procs "nix" ["build", installable] mempty
    Text.unpack <$> Turtle.single (readProc "nix" ["path-info", "--derivation", installable])

data Options = Options
    { action :: Action
    , target :: Text
    }

data Action = List | Count

parserInfo :: O.ParserInfo Options
parserInfo =
    O.info
        (O.helper <*> parseOptions)
        (O.fullDesc <> O.header "Inspect or compare the sources of derivations.")
  where
    parseOptions =
        pure Options
            <*> O.subparser (listCommand <> countCommand)
            <*> O.strArgument (O.metavar "INSTALLABLE")
    listCommand = O.command "list" (O.info (pure List) (O.progDesc "List the materials"))
    countCommand = O.command "count" (O.info (pure Count) (O.progDesc "Count the materials"))

readProc :: Text -> [Text] -> Turtle.Shell Text
readProc cmd args = Turtle.lineToText <$> Turtle.inproc cmd args mempty

main :: IO ()
main = do
    options <- O.execParser parserInfo

    mats <- getBOM =<< installableToDrvPath options.target

    case options.action of
        List -> Text.putStrLn $ showMaterials mats
        Count -> print (sum $ length <$> mats)
