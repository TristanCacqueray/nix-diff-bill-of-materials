module Main where

import Data.Attoparsec.Text.Lazy qualified
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified
import Nix.Derivation qualified
import Options.Applicative qualified as O
import RIO
import Turtle qualified

data Material = Material
    { name :: Text
    , version :: Text
    , src :: FilePath
    }

data Source = Source
    { out :: FilePath
    , url :: Text
    }

showMaterial :: Material -> Text
showMaterial m = mconcat [m.name, "-", m.version, ": ", Text.pack m.src]

showSource :: Source -> Text
showSource s = mconcat [Text.pack s.out, ": ", s.url]

type Drv = Nix.Derivation.Derivation FilePath Text

-- rust crate from crane does not use pname/version
parseRustMaterial :: Drv -> Maybe Material
parseRustMaterial drv = do
    (name, version) <- getName
    src <- getSrc
    pure Material{name, src, version}
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

parseMaterial :: Drv -> Maybe Material
parseMaterial drv =
    parseRustMaterial drv <|> do
        name <- Map.lookup "pname" drv.env
        version <- Map.lookup "version" drv.env
        src <- Text.unpack <$> Map.lookup "src" drv.env
        pure Material{name, src, version}

parseSource :: Drv -> Maybe Source
parseSource drv = do
    -- Question: why some (most?) urls are not using the builtin:fetch?
    -- This hack seems necessary to detect fetchFromGitHub usage.
    out <- Text.unpack <$> Map.lookup "out" drv.env
    url <- Map.lookup "urls" drv.env
    pure Source{out, url}

newtype Visited = Visited (IORef (Set FilePath))

newVisited :: IO Visited
newVisited = Visited <$> newIORef mempty

isVisited :: Visited -> FilePath -> IO Bool
isVisited (Visited ref) fp = do
    s <- readIORef ref
    if fp `Set.member` s
        then pure True
        else do
            writeIORef ref (Set.insert fp s)
            pure False

newtype Sources = Sources (IORef (Map FilePath Text))

newSources :: IO Sources
newSources = Sources <$> newIORef mempty

registerSource :: Sources -> Source -> IO ()
registerSource (Sources ref) src = modifyIORef' ref (Map.insert src.out src.url)

lookupSource :: Sources -> FilePath -> IO (Maybe Text)
lookupSource (Sources ref) src = Map.lookup src <$> readIORef ref

type MaterialSource = (Material, Maybe Text)

showMaterialSource :: MaterialSource -> Text
showMaterialSource (mat, mUrl) = mconcat [showMaterial mat, " <", fromMaybe "unknown" mUrl, ">"]

getBOM :: FilePath -> IO [MaterialSource]
getBOM root = do
    visited <- newVisited
    sources <- newSources
    let
        go :: FilePath -> IO [MaterialSource]
        go fp = do
            known <- isVisited visited fp
            if known
                then pure []
                else do
                    drv <- readDrv fp
                    childs <- concat <$> traverse go (Map.keys drv.inputDrvs)
                    case (Left <$> parseMaterial drv <|> Right <$> parseSource drv) of
                        Just (Left mat) -> do
                            url <- lookupSource sources mat.src
                            pure ((mat, url) : childs)
                        Just (Right src) -> do
                            registerSource sources src
                            pure childs
                        Nothing -> do
                            -- TODO, what are those?
                            putStrLn $ fp <> ": unknown"
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

data Action = List

parserInfo :: O.ParserInfo Options
parserInfo =
    O.info
        (O.helper <*> parseOptions)
        (O.fullDesc <> O.header "Inspect or compare the sources of derivations.")
  where
    parseOptions =
        pure Options
            <*> O.subparser (listCommand)
            <*> O.strArgument (O.metavar "INSTALLABLE")
    listCommand = O.command "list" (O.info (pure List) (O.progDesc "List the materials"))

readProc :: Text -> [Text] -> Turtle.Shell Text
readProc cmd args = Turtle.lineToText <$> Turtle.inproc cmd args mempty

main :: IO ()
main = do
    options <- O.execParser parserInfo

    mats <- getBOM =<< installableToDrvPath options.target

    case options.action of
        List -> traverse_ (Text.putStrLn . showMaterialSource) mats
