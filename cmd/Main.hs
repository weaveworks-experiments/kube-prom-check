module Main (main) where

import Protolude

import Data.Aeson.Types (typeMismatch)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Regex (mkRegex, subRegex)
import Data.Yaml (ParseException, FromJSON(..), Value(..), (.:), decodeFileEither)
import qualified Options.Applicative as Opt
import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.FilePath ((</>), takeExtension)
import System.IO (hClose, openTempFile)
import System.Process.Text (readProcessWithExitCode)


options :: Opt.ParserInfo [FilePath]
options =
  Opt.info (Opt.helper <*> some (Opt.argument Opt.str (Opt.metavar "FILENAME"))) description
  where
    description = fold
      [ Opt.fullDesc
      , Opt.progDesc "Lint Kubernetes monitoring configuration"
      , Opt.header  "monitoring-lint - lint monitoring configuration"
      ]

type Namespace = Text
type Name = Text

data ConfigMap = ConfigMap Name Namespace (Map FilePath Text) deriving (Eq, Show)

instance FromJSON ConfigMap where
  parseJSON (Object v) =
    ConfigMap <$> ((v .: "metadata") >>= (.: "name"))
              <*> ((v .: "metadata") >>= (.: "namespace"))
              <*> (v .: "data")
  parseJSON x = typeMismatch "ConfigMap" x

data LintError
  = YamlError FilePath ParseException
  | PromConfigError FilePath FilePath Text
  deriving (Show)

formatError :: LintError -> Text
formatError (PromConfigError kubeFile promFile err) = toS kubeFile <> ":" <> toS promFile <> ":\n" <> stripFirstLine err
  where
    stripFirstLine = maybe err Text.unlines . tailMay . Text.lines
formatError (YamlError kubeFile err) = toS kubeFile <> ":\n  Invalid Kubernetes config file: " <> show err

-- | Unpack the files of a ConfigMap to the given directory. Assumes the
-- directory exists.
unpackConfig :: FilePath -> Map FilePath Text -> IO [FilePath]
unpackConfig directory configFiles =
  for (Map.toAscList configFiles) $ \(filename, content) -> do
    let fn = directory </> filename
    Text.writeFile fn content
    pure fn

checkConfig :: FilePath -> IO (Maybe Text)
checkConfig path =
  case takeExtension path of
    ".yml" -> checkPrometheusConfig
    ".yaml" -> checkPrometheusConfig
    ".rules" -> checkPrometheusRules
    _ -> pure Nothing  -- TODO: Skipped file. Maybe there should be a warning?

  where
    checkPrometheusRules = do
      (exitCode, _, err) <- readProcessWithExitCode "promtool" ["check-rules", path] ""
      case exitCode of
        ExitSuccess -> pure Nothing
        ExitFailure _ -> pure (Just err)

    checkPrometheusConfig = do
      fudgePrometheusConfig path
      (exitCode, _, err) <- readProcessWithExitCode "promtool" ["check-config", path] ""
      case exitCode of
        ExitSuccess -> pure Nothing
        ExitFailure _ -> pure (Just err)

    fudgePrometheusConfig :: FilePath -> IO ()
    fudgePrometheusConfig p = do
      content <- Text.readFile p
      newFile <- mkTmpFile
      -- XXX: Slow. Unfortunately all the other regex libraries depend on C libs which might not be present.
      let newContent = subRegex (mkRegex "bearer_token_file: .*") (toS content) ("bearer_token_file: " <> newFile)
      Text.writeFile p (toS newContent)

    mkTmpFile = do
      tempDir <- getTemporaryDirectory
      bracket (openTempFile tempDir "empty-file") (hClose . snd) (\(p, h) -> Text.hPutStr h "" *> pure p)


checkConfigFile :: FilePath -> IO [LintError]
checkConfigFile filename = do
  configMap <- decodeFileEither filename
  case configMap of
    Left parseError -> pure [YamlError filename parseError]
    Right (ConfigMap _ _ configFiles) -> do
      tmpDir <- getTemporaryDirectory
      files <- unpackConfig tmpDir configFiles
      catMaybes <$> traverse checkConfig' files
  where
    checkConfig' :: FilePath -> IO (Maybe LintError)
    checkConfig' path = map (PromConfigError filename path) <$> checkConfig path

main :: IO ()
main = do
  config <- Opt.execParser options
  errors <- join <$> for config checkConfigFile
  case errors of
    [] -> exitSuccess
    es -> do
      for_ es $ putStr . formatError
      exitWith (ExitFailure 1)
