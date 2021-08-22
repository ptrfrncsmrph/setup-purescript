module Setup.BuildPlan
  ( constructBuildPlan
  , BuildPlan
  , InstallMethod(..)
  , TarballOpts
  , NPMPackage
  ) where
import Prelude

import Affjax (URL)
import Control.Monad.Except.Trans (ExceptT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError, (.:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), fromRight')
import Data.Foldable (elem, fold)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Version (Version, parseVersion)
import Data.Version as Version
import Effect (Effect)
import Effect.Aff (error, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import GitHub.Actions.Core as Core
import Node.Path (FilePath)
import Node.Path as Path
import Partial.Unsafe (unsafeCrashWith)
import Setup.Data.Key (Key)
import Setup.Data.Key as Key
import Setup.Data.Platform (Platform(..), platform)
import Setup.Data.Tool (Tool(..), ToolRepository, name, repository)
import Setup.Data.Tool as Tool
import Text.Parsing.Parser (parseErrorMessage)
import Text.Parsing.Parser as ParseError

-- | The list of tools that should be downloaded and cached by the action
type BuildPlan = Array InstallMethod

-- | Construct the list of tools that sholud be downloaded and cached by the action
constructBuildPlan :: Json -> ExceptT Error Effect BuildPlan
constructBuildPlan json = map Array.catMaybes $ traverse (resolve json) Tool.allTools

-- | The parsed value of an input field that specifies a version
data VersionField = Latest | Exact Version

-- | Attempt to read the value of an input specifying a tool version
getVersionField :: Key -> ExceptT Error Effect (Maybe VersionField)
getVersionField key = do
  value <- Core.getInput' (Key.toString key)
  case value of
    "" ->
      pure Nothing
    "latest" ->
      pure (pure Latest)
    val -> case Version.parseVersion val of
      Left msg -> do
        liftEffect $ Core.error $ fold [ "Failed to parse version ", val ]
        throwError (error (ParseError.parseErrorMessage msg))
      Right version ->
        pure (pure (Exact version))

-- | How a tool will be installed: either a tarball from a URL, or an NPM package
-- | at a particular version.
data InstallMethod = Tarball TarballOpts | NPM NPMPackage

-- | The source used to download a tarball and its path inside the extracted
-- | directory.
type TarballOpts =
  { source :: URL
  , getExecutablePath :: FilePath -> FilePath
  , version :: String
  , name :: String
  }

-- | An NPM package. Example: "purescript-psa@0.7.2"
type NPMPackage =
  { version :: String
  , name :: String
  }

-- | The installation method for a tool, which includes the source path necessary
-- | to download or install the tool.
-- installMethod :: Tool -> Version -> InstallMethod
-- installMethod tool version = do

-- | Resolve the exact version to provide for a tool in the environment, based
-- | on the action.yml file.
resolve :: Json -> Tool -> ExceptT Error Effect (Maybe InstallMethod)
resolve versionsContents tool = do
  let
    key = Key.fromTool tool
    toolName = name tool
    toolRepo = repository tool

  field <- getVersionField key

  maybeVersion <- case field of
    Nothing -> pure Nothing

    Just (Exact v) -> liftEffect do
      Core.info "Found exact version"
      pure (pure (Exact v))

    Just Latest
      -- If tool is an NPM package, use the "latest" version
      | tool `elem` [ Psa, PursTidy ] -> liftEffect do
      Core.info $ fold [ "Fetching latest tag for ", toolName ]

      pure (pure Latest)

      | otherwise -> liftEffect do
      Core.info $ fold [ "Fetching latest tag for ", toolName ]

      let
        version = lmap printJsonDecodeError $ (_ .: toolName) =<< decodeJson versionsContents
        parse = lmap parseErrorMessage <<< Version.parseVersion

      case parse =<< version of
        Left e -> do
          Core.setFailed $ fold [ "Unable to parse version: ", e ]
          throwError $ error "Unable to complete fetching version."

        Right v -> do
          pure (pure (Exact v))
  
  
  --- TODO ---

  let
    formatArgs = { repo: toolRepo, tag: _, tarball: _ }

    formatGitHub' = map formatGitHub <<< formatArgs

    unsafeVersion str = fromRight' (\_ -> unsafeCrashWith "Unexpected Left") $ parseVersion str

    executableName = case platform of
      Windows -> toolName <> ".exe"
      _ -> toolName

  case tool of
    PureScript -> Tarball
      { source: formatGitHub' ("v" <> versionStr) case platform of
          Windows -> "win64"
          Mac -> "macos"
          Linux -> "linux64"
      , getExecutablePath: \p -> Path.concat [ p, "purescript", executableName ]
      , version: _
      , name: toolName
      }

    Spago -> Tarball
      { source: formatGitHub' versionStr
          -- Spago has changed naming conventions from version to version
          if version >= unsafeVersion "0.18.1" then case platform of
            Windows -> "Windows"
            Mac -> "macOS"
            Linux -> "Linux"
          else if version == unsafeVersion "0.18.0" then case platform of
            Windows -> "windows-latest"
            Mac -> "macOS-latest"
            Linux -> "linux-latest"
          else case platform of
            Windows -> "windows"
            Mac -> "osx"
            Linux -> "linux"
      , getExecutablePath: \p -> Path.concat [ p, executableName ]
      , version: versionStr
      , name: toolName
      }

    Psa ->
      NPM
        { name: "purescript-psa" 
        , version: Version.showVersion version
        }

    PursTidy ->
      NPM
        { name: "purs-tidy" 
        , version: Version.showVersion version
        }

    Zephyr -> Tarball
      { source: formatGitHub' ("v" <> versionStr) case platform of
          Windows -> "Windows"
          Mac -> "macOS"
          Linux -> "Linux"
      , getExecutablePath: \p -> Path.concat [ p, "zephyr", executableName ]
      , version: versionStr
      , name: toolName
      }
  where

  formatGitHub :: { repo :: ToolRepository, tag :: String, tarball :: String } -> String
  formatGitHub { repo, tag, tarball } =
    -- Example: https://github.com/purescript/purescript/releases/download/v0.13.8/win64.tar.gz
    fold
      [ "https://github.com/"
      , repo.owner
      , "/"
      , repo.name
      , "/releases/download/"
      , tag
      , "/"
      , tarball
      , ".tar.gz"
      ]
