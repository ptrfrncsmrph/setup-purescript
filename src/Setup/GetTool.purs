module Setup.GetTool (getTool) where

import Prelude

import Control.Monad.Except.Trans (ExceptT, mapExceptT)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import GitHub.Actions.Core as Core
import GitHub.Actions.Exec as Exec
import GitHub.Actions.ToolCache as ToolCache
import Setup.BuildPlan (InstallMethod(..))
import Setup.Data.Platform (Platform(..), platform)

getTool :: InstallMethod -> ExceptT Error Aff Unit
getTool = do
  case _ of
    Tarball opts -> do
      liftEffect $ Core.info $ fold [ "Fetching ", opts.name ]
      mbPath <- mapExceptT liftEffect $ ToolCache.find { arch: Nothing, toolName: opts.name, versionSpec: opts.version }
      case mbPath of
        Just path -> liftEffect do
          Core.info $ fold [ "Found cached version of ", opts.name ]
          Core.addPath path

        Nothing -> do
          downloadPath <- ToolCache.downloadTool' opts.source
          extractedPath <- ToolCache.extractTar' downloadPath
          cached <- ToolCache.cacheFile { sourceFile: opts.getExecutablePath extractedPath, tool: opts.name, version: opts.version, targetFile: opts.name, arch: Nothing }

          liftEffect do
            Core.info $ fold [ "Cached path ", cached, ", adding to PATH" ]
            Core.addPath cached

    NPM package -> do 
      liftEffect $ Core.info $ fold [ "Fetching ", package.name ]

      let packageString = package.name <> "@" <> package.version
      void $ case platform of
        Windows ->
          Exec.exec { command: "npm", args: Just [ "install", "-g", packageString ], options: Nothing }
        _ ->
          Exec.exec { command: "sudo npm", args: Just [ "install", "-g", packageString ], options: Nothing }
