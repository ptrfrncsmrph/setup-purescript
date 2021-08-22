module Setup.Data.Tool where

import Prelude

import Affjax (URL)
import Data.Enum (class Enum, upFromIncluding)
import Data.Foldable (elem)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericPred, genericSucc)
import Node.Path (FilePath)

data Tool
  = PureScript
  | Spago
  | Psa
  | PursTidy
  | Zephyr

derive instance eqTool :: Eq Tool
derive instance ordTool :: Ord Tool
derive instance genericTool :: Generic Tool _

instance boundedTool :: Bounded Tool where
  bottom = genericBottom
  top = genericTop

instance enumTool :: Enum Tool where
  succ = genericSucc
  pred = genericPred

-- | A list of all available tools in the toolchain
allTools :: Array Tool
allTools = upFromIncluding bottom

-- | Tools that are required in the toolchain
requiredTools :: Array Tool
requiredTools = [ PureScript, Spago ]

-- | Tools that are required in the toolchain
required :: Tool -> Boolean
required tool = elem tool requiredTools

name :: Tool -> String
name = case _ of
  PureScript -> "purs"
  Spago -> "spago"
  Psa -> "psa"
  PursTidy -> "purs-tidy"
  Zephyr -> "zephyr"

-- | The source repository for a tool (whether on GitHub or Gitlab)
type ToolRepository = { owner :: String, name :: String }

repository :: Tool -> ToolRepository
repository = case _ of
  PureScript ->
    { owner: "purescript", name: "purescript" }

  Spago ->
    { owner: "purescript", name: "spago" }

  Psa ->
    { owner: "natefaubion", name: "purescript-psa" }

  PursTidy ->
    { owner: "natefaubion", name: "purescript-tidy" }

  Zephyr ->
    { owner: "coot", name: "zephyr" }

