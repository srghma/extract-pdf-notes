module PdfAnkiTranslator.Lingolive.Actions.Translation where

import Data.Argonaut.Decode
import PdfAnkiTranslator.Lingolive.Types
import Protolude

import Affjax as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Decoders as Decoders
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Generic.Rep.Show (genericShow)
import Foreign.Object (Object)

type Config
  = { token :: String
    , serviceUrl :: String
    }

data Response
  = Response__Success (NonEmptyArray ArticleModel)
  | Response__NoDictionaries
  | Response__NoTranslationForWord

translation :: Config -> String -> Aff Response
translation config word = undefined
