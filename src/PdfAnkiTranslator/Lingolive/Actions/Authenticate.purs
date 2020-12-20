module PdfAnkiTranslator.Lingolive.Actions.Authenticate where

import Data.Argonaut.Decode
import Protolude
import Affjax as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Decoders as Decoders
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Object (Object)
import PdfAnkiTranslator.Lingolive.Types

type Config
  = { token :: String
    , serviceUrl :: String
    }

data Response
  = Response__Success (Array ArticleModel)
  | Response__NoDictionaries
  | Response__NoTranslationForWord

translation :: Config -> String -> Aff (Array ArticleModel)
translation config word = undefined

