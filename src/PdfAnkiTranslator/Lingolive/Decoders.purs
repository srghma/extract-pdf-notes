module PdfAnkiTranslator.Lingolive.Decoders where

import Data.Argonaut.Decode
import Protolude

import Affjax as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Decoders as Decoders
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Object (Object)
import PdfAnkiTranslator.Lingolive.Types

decodeNodeType_Text :: Json -> Either JsonDecodeError NodeType_Text
decodeNodeType_Text = decodeJson >=> \obj -> do
  isItalics  <- obj .: "IsItalics"
  isAccent   <- obj .: "IsAccent"
  text       <- obj .: "Text"
  isOptional <- obj .: "IsOptional"

  pure $ NodeType_Text
    { "IsItalics":  isItalics
    , "IsAccent":   isAccent
    , "Text":       text
    , "IsOptional": isOptional
    }

decodeNodeType_Example :: Json -> Either JsonDecodeError NodeType_Example
decodeNodeType_Example = Decoders.decodeArray decodeNodeType_Text >>> map NodeType_Example

decodeNodeType_ExampleItem :: Json -> Either JsonDecodeError NodeType_ExampleItem
decodeNodeType_ExampleItem = decodeNodeType_Example >>> map NodeType_ExampleItem

decodeNodeType_ListItem :: Json -> Either JsonDecodeError NodeType_ListItem
decodeNodeType_ListItem = Decoders.decodeArray decodeNodeType >>> map NodeType_ListItem

decodeNodeType :: Json -> Either JsonDecodeError NodeType
decodeNodeType = decodeJson >=> \obj -> obj .: "Node" >>= case _ of
  "Comment"       -> undefined
  "Paragraph"     -> undefined
  "Text"          -> undefined
  "List"          -> undefined
  "Examples"      -> undefined
  "CardRefs"      -> undefined
  "CardRefItem"   -> undefined
  "CardRef"       -> undefined
  "Transcription" -> undefined
  "Abbrev"        -> undefined
  "Caption"       -> undefined
  "Sound"         -> undefined
  "Ref"           -> undefined
  "Unsupported"   -> undefined
  other           -> Left $ JsonDecodeError

decodeArticleNode :: Json -> Either JsonDecodeError ArticleNode
decodeArticleNode = decodeNodeType >>> map ArticleNode

decodeArticleModel :: Json -> Either JsonDecodeError ArticleModel
decodeArticleModel = decodeJson >=> \(obj :: Object Json) -> do
  title       <- obj .: "Title"
  titleMarkup <- obj .: "TitleMarkup"
    >>= traverse decodeArticleNode
  dictionary  <- obj .: "Dictionary"
  articleId   <- obj .: "ArticleId"
  body        <- obj .: "Body"
    >>= traverse decodeArticleNode
  pure $ ArticleModel
    { "Title":       title
    , "TitleMarkup": titleMarkup
    , "Dictionary":  dictionary
    , "ArticleId":   articleId
    , "Body":        body
    }
