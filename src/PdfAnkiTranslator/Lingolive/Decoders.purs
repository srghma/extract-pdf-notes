module PdfAnkiTranslator.Lingolive.Decoders where

import Data.Argonaut.Decode
import PdfAnkiTranslator.Lingolive.DecodeUtils
import PdfAnkiTranslator.Lingolive.Types
import Protolude

import Affjax as Affjax
import Control.Monad.Except (withExceptT)
import Control.Monad.ST (ST)
import Control.Monad.ST as Control.Monad.ST
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode.Decoders as Decoders
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Array as Array
import Data.Generic.Rep.Show (genericShow)
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STObject

decodeNodeType_Text :: forall r . STObject r Json -> ExceptT JsonDecodeError (ST r) NodeType_Text
decodeNodeType_Text stobj = do
  isItalics  <- stobj `getFieldAndPop` "IsItalics"
  isAccent   <- stobj `getFieldAndPop` "IsAccent"
  text       <- stobj `getFieldAndPop` "Text"
  isOptional <- stobj `getFieldAndPop` "IsOptional"

  pure $ NodeType_Text
    { "IsItalics":  isItalics
    , "IsAccent":   isAccent
    , "Text":       text
    , "IsOptional": isOptional
    }

decodeNodeType_Example :: forall r . STObject r Json -> ExceptT JsonDecodeError (ST r) NodeType_Example
-- | decodeNodeType_Example stobj = Decoders.decodeArray decodeNodeType_Text >>> map NodeType_Example
decodeNodeType_Example stobj = except $ Left MissingValue

decodeNodeType_ExampleItem :: forall r . STObject r Json -> ExceptT JsonDecodeError (ST r) NodeType_Example
-- | decodeNodeType_ExampleItem = decodeNodeType_Example >>> map NodeType_ExampleItem
decodeNodeType_ExampleItem stobj = except $ Left MissingValue

decodeNodeType_ListItem :: Json -> ExceptT JsonDecodeError NodeType_ListItem
-- | decodeNodeType_ListItem = Decoders.decodeArray decodeNodeType >>> map NodeType_ListItem
decodeNodeType_ListItem stobj = except $ Left MissingValue

decodeNodeType :: Json -> Either JsonDecodeError NodeType
decodeNodeType = decodeJson >=> decodeObjectAndUseUpAllFields \stobj ->
  stobj `getFieldAndPop` "Node" >>=
    case _ of
      "Comment" -> withExceptT (Named "NodeType__Comment") do
        (text :: String) <- stobj `getFieldAndPop` "Text"
        (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
        markup <- getFieldAndPop' (Decoders.decodeArray decodeNodeType) stobj "Node"
        pure $ NodeType__Comment
          { "Text": text
          , "IsOptional": isOptional
          , "Markup": markup
          }
      "Paragraph" -> withExceptT (Named "NodeType__Paragraph") do
        (text :: Unit) <- getFieldAndPop' decodeNull stobj "Text"
        (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
        markup <- getFieldAndPop' (Decoders.decodeArray decodeNodeType_ListItem) stobj "Markup"
        pure $ NodeType__Paragraph
          { "IsOptional": isOptional
          , "Markup": markup
          }
      "Text" -> withExceptT (Named "NodeType__Text") $ decodeNodeType_Text stobj <#> NodeType__Text
      "List" -> withExceptT (Named "NodeType__List") do
         (_type :: Int) <- getFieldAndPop' Decoders.decodeInt stobj "Type"
         (items :: Int) <- getFieldAndPop' (Decoders.decodeArray decodeNodeType) stobj "Items"
         (text :: Unit) <- getFieldAndPop' decodeNull stobj "Text"
         (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
         markup <- getFieldAndPop' (Decoders.decodeArray decodeNodeType) stobj "Markup"
         pure $ NodeType__Paragraph
           { "IsOptional": isOptional
           , "Markup": markup
           }
      "Examples" -> withExceptT (Named "NodeType__Examples") $ except $ Left MissingValue
      "CardRefs" -> withExceptT (Named "NodeType__CardRefs") $ except $ Left MissingValue
      "CardRefItem" -> withExceptT (Named "NodeType__CardRefItem") $ except $ Left MissingValue
      "CardRef" -> withExceptT (Named "NodeType__CardRef") do
        dictionary <- stobj `getFieldAndPop` "Dictionary"
        articleId <- stobj `getFieldAndPop` "ArticleId"
        (text :: String) <- stobj `getFieldAndPop` "Text"
        (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
        pure $ NodeType__CardRef
          { "Dictionary": dictionary
          , "ArticleId": articleId
          , "Text": text
          , "IsOptional": isOptional
          }
      "Transcription" -> withExceptT (Named "NodeType__Transcription") $ except $ Left MissingValue
      "Abbrev" -> withExceptT (Named "NodeType__Abbrev") do
        (fullText :: String) <- stobj `getFieldAndPop` "FullText"
        (text :: String) <- stobj `getFieldAndPop` "Text"
        (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
        pure $ NodeType__Abbrev
          { "FullText": fullText
          , "Text": text
          , "IsOptional": isOptional
          }
      "Caption" -> withExceptT (Named "NodeType__Caption") $ except $ Left MissingValue
      "Sound" -> withExceptT (Named "NodeType__Sound") do
         (text :: Unit) <- getFieldAndPop' decodeNull stobj "Text"
         (isOptional :: Boolean) <- stobj `getFieldAndPop` "IsOptional"
         (fileName :: String) <- stobj `getFieldAndPop` "FileName"
         pure $ NodeType__Sound
           { "IsOptional": isOptional
           , "FileName": fileName
           }
      "Ref" -> withExceptT (Named "NodeType__Ref") $ except $ Left MissingValue
      "Unsupported" -> withExceptT (Named "NodeType__Unsupported") $ except $ Left MissingValue
      other -> except $ Left $ AtKey "Node" $ UnexpectedValue $ Json.fromString other

decodeArticleNode :: Json -> Either JsonDecodeError ArticleNode
decodeArticleNode = decodeNodeType >>> map ArticleNode

decodeArticleModel :: Json -> Either JsonDecodeError ArticleModel
decodeArticleModel = decodeJson >=> \(obj :: Object Json) -> do
  title       <- obj .: "Title"
  titleMarkup <- Decoders.getField (Decoders.decodeArray decodeArticleNode) obj "TitleMarkup"
  dictionary  <- obj .: "Dictionary"
  articleId   <- obj .: "ArticleId"
  body        <- Decoders.getField (Decoders.decodeArray decodeArticleNode) obj "Body"
  pure $ ArticleModel
    { "Title":       title
    , "TitleMarkup": titleMarkup
    , "Dictionary":  dictionary
    , "ArticleId":   articleId
    , "Body":        body
    }

decodeArticleModels :: Json -> Either JsonDecodeError (Array ArticleModel)
decodeArticleModels = Decoders.decodeArray decodeArticleModel
