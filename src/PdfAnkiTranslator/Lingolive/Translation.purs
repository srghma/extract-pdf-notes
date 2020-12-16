module PdfAnkiTranslator.Lingolive.Translation where

import Affjax as Affjax

type Config =
  { token :: Sting
  , serviceUrl :: Sting
  }

newtype NoteType_Text = NoteType_Text
  { "IsItalics" :: Boolean
  , "IsAccent" :: Boolean
  , "Text" :: String
  , "IsOptional" :: Boolean
  }

newtype NoteType_Example = NoteType_Example (Array NoteType_Text)

newtype NoteType_ExampleItem = NoteType_ExampleItem NoteType_Example

newtype NoteType_ListItem = NoteType_ListItem (Array NodeType)
  -- "Node" :: NodeType
  -- "Text" :: Maybe String -- null
  -- | { "IsOptional" :: Boolean -- false
  -- | { "Markup" ::
  -- | }

data NodeType
  = NodeType__Comment       -- 0
    -- | { "Text" :: String
    -- | , "IsOptional" :: Boolean
    { "Markup" :: Array NodeType
    }
  | NodeType__Paragraph     -- 1
    -- | , "Text" :: Maybe String
    { "Markup" :: Array NodeType
    , "IsOptional" :: Boolean
    }
  | NodeType__Text NodeType_Text         -- 2
  | NodeType__List          -- 3
    { "Type" :: Int -- this is NOT length
    , "Items" :: Array NoteType__ListItem
    }
  -- | | NoteType__ListItem      -- 4
  -- |   {
  -- |   }
  | NoteType__Examples (Array NoteType_ExampleItem)      -- 5
    -- | "Text": null,
    -- | "IsOptional": true
  -- | | NoteType__ExampleItem   -- 6
  -- |   {
  -- |   }
  -- | | NoteType__Example       -- 7
  -- |   {
  -- |   }
  | NoteType__CardRefs      -- 8
    -- | {
    -- | }
  | NoteType__CardRefItem   -- 9
    -- | {
    -- | }
  | NoteType__CardRef       -- 10
    { "Dictionary" :: String
    , "ArticleId" :: String
    }
  | NoteType__Transcription -- 11
    -- | {
    -- | }
  | NoteType__Abbrev        -- 12
    { "FullText" :: String
    }
  | NoteType__Caption       -- 13
    -- | {
    -- | }
  | NoteType__Sound         -- 14
    { "FileName" :: String
    }
  | NoteType__Ref           -- 15
    -- | {
    -- | }
  | NoteType__Unsupported   -- 16
    -- | {
    -- | }

newtype ArticleNode = ArticleNode NodeType

type ArticleModel =
  { "Title"       :: String
  , "TitleMarkup" :: Array ArticleNode
  , "Dictionary"  :: String
  , "ArticleId"   :: String
  , "Body"        :: Array ArticleNode
  }

data Response
  = Response__Success (Array ArticleModel)
  | Response__NoDictionaries
  | Response__NoTranslationForWord

translation :: Config -> String -> Aff (Array ArticleModel)
translation config word = undefined
