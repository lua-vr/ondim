{-# LANGUAGE RankNTypes #-}

module Ondim.Extra.Exceptions
  ( tryAttrFilter,
    exceptionExp,
    prettyException,
  ) where

import Data.Text qualified as T
import Ondim
import Ondim.Extra.Expansions (listExp)

tryAttrFilter :: Monad m => Filter m Attribute
tryAttrFilter (k, _) x
  | Just k' <- "?" `T.stripSuffix` k =
      first (const k') <<$>> x `catchFailure` \_ _ _ _ -> return []
  | otherwise = x

exceptionExp :: Monad m => OndimException -> ExpansionMap m
exceptionExp exc@(OndimException e t) = do
  "pretty" #@ prettyException exc
  "stack" #. listExp stackExp (expansionTrace t)
  "depth" #@ show $ depth t
  "site" #. locExp $ currentSite t
  case e of
    MaxExpansionDepthExceeded -> "type" #@ "max-depth"
    TemplateError cs msg -> do
      "type" #@ "template-error"
      "stacktrace" #@ toText $ prettyCallStack cs
      "message" #@ msg
    ExpansionFailure trep name f -> do
      "type" #@ "failure"
      "caller" #. do
        "type" #@ show trep
        "name" #@ name
      "failure" #. case f of
        ExpansionNotBound -> "type" #@ "not-bound"
        ExpansionWrongType trep2 -> do
          "type" #@ "wrong-type"
          "bound-type" #@ show trep2
        ExpansionNoFromText -> "type" #@ "missing-fromtext"
        ExpansionFailureOther msg -> do
          "type" #@ "other"
          "message" #@ msg
  where
    locExp = \case
      NoDefinition -> "type" #@ "none"
      FileDefinition f -> do
        "type" #@ "file"
        "filepath" #@ toText f
      CodeDefinition c -> do
        "type" #@ "code"
        "location" #@ toText $ prettySrcLoc c
    stackExp (name, loc) = namespace do
      "name" #@ name
      "site" #. locExp loc


prettyException :: OndimException -> Text
prettyException (OndimException e t) =
  eMsg <> "\n\n" <> "While expanding " <> loc <> "\n" <> "Expansion stack:\n" <> eStack
  where
    loc = loc' (currentSite t)
    loc' = \case
      NoDefinition -> "undefined location"
      FileDefinition fp -> "file " <> show fp
      CodeDefinition c -> "code location " <> toText (prettySrcLoc c)
    eStack = T.unlines $ expansionTrace t <&> \(name, l) ->
      "'" <> name <> "' from " <> loc' l
    eMsg = case e of
      MaxExpansionDepthExceeded ->
        "Maximum expansion depth exceeded. Did you write something recursive?\n"
      TemplateError cs msg -> do
        msg <> "\n\n" <> "Template error! " <> toText (prettyCallStack cs)
      ExpansionFailure trep name f ->
        case f of
          ExpansionNotBound ->
            "Identifier '" <> name <> "' (of type " <> show trep <> ") is not bound!"
          ExpansionWrongType trep2 ->
            "Identifier '"
              <> name
              <> "' is bound to an expansion of type "
              <> show trep2
              <> " instead of "
              <> show trep
              <> "."
          ExpansionNoFromText ->
            "Identifier '"
              <> name
              <> "' is bound to text data, but type "
              <> show trep
              <> " is missing a 'fromText' instance."
          ExpansionFailureOther msg ->
            msg
              <> "\n\n(While calling identifier '"
              <> name
              <> "' from type "
              <> show trep
              <> ")"
