{-# LANGUAGE RankNTypes #-}

module Ondim.Extra.Exceptions
  ( tryExp,
    tryAttrFilter,
    exceptionExp,
    prettyException,
  ) where

import Data.Text qualified as T
import Ondim
import Ondim.Extra.Expansions (listExp)

tryExp :: Monad m => GlobalExpansion m
tryExp node = liftChildren node `catchFailure` \_ _ _ _ -> return []

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
    Failure trep name f -> do
      "type" #@ "failure"
      "caller" #. do
        "type" #@ show trep
        "name" #@ name
      "failure" #. case f of
        NotBound -> "type" #@ "not-bound"
        ExpansionWrongType trep2 -> do
          "type" #@ "expansion-wrong-type"
          "bound-type" #@ show trep2
        TemplateWrongType trep2 -> do
          "type" #@ "template-wrong-type"
          "bound-type" #@ show trep2
        FailureOther msg -> do
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
      Failure trep name f ->
        case f of
          NotBound ->
            "Identifier '" <> name <> "' (of type " <> show trep <> ") is not bound!"
          ExpansionWrongType trep2 ->
            "Identifier '"
              <> name
              <> "' is bound to an expansion of type "
              <> show trep2
              <> " instead of "
              <> show trep
              <> "."
          TemplateWrongType trep2 ->
            "Identifier '"
              <> name
              <> "' is bound to an expansion of type "
              <> show trep2
              <> " instead of "
              <> show trep
              <> ", and no conversion is declared."
          FailureOther msg ->
            msg
              <> "\n\n(While calling identifier '"
              <> name
              <> "' from type "
              <> show trep
              <> ")"
