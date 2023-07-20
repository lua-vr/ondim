{- | Just a tiny wrapper around html-conduit that fixes some unintuitive choices
for how the root element is defined in documents without root (as is the case
of most templates).
-}
module Ondim.Targets.HTML.Parser (parseT, parseLBS) where

import Conduit (ConduitT, MonadThrow, awaitForever, mapOutput, runConduit, yield, (.|))
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit.List qualified as CL
import Data.XML.Types qualified as XT
import Text.HTML.DOM qualified as HTML
import Text.XML qualified as X

parseT :: Text -> X.Document
parseT tss =
  case runConduit $ yield tss .| sinkDocText of
    Left e -> error $ "Unexpected exception in parseSTChunks: " <> show e
    Right x -> x

parseLBS :: LByteString -> X.Document
parseLBS = parseBSChunks . LBS.toChunks

parseBSChunks :: [ByteString] -> X.Document
parseBSChunks tss =
  case runConduit $ CL.sourceList tss .| sinkDoc of
    Left e -> error $ "Unexpected exception in parseBSChunks: " <> show e
    Right x -> x

sinkDoc :: MonadThrow m => ConduitT ByteString o m X.Document
sinkDoc = sinkDoc' HTML.eventConduit

sinkDocText :: MonadThrow m => ConduitT Text o m X.Document
sinkDocText = sinkDoc' HTML.eventConduitText

sinkDoc' ::
  MonadThrow m =>
  ConduitT a XT.Event m () ->
  ConduitT a o m X.Document
sinkDoc' f =
  fmap stripDummy $ mapOutput (Nothing,) f .| addDummyWrapper .| X.fromEvents
  where
    addDummyWrapper = do
      yield (Nothing, XT.EventBeginElement "html" [])
      awaitForever yield
      yield (Nothing, XT.EventEndElement "html")

    stripDummy doc@(X.Document pro (X.Element _ _ nodes) epi) =
      case nodes of
        [X.NodeElement root@X.Element {X.elementName = "html"}] -> X.Document pro root epi
        _ -> doc
