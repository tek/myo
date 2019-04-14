module Myo.Output.ParseReport where

import Data.Vector ((!?))
import Ribosome.Api.Buffer (bufferForFile, edit)
import Ribosome.Api.Window (currentLine, setCursor)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (nvimWinSetBuf, vimCommand, vimSetCurrentWindow, windowIsValid)

import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(Internal, NoLocation))
import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))

eventByLine :: ParseReport -> Int -> Maybe OutputEvent
eventByLine (ParseReport events lines) line = do
  (ReportLine eventIndex _) <- lines !? line
  events !? eventIndex

findWindow :: m Window
findWindow =
  undefined

selectEvent ::
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  Window ->
  OutputEvent ->
  m ()
selectEvent previousWindow (OutputEvent (Just (Location path line col)) _) = do
  previousExists <- windowIsValid previousWindow
  window <- if previousExists then return previousWindow else findWindow
  vimSetCurrentWindow window
  existingBuffer <- bufferForFile (toText path)
  maybe (edit path) (nvimWinSetBuf window) existingBuffer
  setCursor window line (fromMaybe 0 col)
  vimCommand "normal! zv"
selectEvent _ _ =
  throwHoist OutputError.NoLocation

selectCurrentLineEventFrom ::
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  ParseReport ->
  Window ->
  m ()
selectCurrentLineEventFrom report window =
  maybe err (selectEvent window) =<< eventByLine report <$> currentLine
  where
    err = throwHoist (OutputError.Internal "cursor line has no data")
