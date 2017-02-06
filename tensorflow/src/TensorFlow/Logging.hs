-- | TensorBoard Summary generation. Provides type safe wrappers
-- around raw string emitting CoreOps.
module TensorFlow.Logging
    ( histogramSummary
    , scalarSummary
    , build
    , logSummary
    , newEventsWriter
    , EventsWriter
    , SummaryTensor
    ) where

import Control.Arrow (second)
import Data.Default (def)
import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Lens.Family2 ((.~), (&))
import Proto.ThirdParty.Tensorflow.Core.Framework.Summary (Summary)
import Proto.ThirdParty.Tensorflow.Core.Util.Event (step, summary, wallTime)
import TensorFlow.Build (Build, SummaryTensor, addSummary)
import TensorFlow.Internal.EventsFFI (EventsWriter, logEvent, newEventsWriter)
import TensorFlow.Ops (vector)
import TensorFlow.Session (Session, buildWithSummary)
import qualified TensorFlow.GenOps.Core as CoreOps

-- | Adds a 'CoreOps.histogramSummary' node. The tag argument is
-- intentionally limited to a single value for simplicity.
histogramSummary tag = addSummary . CoreOps.histogramSummary (vector [tag])

-- | Adds a 'CoreOps.scalarSummary' node.
scalarSummary tag = addSummary . CoreOps.scalarSummary (vector [tag])

-- | Builds the graph within a Session and collects all the logging
-- summary operations into the second value in the result pair. All
-- the summary operations are merged into a single value.
build :: Build a -> Session (a, SummaryTensor)
build b = second CoreOps.mergeSummary <$> buildWithSummary b

-- | Logs the given Summary event with an optional global step (use 0
-- if not applicable).
logSummary :: EventsWriter -> Int64 -> Summary -> IO ()
logSummary ew step' summaryProto = do
    t <- doubleWallTime
    logEvent ew (def & wallTime .~ t
                     & step .~ step'
                     & summary .~ summaryProto
                )

-- Number of seconds since epoch.
doubleWallTime :: IO Double
doubleWallTime = asDouble <$> getCurrentTime
    where asDouble t = fromRational (toRational (utcTimeToPOSIXSeconds t))
