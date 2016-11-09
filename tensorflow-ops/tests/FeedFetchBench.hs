-- Disable full-laziness to keep ghc from optimizing most of the benchmark away.
{-# OPTIONS_GHC -fno-full-laziness #-}
import Control.Exception (evaluate)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Criterion.Main (defaultMain, bgroup, bench, nfIO)
import Criterion.Types (Benchmarkable(..))
import qualified Data.Vector as V
import qualified TensorFlow.Build as TF
import qualified TensorFlow.ControlFlow as TF
import qualified TensorFlow.Gradient as TF
import qualified TensorFlow.Nodes as TF
import qualified TensorFlow.Ops as TF
import qualified TensorFlow.Session as TF
import qualified TensorFlow.Tensor as TF
import qualified TensorFlow.Types as TF

-- | Create 'Benchmarkable' for 'TF.Session'.
--
-- The entire benchmark will be run in a single tensorflow session. The
-- 'TF.Session' argument will be run once and then its result will be run N
-- times.
whnfSession :: TF.Session (a -> TF.Session b) -> a -> Benchmarkable
whnfSession init x = Benchmarkable $ \m -> TF.runSession $ do
    f <- init
    -- Can't use replicateM because n is Int64.
    let go n | n <= 0    = return ()
             | otherwise = f x >>= liftIO . evaluate >> go (n-1)
    go m

-- | Benchmark feeding and fetching a vector.
feedFetchBenchmark :: TF.Session (V.Vector Float -> TF.Session (V.Vector Float))
feedFetchBenchmark = do
    input <- TF.build (TF.placeholder (TF.Shape [-1]))
    output <- TF.build (TF.render (TF.identity input))
    return $ \v -> do
        let shape = TF.Shape [fromIntegral (V.length v)]
            inputData = TF.encodeTensorData shape v
            feeds = [TF.feed input inputData]
        TF.runWithFeeds feeds output

main :: IO ()
main = defaultMain
    [ bgroup "feedFetch"
        [ bench "4 byte" $ whnfSession feedFetchBenchmark (V.replicate 1 0)
        , bench "4 KiB" $ whnfSession feedFetchBenchmark (V.replicate 1024 0)
        , bench "4 MiB" $ whnfSession feedFetchBenchmark (V.replicate (1024^2) 0)
        ]
    ]
