module Main where

import Criterion.Main
import Data.Function
import Data.Text (Text)
import qualified Data.Text.Encoding as D
import qualified Data.Text.Lazy as C
import qualified Data.Text.Lazy.Builder as B
import qualified TextBuilderDev as A
import Prelude

main :: IO ()
main =
  defaultMain byAlgorithm

byAlgorithm :: [Benchmark]
byAlgorithm =
  [ bgroup "Left-biased mappend" $
      bySize (foldl' (<>) mempty),
    bgroup "Right-biased mappend" $
      bySize (foldl' (flip (<>)) mempty),
    bgroup "mconcat" $
      bySize mconcat
  ]

bySize :: (forall a. (Monoid a) => [a] -> a) -> [Benchmark]
bySize concat =
  [ bgroup "100B" . bySubject concat $
      replicate 10 "фывапролдж",
    bgroup "1kB" . bySubject concat $
      replicate 100 "фывапролдж",
    bgroup "1MB" . bySubject concat $
      replicate 100_000 "фывапролдж"
  ]

bySubject :: (forall a. (Monoid a) => [a] -> a) -> [Text] -> [Benchmark]
bySubject concat texts =
  [ bench
      "TextBuilderDev.TextBuilder"
      ( whnf
          (A.toText . concat)
          (fmap A.text texts)
      ),
    bench
      "Data.Text.Encoding.StrictTextBuilder"
      ( whnf
          (D.strictBuilderToText . concat)
          (fmap D.textToStrictBuilder texts)
      ),
    bench
      "Data.Text.Lazy.Builder.Builder"
      ( whnf
          (C.toStrict . B.toLazyText . concat)
          (fmap B.fromText texts)
      ),
    bench
      "Data.Text.Text"
      ( whnf
          concat
          texts
      ),
    bench
      "Data.Text.Lazy.Text"
      ( whnf
          (C.toStrict . concat)
          (fmap C.fromStrict texts)
      )
  ]
