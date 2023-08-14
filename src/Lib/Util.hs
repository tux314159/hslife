module Lib.Util
  ( fromBool,
    genIndices,
    toBool,
  )
where

-- | Generate 2d list with (col-major!) indices
genIndices :: Integral a => a -> a -> [[(a, a)]]
genIndices x y = zipWith zip (replicate (fromIntegral y) [0 .. x - 1]) (replicate (fromIntegral x) <$> [0 .. y - 1])

-- | What do you think
fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

-- | What do you think
toBool :: Int -> Bool
toBool 0 = False
toBool _ = True
