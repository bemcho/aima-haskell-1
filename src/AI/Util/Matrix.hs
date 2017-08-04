{-# LANGUAGE FlexibleContexts #-}

module AI.Util.Matrix where

import Control.Monad.Random hiding (fromList)
import Foreign.Storable (Storable)
import Numeric.LinearAlgebra

-- |Concatenate matrices horizontally.
horzcat :: Element a => [Matrix a] -> Matrix a
horzcat = fromBlocks . return

-- |Concatenate matrices vertically.
vertcat :: Element a => [Matrix a] -> Matrix a
vertcat = fromBlocks . map return

-- |Add a column of ones to a matrix.
addOnes :: Matrix Double -> Matrix Double
addOnes x = fromBlocks [[1, x]]

--------------------------
-- Functions on Vectors --
--------------------------

constant :: Storable a => a -> Int -> Vector a
constant a n = fromList (replicate n a)

takeVector :: Storable a => Int -> Vector a -> Vector a
takeVector n v = subVector 0 n v

dropVector :: (Storable a, Container Vector a) => Int -> Vector a -> Vector a
dropVector n v = subVector n (size v - n) v

foldVector :: Storable b => (a -> b -> a) -> a -> Vector b -> a
foldVector f acc vec = foldl f acc (toList vec)

sumVector :: (Num a, Storable a) => Vector a -> a
sumVector xs = foldVector (+) 0 xs

prodVector :: (Num a, Storable a) => Vector a -> a
prodVector xs = foldVector (*) 1 xs

-- TODO: check if this can be made more efficient
mapVector :: (Storable a, Storable b) => (a -> b) -> Vector a -> Vector b
mapVector f = fromList . map f . toList

---------------------------
-- Functions on Matrices --
---------------------------

mapRows :: Element a => (Vector a -> b) -> Matrix a -> [b]
mapRows f m = map f (toRows m)

mapCols :: Element a => (Vector a -> b) -> Matrix a -> [b]
mapCols f m = map f (toColumns m)

eachRow :: (Element a, Element b) => (Vector a -> Vector b) -> Matrix a -> Matrix b
eachRow f = fromRows . mapRows f

eachCol :: (Element a, Element b) => (Vector a -> Vector b) -> Matrix a -> Matrix b
eachCol f = fromColumns . mapCols f

sumRows :: (Element a, Num (Vector a)) => Matrix a -> Vector a
sumRows m = sum $ toRows m

sumCols :: (Element a, Num (Vector a)) => Matrix a -> Vector a
sumCols m = sum $ toColumns m

sumMatrix :: Matrix Double -> Double
sumMatrix = sumVector . sum . toRows

------------------------
-- Subset Referencing --
------------------------

subRefVec :: (Storable a, Container Vector a) => Vector a -> [Int] -> Vector a
subRefVec v is = fromList $ map (v `atIndex`) is

subRefRows :: Element a => Matrix a -> [Int] -> Matrix a
subRefRows m is = fromRows $ map (r!!) is where r = toRows m

subRefCols :: Element a => Matrix a -> [Int] -> Matrix a
subRefCols m is = fromColumns $ map (c!!) is where c = toColumns m

