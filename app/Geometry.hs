module Geometry where

import Data.List

magnitude :: (Floating a) => [a] -> a
magnitude xs = sqrt $ foldl' (+) 0.0 $ map (\x -> x*x) xs

magnitudeD3 :: (Floating a) => (a,a,a) -> a
magnitudeD3 (x,y,z) = sqrt $ x*x + y*y + z*z

euclidean :: (Floating a) => [a] -> [a] -> a
euclidean x y = magnitude $ zipWith (-) x y

euclideanD3 :: (Floating a) => (a,a,a) -> (a,a,a) -> a
euclideanD3 x y = magnitudeD3 $ diffD3 x y

sumD3, diffD3, crossD3 :: (Floating a) => (a,a,a) -> (a,a,a) -> (a,a,a)
sumD3 (x1,y1,z1) (x2,y2,z2) = (x2+x1,y2+y1,z2+z1)
diffD3 (x1,y1,z1) (x2,y2,z2) = (x2-x1,y2-y1,z2-z1)
crossD3 (x1,y1,z1) (x2,y2,z2) = (y1*z2-y2*z1, x1*z2-x2*z1, x1*y2-x2*y1)

dotD3 :: (Floating a) => (a,a,a) -> (a,a,a) -> a
dotD3 (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

angleD3  :: (Floating a) => (a,a,a) -> (a,a,a) -> a
angleD3 v1 v2 = (dotD3 v1 v2)/((magnitudeD3 v1) * (magnitudeD3 v2))

normalize :: (Floating a, Eq a) => ([a] -> a) -> [a] -> [a]
normalize f xs = let len = length xs;
                     norm = f xs;
                     val = 1 / fromIntegral len; in
  if norm == 0.0
  then replicate len val
  else map (/ norm) xs

normalizeMag :: (Floating a, Eq a) => [a] -> [a]
normalizeMag = normalize magnitude

normalizeSum :: (Floating a, Eq a) => [a] -> [a]
normalizeSum = normalize sum