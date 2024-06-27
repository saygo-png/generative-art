module Main where
import Graphics.Gloss

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- | Calculates the midpoint of two points.
midpoint :: Vector -> Vector -> Vector
midpoint (vector1:vector2) = 

addVectors :: Vector -> Vector -> Vector
addVectors vectorA vectorB = vectorA + vectorB
