module Main where

data Color = Red | Green | Blue

type MyColor = Color

newtype ColorString = ColorString String

printColorString :: ColorString -> IO ()
printColorString (ColorString colorString) = putStrLn colorString

printColor :: MyColor -> IO ()
printColor Red = putStrLn "Red"
printColor Green = putStrLn "Green"
printColor Blue = putStrLn "Blue"

main :: IO ()
main = do
    printColor Green
    printColorString (ColorString "violet")





