module Main where
import Graphics.UI.WX

main :: IO ()
main = start hello

hello :: IO ()
hello = 
  do f    <- frame    [text := "Hello Evren!"]
     quit <- button f [text := "Quit", on command := close f]
     set f [layout := margin 10 (column 5 [floatCentre (label "Hello")
                                          ,floatCentre (widget quit)
                                          ] )]
