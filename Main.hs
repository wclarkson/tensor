import Tensor
import SVGWriter

testDoc :: SVG
testDoc = SVG 400 400 [
  Rect 10 10 390 390,
  Line 50 50 50 200,
  Line 50 50 350 350 ]

main :: IO ()
main = putStrLn (writeSVG testDoc)
