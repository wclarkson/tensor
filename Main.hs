import qualified TensorField as TF
import Constraint (Constraint)
import SVGWriter
import JSONParser
import Control.Applicative
import Data.Aeson
import qualified Streamline as SL
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy as B

fieldWidth :: Float
fieldWidth = 20
fieldHeight :: Float
fieldHeight = 20

makeSVG :: [Constraint] -> IO ()
makeSVG cs =
    let tf = TF.makeTensorField cs
    in putStrLn $ writeSVG (appendElements
                           (TF.plotTensorField tf cs fieldWidth fieldHeight)
                           (SL.traceLines tf fieldWidth fieldHeight))

main :: IO ()
main = getArgs >>= parse

parse ["-h"]              = usage                               >> exit
parse ["-v"]              = version                             >> exit
parse [file, fW, fH]      = (buildViz file (read fW) (read fH)) >> exit
parse [file, fW, fH, opt] = (buildViz file (read fW) (read fH)) >> exit
parse _                   = usage                               >> exit

usage   = putStrLn "usage: tensor <inputFile> <fieldWidth> <fieldHeight> [opt]"
version = putStrLn "Tensor v0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

buildViz :: FilePath -> Float -> Float -> IO ()
buildViz inputFile fW fH = do
    d <- (eitherDecode <$> (B.readFile inputFile) :: IO (Either String [Input]))
    case d of
      Left err    -> putStrLn $ "Failure on input: " ++ err
      Right input -> makeSVG $ map inputToConstraint input
