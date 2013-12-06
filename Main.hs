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

makeSVG :: [Constraint] -> Float -> Float -> IO ()
makeSVG cs fw fh =
    let tf = TF.makeTensorField cs
    in putStrLn $ writeSVG (appendElements
                           (TF.plotTensorField tf cs fw fh)
                           (SL.traceLines tf fw fh))

main :: IO ()
main = getArgs >>= parseCmdArgs

parseCmdArgs ["-h"]              = usage                               >> exit
parseCmdArgs ["-v"]              = version                             >> exit
parseCmdArgs [file, fw, fh]      = (buildViz file (read fw) (read fh)) >> exit
parseCmdArgs [file, fw, fh, opt] = (buildViz file (read fw) (read fh)) >> exit
parseCmdArgs _                   = usage                               >> exit

usage   = putStrLn "usage: tensor <inputFile> <fieldWidth> <fieldHeight> [opt]"
version = putStrLn "Tensor v0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

buildViz :: FilePath -> Float -> Float -> IO ()
buildViz inputFile fw fh = do
    d <- (eitherDecode <$> (B.readFile inputFile) :: IO (Either String [Input]))
    case d of
      Left err    -> putStrLn $ "Failure on input: " ++ err
      Right input -> makeSVG (map inputToConstraint input) fw fh
