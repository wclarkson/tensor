import qualified TensorField as TF
import Constraint (Constraint)
import SVGWriter
import JSONParser
import Control.Applicative
import Data.Aeson
import Streamline (PlacementMethod (Furthest, Random, Improved))
import qualified Streamline as SL
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy as B

makeSVG :: [Constraint] -> Float -> Float -> PlacementMethod -> IO ()
makeSVG cs fw fh method =
    let tf = TF.makeTensorField cs
    in putStrLn $ writeSVG (appendElements
                           (TF.plotTensorField tf cs fw fh)
                           (SL.traceLines tf fw fh method))

main :: IO ()
main = getArgs >>= parseCmdArgs

def :: String
def = "furthest"

parseCmdArgs ["-h"]            = usage                                 >> exit
parseCmdArgs ["-v"]            = version                               >> exit
parseCmdArgs [file, fw, fh]    = buildViz file (read fw) (read fh) def >> exit
parseCmdArgs [file, fw, fh, o] = buildViz file (read fw) (read fh) o   >> exit
parseCmdArgs _                 = usage                                 >> exit

usage   = putStrLn "usage: tensor <inputFile> <fieldWidth> <fieldHeight> [opt]"
version = putStrLn "Tensor v0.1"
exit    = exitSuccess
die     = exitWith (ExitFailure 1)

buildViz :: FilePath -> Float -> Float -> String -> IO ()
buildViz inputFile fw fh pmeth = do
    d <- (eitherDecode <$> (B.readFile inputFile) :: IO (Either String [Input]))
    case d of
      Left err    -> putStrLn $ "Failure on input: " ++ err
      Right input -> let method = case pmeth of
                                    "furthest" -> Furthest
                                    "random"   -> Random
                                    "improved" -> Improved
                                    _          -> error "Seed placement method not found."
                     in makeSVG (map inputToConstraint input) fw fh method


