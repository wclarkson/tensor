import qualified TensorField as TF
import Constraint (Constraint)
import SVGWriter
import JSONParser
import Control.Applicative
import Data.Aeson
import qualified Streamline as SL

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
main = do
    d <- (eitherDecode <$> contentsOfArgv1) :: IO (Either String [Input])
    case d of
      Left err    -> putStrLn $ "Failure on input: " ++ err
      Right input -> makeSVG $ map inputToConstraint input
