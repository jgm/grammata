module Grammata.Chart (Chart(..), toSVG, writeEPS, parseChart) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import qualified Data.ByteString.Lazy as BL
import Grammata.Types hiding (render)
import Language.Haskell.Interpreter -- hint

newtype Chart = Chart {unChart :: EC (Layout Double Double) ()}

instance ToRenderable Chart where
  toRenderable (Chart x) = toRenderable x

-- instance ToArg Chart where
--   toArg s = Right ("Chart do{ " ++ s ++ "}")

parseChart :: String -> Doc IO (Either InterpreterError Chart)
parseChart s =
  lift $ runInterpreter $ do
    loadModules ["Grammata.Chart"]
    setImports ["Prelude", "Grammata.Types", "Grammata.Chart", "Graphics.Rendering.Chart.Easy"]
    interpret s (as :: Chart)

mychart :: Chart
mychart = Chart $ do
    let signal xs = [ (x,(sin (x*3.14159/45) + 1) /
                           2 * (sin (x*3.14159/5))) | x <- xs ]
    layout_title .= "Amplitude Modulation"
    plot (line "am" [signal [0,(0.5)..400]])
    plot (points "am points" (signal [0,7..400]))

toSVG :: (Double, Double) -> Chart -> IO BL.ByteString
toSVG (x, y) chart = fst <$> renderableToSVGString (toRenderable chart) x y

writeEPS :: Chart -> (Double, Double) -> FilePath -> IO (PickFn ())
writeEPS chart (x,y) filepath = do
  let renderedchart :: BackendProgram (PickFn ())
      renderedchart = render (toRenderable chart) (x,y)
  env <- defaultEnv vectorAlignmentFns x y
  cBackendToEPSFile renderedchart env filepath
