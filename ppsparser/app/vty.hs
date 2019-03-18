
import Potato.PuzzleScript
import Potato.PuzzleScript.Vty
import qualified Data.Text.IO as T
import Text.Parsec
import Graphics.Vty

main :: IO ()
main = do
  text1 <- T.readFile "test.txt"
  cfg <- standardIOConfig
  vty <- mkVty cfg
  let
    Right output = runParser potatoParse emptyOutput "(unknown)" text1
    level@(Level sz _ _) = head $ _levels output
    lm = _legend output
    om = _objectList output
    levelState = initLevelState lm level
    pic = renderLevel om sz levelState
  update vty pic
  e <- nextEvent vty
  shutdown vty
