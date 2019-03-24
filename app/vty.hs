
import Potato.PuzzleScript
import Potato.PuzzleScript.Vty
import qualified Data.Text.IO as T
import Text.Parsec
import Graphics.Vty

vtyKeyToKeyboardInput :: Key -> KeyboardInput
vtyKeyToKeyboardInput k = case k of
  KChar 'x' -> K_X
  KChar 'z' -> K_Z
  KLeft -> K_LEFT
  KRight -> K_RIGHT
  KUp -> K_UP
  KDown -> K_DOWN

loop :: Vty -> Point -> ObjectMap -> LevelState -> IO ()
loop vty sz om ls = do
  let
    pic = renderLevel om sz ls
    wait = do
      e <- nextEvent vty
      case e of
        EvKey k_ _ -> case k_ of
          KEsc -> shutdown vty
          k -> do
            let
              keys = vtyKeyToKeyboardInput k
            -- TODO update ls
            loop vty sz om ls
        _ -> wait
  update vty pic
  wait




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
  loop vty sz om levelState
