import qualified Models as M

import Database

import Config (loadConfig, defaultConfig, Config(..))
import System.IO (hFlush, stdout)
import UI (start, setup)



main :: IO ()
main = do
	start
   