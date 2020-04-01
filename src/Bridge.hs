module Bridge where

import           Data.Proxy
import           Game
import           Language.PureScript.Bridge

myTypes = [let p = (Proxy :: Proxy Space) in order p (mkSumType p)]

generateModules :: IO ()
generateModules = writePSTypes "./purescript" (buildBridge defaultBridge) myTypes
