module Bridge where

import           Data.Proxy
import           Game
import           Language.PureScript.Bridge

myTypes = [let p = (Proxy :: Proxy (Pair Integer)) in order p (mkSumType p),
           let p = (Proxy :: Proxy GameStatus) in equal p (mkSumType p),
           let p = (Proxy :: Proxy Outcome) in equal p (mkSumType p),
           let p = (Proxy :: Proxy MoveError) in equal p (mkSumType p),
           let p = (Proxy :: Proxy Space) in order p (mkSumType p),
           let p = (Proxy :: Proxy Game) in equal p (mkSumType p),
           let p = (Proxy :: Proxy GameState) in equal p (mkSumType p),
           let p = (Proxy :: Proxy Group) in equal p (mkSumType p)]

generateModules :: IO ()
generateModules = writePSTypes "./purescript" (buildBridge defaultBridge) myTypes
