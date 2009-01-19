import Control.Monad.State
import System.Random

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom =
    do gen <- get
       let (val, gen') = random gen
       put gen'
       return val
