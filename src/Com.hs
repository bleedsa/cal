module Com where

import Text.Megaparsec
import Debug.Trace
import Data.Text (Text)

type Res t = Either Text t

-- #define DBG

#ifdef DBG
dbgTrace x = traceM x
#else
dbgTrace _ = pure ()
#endif
