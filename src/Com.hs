module Com where

import Text.Megaparsec
import Debug.Trace

type Res t = Either String t

-- line, col position
data P = P Pos Pos
         deriving (Show, Eq)

#define DBG

#ifdef DBG
dbgTrace x = traceM x
#else
dbgTrace _ = pure ()
#endif
