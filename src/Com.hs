module Com where

import Text.Megaparsec
import Debug.Trace

type Res t = Either String t

#define DBG

#ifdef DBG
dbgTrace x = traceM x
#else
dbgTrace _ = pure ()
#endif
