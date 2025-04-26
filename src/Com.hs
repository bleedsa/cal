module Com where

import Text.Megaparsec
import Debug.Trace
import Data.Text (Text, unpack)

type Res t = Either Text t

-- #define DBG

#ifdef DBG
dbgTrace x = traceM x
#else
dbgTrace _ = pure ()
#endif

lnOfSrc :: Text -> Int -> String
lnOfSrc src i = (lines $ unpack src) !! (i - 1)

arrowTxt :: Int -> String
arrowTxt i = replicate (i - 1) '-' ++ "^"

slice :: Int -> Int -> [a] -> [a]
slice from to x = take (to - from + 1) $ drop from x
