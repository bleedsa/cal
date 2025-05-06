module Asm.Linux_x64 where

import Control.Monad.State
import Text.Printf
import Text.Megaparsec (parse, errorBundlePretty)
import Data.Text (Text)

import Com
import Fmt
import Ty
import Parse
import Ir

asmFun :: Function -> AsmPrcT ()
asmFun f = pure ()

asmMod :: Mod -> AsmObjT ()
asmMod mod = pure ()

asmTxt :: Text -> IO ()
asmTxt src = putStrLn $ case parse top "" src of
                            Left e -> errorBundlePretty e
                            Right x -> un $ do{ ir <- execStateT (cmpTopLeaves x) mod
                                              ; asm <- execStateT (asmMod ir) $ mkObj src ir
                                              ; Right asm
                                              }
           where
               mod = mkMod src
               un (Left e) = show e
               un (Right x) = show x
