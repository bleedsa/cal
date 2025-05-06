module Cc.Cosmo where

import Control.Monad.State
import Text.Printf
import Text.Megaparsec (parse, errorBundlePretty)
import Data.Text (Text)
import qualified Data.Text as T

import Com
import Fmt
import Ty
import Parse
import Ir
import TyCh

ccErr :: String -> CcMonad Text s a
ccErr msg = lift $ Left $ T.pack $ printf "err: 'cc: %s" msg

ccType :: Type -> CcMonad Text s String
ccType GenInt = pure "int"
ccType (Signed 64) = pure "signed long long"
ccType (Unsigned 64) = pure "unsigned long long"
ccType t = ccErr $ printf "cannot cc type %s" $ fmtType t

ccInstr :: Instr -> CcPrcT String
ccInstr (Lit t (Var x) (IrInt yt y)) = do{ t <- (t `is` yt) `maybeOr` ccErr (printf "%s does not match %s"
                                                                                    (fmtType t) (fmtType yt))
                                         ; t' <- ccType t
                                         ; return $ printf "%s t%d = t%d" t' x y
                                         }
ccInstr (Add t (Var r) (Var x) (Var y)) = do{ t <- ccType t
                                            ; return $ printf "%s t%d = t%d + t%d" t r x y
                                            }
ccInstr (Local t n) = do{ t <- ccType t
                        ; pure $ printf "%s %s" t $ T.unpack n
                        }
ccInstr (LoadLocal t (Var r) n) = do{ t <- ccType t
                                    ; pure $ printf "%s t%d = %s" t r $ T.unpack n
                                    }
ccInstr (Ret (Var r)) = pure $ printf "return t%d" r
ccInstr i = ccErr $ printf "cannot cc instr\n%s" $ show i

ccInstrs :: [Instr] -> CcPrcT ()
ccInstrs [] = pure ()
ccInstrs (h:t) = do{ i <- ccInstr h
                   ; pushPInstr i
                   ; ccInstrs t
                   }

ccFun :: Named (Typed Function) -> CcPrcT ()
ccFun (n, (t, f)) = do{ r <- ccType $ arrowRet t
                      ; pushPInstr $ printf "%s %s() {" r n
                      ; ccInstrs $ finstrs f
                      ; pushPInstr "}"
                      }

ccFuns :: [Named (Typed Function)] -> CcObjT ()
ccFuns [] = pure ()
ccFuns (h:t) = do{ src <- getOSrc
                 ; obj <- getOObj
                 ; x <- un $ execStateT (ccFun h) $ mkPrc src obj
                 ; pushOPrc x
                 ; ccFuns t
                 }

ccMod :: Mod -> CcObjT ()
ccMod mod = ccFuns $ funs mod

ccTxt :: Text -> IO ()
ccTxt src = putStrLn $ case parse top "" src of
                            Left e -> errorBundlePretty e
                            Right x -> un $ do{ ir <- execStateT (cmpTopLeaves x) mod
                                              ; cc <- execStateT (ccMod ir) $ mkObj src ir
                                              ; Right cc
                                              }
           where
               mod = mkMod src
               un (Left e) = T.unpack e
               un (Right x) = fmtObj x

ccFile :: Text -> IO ()
ccFile f = do{ c <- readFile $ T.unpack f
             ; ccTxt $ T.pack c
             }
