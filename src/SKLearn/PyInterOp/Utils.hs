{-# LANGUAGE ForeignFunctionInterface, ExtendedDefaultRules, GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module SKLearn.PyInterOp.Utils where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Control.Monad
import Language.C.Inline



-- foreign import ccall "&decref_with_gil"
--   decref_with_gil :: FunPtr (Ptr a -> IO ())
