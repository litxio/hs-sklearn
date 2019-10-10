{-# LANGUAGE TemplateHaskell #-}

module SKLearn.PyInterOp.TH where

import System.Process
import Language.Haskell.TH
import Data.Char (isSpace)

numpyIncludeDir :: ExpQ
numpyIncludeDir = do
  includeDirStr <- runIO $
    readCreateProcess 
      (shell "python3 -c 'import numpy; print(numpy.get_include())'") ""
  return $ LitE $ StringL $ rstrip includeDirStr


rstrip = reverse . dropWhile isSpace . reverse
