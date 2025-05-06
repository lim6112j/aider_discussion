module TH where

import Language.Haskell.TH

hello :: Q [Dec]
hello = do
  name <- newName "world"
  return [ FunD (mkName "hello") 
          [ Clause [] (NormalB (LitE (StringL ("Hello " ++ show name))) [] ] ]
