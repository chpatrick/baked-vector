{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Bake

lookupTable :: VS.Vector Int
lookupTable = $$(bake $ VS.fromList [1..10])

main :: IO ()
main = print lookupTable
