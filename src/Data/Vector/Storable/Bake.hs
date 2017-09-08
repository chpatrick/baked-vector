{-# LANGUAGE MagicHash, TemplateHaskell #-}

module Data.Vector.Storable.Bake
  ( bake
  , unsafeFromPtrLen
  ) where

import qualified Data.Vector.Storable as VS
import Foreign
import GHC.Exts
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

-- | Bakes a given `Vector` into the binary.
bake :: Storable a => VS.Vector a -> Q (TExp (VS.Vector a))
bake xs = [||unsafeFromPtrLen $$(ptr) len||]
  where
    ptr = unsafeTExpCoerce [|Ptr $(litE $ StringPrimL $ VS.toList $ VS.unsafeCast xs)|]
    len = VS.length xs

unsafeFromPtrLen :: Storable a => Ptr a -> Int -> VS.Vector a
unsafeFromPtrLen ptr =
  VS.unsafeFromForeignPtr0 (unsafePerformIO (newForeignPtr_ ptr))
{-# NOINLINE unsafeFromPtrLen #-}
