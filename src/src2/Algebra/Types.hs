{-# LANGUAGE RankNTypes, ExistentialQuantification, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Algebra.Types where
import qualified Algebra.Raw as Raw
import Algebra.Utils
import Algebra.TypeClasses
import qualified Foreign as F
import qualified Data.Massiv.Array as M
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CBool(..), CSize(..), CChar(..))
import Foreign.Ptr (Ptr)

newtype F32_1d c = F32_1d (F.ForeignPtr Raw.Futhark_f32_1d)
instance FutharkObject F32_1d Raw.Futhark_f32_1d where
  wrapFO = F32_1d
  freeFO = Raw.free_f32_1d
  withFO (F32_1d fp) = F.withForeignPtr fp
instance FutharkArray F32_1d Raw.Futhark_f32_1d M.Ix1 Float where
  shapeFA  = to1d Raw.shape_f32_1d
  newFA    = from1d Raw.new_f32_1d
  valuesFA = Raw.values_f32_1d

newtype F32_2d c = F32_2d (F.ForeignPtr Raw.Futhark_f32_2d)
instance FutharkObject F32_2d Raw.Futhark_f32_2d where
  wrapFO = F32_2d
  freeFO = Raw.free_f32_2d
  withFO (F32_2d fp) = F.withForeignPtr fp
instance FutharkArray F32_2d Raw.Futhark_f32_2d M.Ix2 Float where
  shapeFA  = to2d Raw.shape_f32_2d
  newFA    = from2d Raw.new_f32_2d
  valuesFA = Raw.values_f32_2d
