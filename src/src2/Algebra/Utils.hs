{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Algebra.Utils where
import qualified Algebra.Raw as Raw
import Algebra.Context
import Algebra.FT
import Algebra.TypeClasses
import Foreign as F
import qualified Foreign.Concurrent as FC
import Foreign.C
import qualified Data.Massiv.Array as M
import qualified Data.Massiv.Array.Unsafe as MU

wrapIn context rawObject 
    = fmap wrapFO
    $ FC.newForeignPtr
        rawObject 
        (inContextWithError context $ \c -> freeFO c rawObject)

peekFree p = peek p >>= \v -> free p >> return v
peekFreeWrapIn context rawP 
    = peek rawP >>= wrapIn context >>= \fo -> F.free rawP >> return fo


instance Input CBool Bool where 
    toFuthark b = return $ if b then fromInteger 1 else fromInteger 0

instance Output CBool Bool where
    fromFuthark cb = return $ if cb /= fromInteger 0 then True else False


-- Ptr - Dim conversion

to1d f cP aP
       = f cP aP
       >>= fmap (\[d0] -> M.Sz1 d0)
       . fmap (fmap fromIntegral)
       . peekArray 1

to2d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1] -> M.Sz2 d0 d1)
       . fmap (fmap fromIntegral)
       . peekArray 2

to3d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2] -> M.Sz3 d0 d1 d2)
       . fmap (fmap fromIntegral)
       . peekArray 3

to4d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2, d3] -> M.Sz4 d0 d1 d2 d3)
       . fmap (fmap fromIntegral)
       . peekArray 4

to5d f cP aP
       = f cP aP
       >>= fmap (\[d0, d1, d2, d3, d4] -> M.Sz5 d0 d1 d2 d3 d4)
       . fmap (fmap fromIntegral)
       . peekArray 5

from1d f cP eP (M.Sz1 d0)             = f cP eP (fromIntegral d0)

from2d f cP eP (M.Sz2 d0 d1)          = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)

from3d f cP eP (M.Sz3 d0 d1 d2)       = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)
                                                (fromIntegral d2)

from4d f cP eP (M.Sz4 d0 d1 d2 d3)    = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)
                                                (fromIntegral d2)
                                                (fromIntegral d3)

from5d f cP eP (M.Sz5 d0 d1 d2 d3 d4) = f cP eP (fromIntegral d0)
                                                (fromIntegral d1)
                                                (fromIntegral d2)
                                                (fromIntegral d3)
                                                (fromIntegral d4)

instance (FutharkArray array rawArray dim element)
  => Input (array c) (M.Array M.S dim element) where
    toFuthark array = unsafeLiftFromIO $ \context
      -> inContext context $ \c
      -> MU.unsafeWithPtr array (\aP -> newFA c aP $ M.size array)
      >>= wrapIn context

instance (FutharkArray array rawArray dim element)
  => Output (array c) (M.Array M.S dim element) where
    fromFuthark array = unsafeLiftFromIO $ \context
      -> inContext context $ \c
      -> withFO array $ \aP
      -> do
          shape <- shapeFA c aP
          pointer <- mallocForeignPtrArray $ M.totalElem shape
          withForeignPtr pointer $ valuesFA c aP
          return $ M.resize' shape
                 $ MU.unsafeArrayFromForeignPtr0 M.Seq pointer
                 $ M.Sz1 (M.totalElem shape)

instance (Output a a', Output b b')
  => Output (a, b) (a', b') where
    fromFuthark (a, b) = do
        a' <- fromFuthark a
        b' <- fromFuthark b
        return (a', b')

instance (Output a a', Output b b', Output c c')
  => Output (a, b, c) (a', b', c') where
    fromFuthark (a, b, c) = do
        a' <- fromFuthark a
        b' <- fromFuthark b
        c' <- fromFuthark c
        return (a', b', c')

instance (Output a a', Output b b', Output c c', Output d d')
  => Output (a, b, c, d) (a', b', c', d') where
    fromFuthark (a, b, c, d) = do
        a' <- fromFuthark a
        b' <- fromFuthark b
        c' <- fromFuthark c
        d' <- fromFuthark d
        return (a', b', c', d')

instance (Output a a', Output b b', Output c c', Output d d', Output e e')
  => Output (a, b, c, d, e) (a', b', c', d', e') where
    fromFuthark (a, b, c, d, e) = do
        a' <- fromFuthark a
        b' <- fromFuthark b
        c' <- fromFuthark c
        d' <- fromFuthark d
        e' <- fromFuthark e
        return (a', b', c', d', e')


instance (Input a a', Input b b')
  => Input (a, b) (a', b') where
    toFuthark (a, b) = do
        a' <- toFuthark a
        b' <- toFuthark b
        return (a', b')

instance (Input a a', Input b b', Input c c')
  => Input (a, b, c) (a', b', c') where
    toFuthark (a, b, c) = do
        a' <- toFuthark a
        b' <- toFuthark b
        c' <- toFuthark c
        return (a', b', c')

instance (Input a a', Input b b', Input c c', Input d d')
  => Input (a, b, c, d) (a', b', c', d') where
    toFuthark (a, b, c, d) = do
        a' <- toFuthark a
        b' <- toFuthark b
        c' <- toFuthark c
        d' <- toFuthark d
        return (a', b', c', d')

instance (Input a a', Input b b', Input c c', Input d d', Input e e')
  => Input (a, b, c, d, e) (a', b', c', d', e') where
    toFuthark (a, b, c, d, e) = do
        a' <- toFuthark a
        b' <- toFuthark b
        c' <- toFuthark c
        d' <- toFuthark d
        e' <- toFuthark e
        return (a', b', c', d', e')

