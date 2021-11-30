
module Algebra.Entries where
import qualified Algebra.Raw as Raw
import qualified Algebra.Context as C
import Algebra.FT (FT)
import qualified Algebra.FT as FT
import qualified Algebra.Utils as U
import Algebra.Types
import qualified Algebra.TypeClasses as T
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Foreign as F
import Foreign.C.Types

matrixMatrixMul
  :: F32_2d c
  -> F32_2d c
  -> FT c (F32_2d c)
matrixMatrixMul in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_matrixMatrixMul context' out0 in0' in1')
  >> U.peekFreeWrapIn context out0

vectorMatrixMul
  :: F32_1d c
  -> F32_2d c
  -> FT c (F32_1d c)
vectorMatrixMul in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_vectorMatrixMul context' out0 in0' in1')
  >> U.peekFreeWrapIn context out0

matrixMul
  :: F32_2d c
  -> F32_2d c
  -> FT c (F32_2d c)
matrixMul in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_matrixMul context' out0 in0' in1')
  >> U.peekFreeWrapIn context out0

matrixSub
  :: F32_2d c
  -> F32_2d c
  -> FT c (F32_2d c)
matrixSub in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_matrixSub context' out0 in0' in1')
  >> U.peekFreeWrapIn context out0

matrixAdd
  :: F32_2d c
  -> F32_2d c
  -> FT c (F32_2d c)
matrixAdd in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_matrixAdd context' out0 in0' in1')
  >> U.peekFreeWrapIn context out0

matrixVectorMul
  :: F32_2d c
  -> F32_1d c
  -> FT c (F32_1d c)
matrixVectorMul in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_matrixVectorMul context' out0 in0' in1')
  >> U.peekFreeWrapIn context out0

normalizeV
  :: F32_1d c
  -> FT c (F32_1d c)
normalizeV in0
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_normalizeV context' out0 in0')
  >> U.peekFreeWrapIn context out0

vectorNorm
  :: F32_1d c
  -> FT c Float
vectorNorm in0
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_vectorNorm context' out0 in0')
  >> U.peekFree out0

dot
  :: F32_1d c
  -> F32_1d c
  -> FT c Float
dot in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_dot context' out0 in0' in1')
  >> U.peekFree out0

matrixSgn
  :: F32_2d c
  -> FT c (F32_2d c)
matrixSgn in0
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_matrixSgn context' out0 in0')
  >> U.peekFreeWrapIn context out0

scaleM
  :: Float
  -> F32_2d c
  -> FT c (F32_2d c)
scaleM in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_scaleM context' out0 in0 in1')
  >> U.peekFreeWrapIn context out0

matrixAbs
  :: F32_2d c
  -> FT c (F32_2d c)
matrixAbs in0
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_matrixAbs context' out0 in0')
  >> U.peekFreeWrapIn context out0

vectorMul
  :: F32_1d c
  -> F32_1d c
  -> FT c (F32_1d c)
vectorMul in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_vectorMul context' out0 in0' in1')
  >> U.peekFreeWrapIn context out0

vectorSub
  :: F32_1d c
  -> F32_1d c
  -> FT c (F32_1d c)
vectorSub in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_vectorSub context' out0 in0' in1')
  >> U.peekFreeWrapIn context out0

vectorAdd
  :: F32_1d c
  -> F32_1d c
  -> FT c (F32_1d c)
vectorAdd in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_vectorAdd context' out0 in0' in1')
  >> U.peekFreeWrapIn context out0

vectorSgn
  :: F32_1d c
  -> FT c (F32_1d c)
vectorSgn in0
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_vectorSgn context' out0 in0')
  >> U.peekFreeWrapIn context out0

scaleV
  :: Float
  -> F32_1d c
  -> FT c (F32_1d c)
scaleV in0 in1
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in1 $ \in1'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_scaleV context' out0 in0 in1')
  >> U.peekFreeWrapIn context out0

vectorAbs
  :: F32_1d c
  -> FT c (F32_1d c)
vectorAbs in0
  =  FT.unsafeLiftFromIO $ \context
  -> T.withFO in0 $ \in0'
  -> F.malloc >>= \out0
  -> C.inContextWithError context (\context'
  -> Raw.entry_vectorAbs context' out0 in0')
  >> U.peekFreeWrapIn context out0
