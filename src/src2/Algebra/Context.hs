
module Algebra.Context where
import qualified Algebra.Raw as Raw
import Algebra.Config
import Foreign as F
import qualified Foreign.Concurrent as FC
import Foreign.C

data Context = Context (ForeignPtr Raw.Futhark_context)

getContext :: [ContextOption] -> IO Context
getContext options = do
     config <- Raw.context_config_new
     mapM_ (setOption config) options
     context <- Raw.context_new config
     Raw.context_config_free config
     fmap Context $ FC.newForeignPtr context (Raw.context_free context)

inContext (Context fp) = withForeignPtr fp
inContextWithError :: Context -> (Ptr Raw.Futhark_context -> IO Int) -> IO ()
inContextWithError context f 
    = inContext context f >>= \code 
    -> if code == 0 
            then inContext context Raw.context_sync 
--              >> inContext context Raw.context_clear_caches --OpenCL specific
              >> return ()  
            else inContext context Raw.context_get_error 
             >>= \cs -> peekCString cs >>= \s -> F.free cs >> error s
