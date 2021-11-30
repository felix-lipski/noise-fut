
module Algebra.Config where
import qualified Algebra.Raw as Raw
import Foreign.C

data ContextOption
    = Debug Int
    | Log Int

setOption config option = case option of
    (Debug flag)         -> Raw.context_config_set_debugging config flag
    (Log flag)           -> Raw.context_config_set_logging   config flag
