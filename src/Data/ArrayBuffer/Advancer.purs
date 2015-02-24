module Data.ArrayBuffer.Advancer where

import Data.ArrayBuffer.Types
import Control.Monad.Eff

type Advancer = { dv :: DataView, off :: ByteOffset }

foreign import advance
"""
function advance(s) {
  return function(d) {
    return function() {
      var off = d.off;
      d.off += s;
      return off;
    };
  };
}
""" :: forall e. ByteOffset -> Advancer -> Eff ( |e) ByteOffset
