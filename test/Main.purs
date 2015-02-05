module Test.Main where

import Debug.Trace
import Data.Maybe
import Test.QuickCheck
import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV
import qualified Data.ArrayBuffer.Typed as TA
import qualified Data.ArrayBuffer.Serializer as S
import qualified Data.ArrayBuffer.Deserializer as D
import Data.ArrayBuffer.Show
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Exception
import Math

instance eqInt8 :: Eq Int8 where
  (==) (Int8 v0) (Int8 v1) = v0 == v1
  (/=) a b = not $ a == b

instance showInt8 :: Show Int8 where
  show (Int8 v) = "Int8 " ++ show v

instance arbInt8 :: Arbitrary Int8 where
  arbitrary = uniformToInt8 <$> arbitrary
    where
    uniformToInt8 n = Int8 $ Math.floor (n * 256) - 128

data V4I8 = V4I8 Int8 Int8 Int8 Int8

instance eqV4I8 :: Eq V4I8 where
  (==) (V4I8 x0 y0 z0 t0) (V4I8 x1 y1 z1 t1) = x0 == x0 && y0 == y1 && z0 == z1 && t0 == t1
  (/=) a b = not $ a == b

instance showV4I8 :: Show V4I8 where
  show (V4I8 x y z t) = "(V4I8 " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show t ++ ")"
  
instance arbV4I8 :: Arbitrary V4I8 where
  arbitrary = V4I8 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance v4i8IsSerializable :: S.IsSerializable V4I8 where
  put d (V4I8 x y z t) = do
    let i8 = S.put d
    i8 x
    i8 y
    i8 z
    i8 t

instance v4i8IsDeserializable :: D.IsDeserializable V4I8 where
  get d = do
    let i8 = D.get d
    x <- i8
    y <- i8
    z <- i8
    t <- i8
    return $ V4I8 x y z t

data M4I8 = M4I8 V4I8 V4I8 V4I8 V4I8

instance eqM4I8 :: Eq M4I8 where
  (==) (M4I8 x0 y0 z0 t0) (M4I8 x1 y1 z1 t1) = x0 == x0 && y0 == y1 && z0 == z1 && t0 == t1
  (/=) a b = not $ a == b

instance showM4I8 :: Show M4I8 where
  show (M4I8 x y z t) = "M4I8 " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show t

instance arbM4I8 :: Arbitrary M4I8 where
  arbitrary = M4I8 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  

instance m4i8IsSerializable :: S.IsSerializable M4I8 where
  put d (M4I8 x y z t) = do
    let v4i8 = S.put d
    v4i8 x
    v4i8 y
    v4i8 z
    v4i8 t

instance m4i8IsDeserializable :: D.IsDeserializable M4I8 where
  get d = do
    let v4i8= D.get d
    x <- v4i8
    y <- v4i8
    z <- v4i8
    t <- v4i8
    return $ M4I8 x y z t

foreign import ut """
function ut(a) {
  console.log(a);
  return a;
}
""" :: forall a. a -> a

main :: Eff (trace :: Trace, random :: Random, err :: Exception, writer :: DV.Writer, reader :: DV.Reader) Unit
main = do
  let ab = AB.create 4
  assert $ AB.byteLength ab == 4
  assert $ AB.byteLength (AB.slice 0 2 ab) == 2
  assert $ AB.byteLength (AB.slice 2 4 ab) == 2
  assert $ AB.byteLength (AB.slice (-2) (-2) ab) == 0
  assert $ AB.byteLength (AB.slice (-2) (-1) ab) == 1     
  assert $ DV.byteLength (DV.slice 0 2 ab) == 2
  assert $ DV.byteLength (DV.slice 2 2 ab) == 2
  let aab = AB.fromArray [1, 2, 3, 4]
  assert $ AB.byteLength aab == 4
  let sab = AB.fromString "hola"
  assert $ AB.byteLength sab == 8

  let nab = AB.create 8
  let dv = DV.whole nab      
  assert $ AB.byteLength (DV.buffer dv) == 8

  assert $ AB.byteLength (DV.buffer $ TA.dataView (TA.asInt8Array dv)) == 8

  assert $ DV.byteLength (DV.slice 0 4 nab) == 4

  assert $ do
     let ab = AB.fromArray [1,2,3,4]
     let dv = DV.whole ab
     let i8 = TA.asInt8Array dv
     ((Just 2) == i8 `TA.at` 1) && (Nothing == i8 `TA.at` 4) && (Nothing == i8 `TA.at` (-1))

  quickCheck' 5000 serdes

serdes :: M4I8 -> M4I8 -> M4I8 -> M4I8 -> Boolean
serdes m0 m1 m2 m3 = forcePure $ do
    a <- S.serialized 256 $ \s -> do
      let p = S.put s
      p m0
      p m1
      p m2
      p m3      
    d <- D.deserializer a
    let g = D.get d
    m0' <- g
    m1' <- g
    m2' <- g
    m3' <- g
    return $ m0 == m0' && m1 == m1' && m2 == m2' && m3 == m3'
        

assert :: Boolean -> QC Unit
assert = quickCheck' 1

foreign import forcePure "function forcePure(e) { return e(); }" :: forall e. (Eff (|e) Boolean) -> Boolean

