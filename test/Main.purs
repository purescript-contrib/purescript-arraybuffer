module Test.Main where

import Debug.Trace
import Data.Either
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

newtype Comp = Comp Number

instance isSerializableComp :: S.IsSerializable Comp where
  put d (Comp v) = S.put d $ Int8 v

instance isDeserializableComp :: D.IsDeserializable Comp where
  get d = do
    v <- D.getInt8 d
    return $ case v of
      (Right (Int8 vv)) -> Right $ Comp vv
      (Left err) -> Left err

instance eqComp :: Eq Comp where
  (==) (Comp v0) (Comp v1) = v0 == v1
  (/=) a b = not $ a == b

instance showComp :: Show Comp where
  show (Comp v) = "Comp " ++ show v

instance arbComp :: Arbitrary Comp where
  arbitrary = uniformToComp <$> arbitrary
    where
    uniformToComp n = Comp $ Math.floor (n * 256) - 128

data V4 = V4 Comp Comp Comp Comp

instance eqV4 :: Eq V4 where
  (==) (V4 x0 y0 z0 t0) (V4 x1 y1 z1 t1) = x0 == x0 && y0 == y1 && z0 == z1 && t0 == t1
  (/=) a b = not $ a == b

instance showV4 :: Show V4 where
  show (V4 x y z t) = "(V4 " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show t ++ ")"
  
instance arbV4 :: Arbitrary V4 where
  arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance v4IsSerializable :: S.IsSerializable V4 where
  put d (V4 x y z t) = do
    let i8 = S.put d
    i8 x
    i8 y
    i8 z
    i8 t

instance v4IsDeserializable :: D.IsDeserializable V4 where
  get d = do
    let comp = D.get d
    x <- comp
    y <- comp
    z <- comp
    t <- comp
    return $ V4 <$> x <*> y <*> z <*> t
    
data M4 = M4 V4 V4 V4 V4

instance eqM4 :: Eq M4 where
  (==) (M4 x0 y0 z0 t0) (M4 x1 y1 z1 t1) = x0 == x0 && y0 == y1 && z0 == z1 && t0 == t1
  (/=) a b = not $ a == b

instance showM4 :: Show M4 where
  show (M4 x y z t) = "M4 " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show t

instance arbM4 :: Arbitrary M4 where
  arbitrary = M4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  

instance m4IsSerializable :: S.IsSerializable M4 where
  put d (M4 x y z t) = do
    let v4 = S.put d
    v4 x
    v4 y
    v4 z
    v4 t

instance m4IsDeserializable :: D.IsDeserializable M4 where
  get d = do
    let v4= D.get d
    x <- v4
    y <- v4
    z <- v4
    t <- v4
    return $ M4 <$> x <*> y <*> z <*> t

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

  assert $ [1,2,3] == (TA.toArray $ TA.asInt8Array $ DV.whole $ AB.fromArray [1,2,3])

  quickCheck short

  quickCheck serdes

serdes :: M4 -> M4 -> M4 -> M4 -> Boolean
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
    return $ (Right m0) == m0' && (Right m1) == m1' && (Right m2) == m2' && (Right m3) == m3'

short :: M4 -> M4 -> Boolean
short m0 m1 = forcePure $ do
    a <- S.serialized 256 $ \s -> do
      let p = S.put s
      p m0
    d <- D.deserializer a
    let g = D.get d
    m0' <- g
    m1' <- g
    return $ m0' == (Right m0) && m1' /= (Right m1) && m1' == (Left "Short read")

        

assert :: Boolean -> QC Unit
assert = quickCheck' 1

foreign import forcePure "function forcePure(e) { return e(); }" :: forall e. (Eff (|e) Boolean) -> Boolean

