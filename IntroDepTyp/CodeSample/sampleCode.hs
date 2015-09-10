{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Data.Proxy
import Data.Singletons

data Person = Person String String Int
              deriving (Show, Eq)


type Side   = Double
type Radius = Double

data Shape = Triangle Side Side Side
           | Rectangle Side Side
           | Circle Radius
           deriving (Show, Eq)

myTri, myRect, myCir :: Shape
myTri  = Triangle 2.1 3.2 5
myRect = Rectangle 4 4
myCir  = Circle 7.2


data Id a = Id a deriving (Show, Eq)


data Tuple a b = Tuple a b

type Employed = Bool
barbara1, chet1, luffy1 :: (Person, Employed)
barbara1 = (Person "Barbara" "Smith" 30, True)
chet1    = (Person "Chet" "Awesome-Laser" 2, False)
luffy1   = (Person "Luffy D." "Monkey" 19, False)


type Occupation = Maybe String
barbara2, chet2, luffy2 :: (Person, Occupation)
barbara2 = (Person "Barbara" "Smith" 30, Just "dancer")
chet2    = (Person "Chet" "Awesome-Laser" 2, Nothing)
luffy2   = (Person "Luffy D." "Monkey" 19, Just "pirate")


type Earning = Either String Int
barbara3, chet3, luffy3 :: (Person, Earning)
barbara3 = (Person "Barbara" "Smith" 30, Right 100000)
chet3    = (Person "Chet" "Awesome-Laser" 2, Left "Is a baby")
luffy3   = (Person "Luffy D." "Monkey" 19, Right 2000000)


data Nat = Z | S Nat deriving (Show, Eq)
data List a = Nil | Cons a (List a)

myEven :: Int -> Bool
myEven 0 = True
myEven n = if rem n 2 == 0
           then True
           else False

lappend :: [a] -> [a] -> [a]
lappend [] ys     = ys
lappend (x:xs) ys = x : lappend xs ys

apply :: (a -> b) -> a -> b
apply f x = f x

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

lmap :: (a -> b) -> [a] -> [b]
lmap f []     = []
lmap f (x:xs) = f x : map f xs

lzip :: [a] -> [b] -> [(a,b)]
lzip (x:xs) (y:ys) = (x,y) : zip xs ys
lzip _      _      = []

lreplicate :: Int -> a -> [a]
lreplicate 0 x = []
lreplicate n x = x : replicate (n - 1) x

lfilter :: (a -> Bool) -> [a] -> [a]
lfilter f []     = []
lfilter f (x:xs) = if f x
                   then x : filter f xs
                   else filter f xs


type ShowCxt a b = (Show a, Show b)
sameSerialization :: ShowCxt a b => a -> b -> Bool
sameSerialization a b = show a == show b

data MyB :: * where
  F :: MyB
  T :: MyB

data MyM :: * -> * where
  N :: MyM a
  J :: a -> MyM a

data MyL :: * -> * where
  E :: MyL a
  C :: a -> MyL a -> MyL a

data TextInput (a :: Bool) where
  RawText  :: String -> TextInput False
  SafeText :: String -> TextInput True

htmlEncode :: String -> String
htmlEncode = undefined

sanitize :: TextInput a -> TextInput True
sanitize (RawText s)  = SafeText $ htmlEncode s
sanitize (SafeText s) = SafeText s


type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add Z     m = m
  Add (S n) m = S (Add n m)

type family (:+) n m where
  Z :+ m     = m
  (S n) :+ m = S (n :+ m)

type Assoc n = (S n) ~ ((S Z) :+ n)

data Vect n a where
  VNil :: Vect Z a
  (:>) :: a -> Vect n a -> Vect (S n) a
infixr 5 :>

type Six = S (S (S (S (S (S Z)))))
vs :: Vect Six Int
vs = 4 :> 8 :> 15 :> 16 :> 23 :> 42 :> VNil

vhead :: Vect (S n) a -> a
vhead (x:>xs) = x

vtail :: Vect (S n) a -> Vect n a
vtail (x:>xs) = xs

vappend :: Vect n a -> Vect m a -> Vect (n :+ m) a
vappend VNil ys     = ys
vappend (x:> xs) ys = x :> vappend xs ys

vmap :: (a -> b) -> Vect n a -> Vect n b
vmap f VNil    = VNil
vmap f (x:>xs) = f x :> vmap f xs

vzip :: Vect n a -> Vect n b -> Vect n (a, b)
vzip (x:>xs) (y:>ys) = (x,y) :> vzip xs ys
vzip VNil    VNil    = VNil

type family Min n m where
  Min Z     m     = Z
  Min n     Z     = Z
  Min (S n) (S m) = S (Min n m)

vzip2 :: Vect n a -> Vect m b -> Vect (Min n m) (a, b)
vzip2 (x:>xs) (y:>ys) = (x,y) :> vzip2 xs ys
vzip2 VNil    ys      = VNil
vzip2 xs      VNil    = VNil

data HList (t :: [*]) where
  HNil :: HList '[]
  (::>) :: t -> HList ts -> HList (t ': ts)
infixr 5 ::>

defaults :: HList '[Int, Bool, Maybe a]
defaults = 0 ::> False ::> Nothing ::> HNil

data HVect (n :: Nat) (t :: [*]) where
  HVNil :: HVect Z '[]
  (:>>) :: t -> HVect n ts -> HVect (S n) (t ': ts)
infixr 5 :>>

type Three = S (S (S Z))
vdefaults :: HVect Three '[Int, Bool, Maybe a]
vdefaults = 0 :>> False :>> Nothing :>> HVNil

data instance Sing (n :: Nat) where
  SZ :: Sing Z
  SS :: Sing n -> Sing (S n)

type SNat (n :: Nat) = Sing n

vreplicate :: Sing (n :: Nat) -> a -> Vect n a
vreplicate SZ     a = VNil
vreplicate (SS n) a = a :> vreplicate n a


data Sigma :: KProxy a -> (a -> *) -> * where
  Exists :: Sing (x :: a) -> b x -> Sigma ('KProxy :: KProxy a) b

