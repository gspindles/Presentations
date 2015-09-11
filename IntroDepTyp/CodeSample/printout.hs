-- Teaser:
infixr 5 :>
data Vect (n :: Nat) a where
  VNil :: Vect 0 a
  (:>) :: a -> Vect n a -> Vect (n :+ 1) a

vs :: Vect 6 Int
vs = 4 :> 8 :> 15 :> 16 :> 23 :> 42 :> VNil

-- Translation:
enum Vect<Nat n, A> {
  Vect<0, A> VNil,
  Vect<n :+ 1, A> VCons<A>(A a, Vect<n, A> va)
}

Vect<6, Int> vs = VCons(4, VCons(8, VCons(15, VConsr(16, VCons(23, VCons(42, VNil))))))

------------------------------------------------------------
-- Defining new type with data:
data Person = Person String String Int

barbara :: Person
barbara = Person "Barbara" "Smith" 30

-- Translation:
enum Person {
  Person(String firstname, String lastname, Int age)
}

------------------------------------------------------------
-- Types with multiple value constructors:
data Bool = False | True
data Weekdays = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

-- Translation:
enum Bool { False, True }
enum Weekdays { Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday }

------------------------------------------------------------
-- Type synonyms - aliasing:
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

-- Translation:
enum Shape {
  Triangle(Double side1, Double side2, Double side3),
  Rectangle(Double side1, Double side2),
  Circle(Double radius)
}

------------------------------------------------------------
-- Types parametrizing over types:
data Tuple a b = Tuple a b

type Employed = Bool
barbara1, chet1, luffy1 :: (Person, Employed)
barbara1 = (Person "Barbara" "Smith" 30, True)
chet1    = (Person "Chet" "Awesome-Laser" 2, False)
luffy1   = (Person "Luffy D." "Monkey" 19, False)

-- Translation:
enum Tuple<A, B> {
  Tuple<A,B>(A a, B b)
}

------------------------------------------------------------
-- Types parametrizing over types:
data Maybe a = Nothing | Just a

type Occupation = Maybe String
barbara2, chet2, luffy2 :: (Person, Occupation)
barbara2 = (Person "Barbara" "Smith" 30, Just "dancer")
chet2    = (Person "Chet" "Awesome-Laser" 2, Nothing)
luffy2   = (Person "Luffy D." "Monkey" 19, Just "pirate")

-- Translation:
enum Maybe<A> {
  Nothing,
  Just<A>(A a)
}
------------------------------------------------------------
-- Types with recursive definition:
data Nat = Z | S Nat

0 ~ Z
1 ~ S Z
2 ~ S (S Z)

-- Translation:
enum Nat {
  Z,
  S(Nat n)
}

------------------------------------------------------------
-- Types with recursive definition:
   data List a = Nil | Cons a (List a)
=> data [] a = [] | (:) a ([] a)  -- built-in sugar
=> data [a] = [] | (:) a [a]  -- 2x sugar!

ints :: List Int
ints = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
ints = 1 : 2 : 3 : 4 : []  -- built-in sugar
ints = [1, 2, 3, 4]  -- 2x sugar

-- Translation:
enum List<A> {
  Nil,
  Cons<A>(A a, List<A> as)
}

------------------------------------------------------------
-- Functions
even :: Int -> Bool
even 0 = True
even n = if rem n 2 == 0
         then True
         else False

-- Translation:
Bool even(Int n) {
  switch n:
    case n == 0:
      return True;
    default:
      if rem(n, 2) == 0:
        return True;
      else
        return False;
}
------------------------------------------------------------
-- Function on recursive types
toInt :: Nat -> Int
toInt Z     = 0
toInt (S n) = 1 + toInt n

three = S (S (S Z))
  toInt three
= toInt (S (S (S Z)))
= 1 + toInt (S (S Z))
= 1 + 1 + toInt (S Z)
= 1 + 1 + 1 + toInt Z
= 1 + 1 + 1 + 0
= 3

-- Translation:
Int toInt(Nat n) {
  switch n:
    case Z:
      return 0;
    case (S m):  -- n ~ S m
      return 1 + toInt(m);
}

------------------------------------------------------------
-- Parametric functions
length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

  length [4, 8, 15]
= 1 + length [8, 15]
= 1 + 1 + length [15]
= 1 + 1 + 1 + length []
= 1 + 1 + 1 + 0
= 3

-- Translation:
Int length(List<A> ls) {
  switch ls:
    case Nil:
      return 0;
    case Cons(x, xs):
      return 1 + length(xs);
}

------------------------------------------------------------
-- Higher-order functions
apply :: (a -> b) -> a -> b
apply f x = f x

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- Translation:
B apply(Func<A, B> f, A a) {
  return f(a);
}

Func<A, C> compose(Func<B, C> f, Func<A, B> g) {
  return x => f(g(x));
}

