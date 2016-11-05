
module Tkach07 where
import qualified Data.List as List

data AbstractInteger 
    = Zero
    | Succ AbstractInteger
    | Pred AbstractInteger
    deriving (Show, Eq)


-- Exercise 1 -----------------------------------------
instance Ord AbstractInteger where
	(<=) = lessThen
	compare  = ai_Compare
	x < y = ai_Compare x y == LT
	x > y = ai_Compare x y == GT
   
lessThen ::AbstractInteger->AbstractInteger->Bool
lessThen x y = not(x>y)   
-- Exercise 2 ----------------------------------------
ai_toInteger :: AbstractInteger -> Integer
ai_toInteger Zero = 0
ai_toInteger (Pred x) = (ai_toInteger x) - 1
ai_toInteger (Succ x) = (ai_toInteger x) + 1
 
-- Exercise 3 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger

plusAbs Zero x = x
plusAbs x Zero = x
plusAbs (Pred x) (Pred y) = plusAbs(Pred (Pred x)) y
plusAbs (Succ x) (Succ y) = plusAbs(Succ (Succ x)) y
plusAbs (Pred x) (Succ y) = plusAbs x y
plusAbs (Succ x) (Pred y) = plusAbs y x

timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs x Zero = 0



timesAbs  x (Pred y) = (-x) + (x *y)
timesAbs x (Succ y) = x+ (x*y)


-- Exercise 4 -----------------------------------------
instance Num AbstractInteger  where
    (+)   = plusAbs
    (*)   = timesAbs
    negate      = ai_Negate
    fromInteger = ai_fromInteger
    abs         = ai_Abs
    signum      = ai_Signum

ai_Negate ::AbstractInteger -> AbstractInteger 
ai_Negate Zero = Zero 
ai_Negate (Pred x) = Succ (ai_Negate x)
ai_Negate (Succ x) = Pred (ai_Negate x)

ai_fromInteger ::Integer ->AbstractInteger
ai_fromInteger 0 = Zero 

ai_fromInteger x |x<0 = Pred (ai_fromInteger (x + 1))
                 |x>0 = Succ (ai_fromInteger (x - 1))


ai_Abs ::AbstractInteger -> AbstractInteger
ai_Abs Zero = Zero
ai_Abs (Pred x) = Succ (negate x)
ai_Abs x = x
ai_Signum ::AbstractInteger -> AbstractInteger
ai_Signum Zero = Zero 
ai_Signum (Pred x) = -1
ai_Signum (Succ x) =  1




ai_Compare ::AbstractInteger -> AbstractInteger -> Ordering
ai_Compare Zero Zero = EQ
ai_Compare Zero (Pred x) = GT
ai_Compare Zero (Succ x) = LT 
ai_Compare (Pred x) Zero = LT 
ai_Compare (Succ x) Zero = GT
ai_Compare (Pred _)(Succ _) = EQ
ai_Compare (Pred _)(Pred _) = EQ
ai_Compare (Succ _)(Succ _) = EQ
ai_Compare (Succ _)(Pred _) = EQ
-- Exercise 5 -----------------------------------------
factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial a = a * factorial (a-1)
 
factorialFold :: (Eq a, Num a) => a -> a
factorialFold a = factor a 1
 where factor 0 x = x
       factor a x = factor (a-1) (a*x)


-- Exercise 6 -----------------------------------------
data Quaternion = Quaternion Double Double Double Double 

instance Show Quaternion where
	show (Quaternion r i j k ) = show r ++ " + " ++ show i ++"i"++" + "++show j++"j"++" + "++show k ++"k"

-- Exercise 7 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion (Quaternion r1 i1 j1 k1) (Quaternion r2 i2 j2 k2 )= Quaternion (r1 + r2) (i1 + i2) (j1+j2) (k1+k2)

timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion(Quaternion r1 i1 j1 k1) (Quaternion r2 i2 j2 k2 )= Quaternion r3 i3 j3 k3
 where r3 = (r2*r1) - (i2 *i1) - (j2 * j1) - (k2* k1)
       i3 = (r2*r1) + (i2 *i1) - (j2 * j1) + (k2* k1)
       j3 = (r2*r1) + (i2 *i1) + (j2 * j1) - (k2* k1)
       k3 = (r2*r1) - (i2 *i1) + (j2 * j1) - (k2* k1)

--- Exercise 8 -----------------------------------------
instance Num Quaternion  where
    (+)   = plusQuaternion
    (*)   = timesQuaternion 
    negate      = qt_Negate
    fromInteger = qt_fromInteger
    abs         = qt_Abs
    signum      = qt_Signum
qt_Str ::Quaternion -> String 
qt_Str (Quaternion r i j k )= show r ++ "+" ++ show i ++"i"++show j++"j"++show k ++"k"





qt_Negate::Quaternion -> Quaternion
qt_Negate (Quaternion r1 i1 j1 k1) = (Quaternion (negate r1) (negate i1) (negate j1) (negate k1) )

qt_Abs::Quaternion -> Quaternion
qt_Abs (Quaternion r1 i1 j1 k1) = (Quaternion (sqrt(r1^2+i1^2+j1^2+k1^2)) 0 0 0)

qt_Signum ::Quaternion -> Quaternion
qt_Signum (Quaternion r i j k ) = Quaternion (r/s) (i/s) (j/s) (k/s)
 where s = real(abs(Quaternion r i j k))


qt_fromInteger::Integer-> Quaternion
qt_fromInteger x = Quaternion (fromIntegral x) 0 0 0
 
real ::Quaternion -> Double 
real (Quaternion r i j k ) = r



