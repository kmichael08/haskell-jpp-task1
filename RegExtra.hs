module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving(Eq,Ord,Show)

infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
   x === y = equal (simpl x) (simpl y) where
     equal (Lit a) (Lit b) = a == b
     equal x y = True

instance Mon (Reg c) where
  m1 = Eps
  x <> y = (simpl x) :> (simpl y)
  
simpl :: Reg c -> Reg c
simpl (x :| y) = merger (simpl x) (simpl y) where
  merger Empty x = x
  merger x Empty = x
  merger Eps Eps = Eps
  merger x y = x :| y
simpl (Eps :> x) = simpl x
simpl (x :> Eps) = simpl x
simpl (Empty :> x) = Empty
simpl (x :> Empty) = Empty
simpl x = x

nullable :: Reg c -> Bool
nullable x = null_helper simpler where
  null_helper Eps = True
  null_helper (Many _) = True
  null_helper (x :| y) = null_helper x || null_helper y
  null_helper (x :> y) = null_helper x && null_helper y
  null_helper _ = False
  simpler = simpl x

empty :: Reg c -> Bool 
empty z = empty_helper simpler where
  empty_helper Empty = True
  empty_helper _ = False
  simpler = simpl z

der :: Eq c => c -> Reg c -> Reg c
der ch exp = simpl (der_h ch (simpl exp)) where
  der_h c (a :| b) = (der_h c a) :| (der_h c b)
  der_h c Empty = Empty
  der_h c Eps = Eps
  der_h c (Lit a) = Empty
  der_h c ex@(Many sub_ex) = if (mayStart c sub_ex) then ex else Empty
  der_h c (a :> b) =
    if (dera /= Empty) then simpl (dera :> b) else (if (nullable a) then der_h c b else Empty) where
      dera = der_h c a

ders :: Eq c => [c] -> Reg c -> Reg c
ders c r = r

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = False

mayStart :: Eq c => c -> Reg c -> Bool
mayStart ch exp = startHelper ch (simpl exp) where
  startHelper ch Empty = False
  startHelper ch Eps = False
  startHelper ch (a :| b) = startHelper ch a || startHelper ch b
  startHelper ch (Lit a) = ch == a
  startHelper ch (Many ex) = startHelper ch ex
  startHelper ch (a :> b) = startHelper ch a || (nullable a && startHelper ch b)

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w = Nothing

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r w = Nothing

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = []

char :: Char -> Reg Char
char = Lit

string :: [Char] -> Reg Char
string = foldr1 (:>) . map Lit

alts :: [Char] -> Reg Char
alts = foldr1 (:|) . map Lit

letter = alts ['a'..'z'] :| alts ['A'..'Z']
digit = alts ['0'..'9']
number = digit :> Many digit
ident = letter :> Many (letter :| digit)

many1 r = r :> Many r
