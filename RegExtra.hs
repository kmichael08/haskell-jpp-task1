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

-- simplifies the regular expression
-- FAILS SOMETIMES
simpl :: Reg c -> Reg c
simpl (x :| y) = merger (simpl x) (simpl y) where
  merger Empty x = x
  merger x Empty = x
  merger Eps Eps = Eps
  merger x y = x :| y
simpl (x :> y) = concat (simpl x) (simpl y) where
  concat Eps x = x
  concat x Eps = x
  concat Empty x = Empty
  concat x Empty = Empty
  concat x y = x :> y
simpl x = x

-- does empty word belongs to the language
nullable :: Reg c -> Bool
nullable x = null_helper simpler where
  null_helper Eps = True
  null_helper (Many _) = True
  null_helper (x :| y) = null_helper x || null_helper y
  null_helper (x :> y) = null_helper x && null_helper y
  null_helper _ = False
  simpler = simpl x

-- is language empty
empty :: Reg c -> Bool 
empty z = empty_helper simpler where
  empty_helper Empty = True
  empty_helper _ = False
  simpler = simpl z

-- derivative of expression on character c
der :: Eq c => c -> Reg c -> Reg c
der ch exp = simpl (der_h ch (simpl exp)) where
  der_h c (a :| b) = simpl (der_h c a) :| simpl (der_h c b)
  der_h c Empty = Empty
  der_h c Eps = Empty
  der_h c (Lit a) = if (c == a) then Eps else Empty
  der_h c ex@(Many sub_ex) = if (mayStart c sub_ex) then ex else Empty
  der_h c (a :> b) =
    if (nullable a) then simpl (derb :| derab) else derab where
      dera = der c a
      derb = der c b
      derab = simpl (dera :> b)

-- derivative of expression on the word
ders :: Eq c => [c] -> Reg c -> Reg c
ders [] ex = ex 
ders (c:cs) ex = ders cs (der c ex)

-- whether l(r) accepts the word w
accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = nullable (ders w r)

-- whether ch may start the expression
mayStart :: Eq c => c -> Reg c -> Bool
mayStart ch exp = startHelper ch (simpl exp) where
  startHelper ch Empty = False
  startHelper ch Eps = False
  startHelper ch (a :| b) = startHelper ch a || startHelper ch b
  startHelper ch (Lit a) = ch == a
  startHelper ch (Many ex) = startHelper ch ex
  startHelper ch (a :> b) = startHelper ch a || (nullable a && startHelper ch b)

-- helper function inits, ex. [1, 2] -> [[], [1], [1, 2]]
prefs [] = [[]]
prefs (x:xs) = []:[x:y | y <- prefs xs]

-- longest prefix of w matching expression r
match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w = foldl (\res word -> if (accepts r word) then Just word else res) Nothing (prefs w) 

-- NOT IMPLEMENTED
search :: Eq c => Reg c -> [c] -> Maybe [c]
search r w = Nothing

-- NOT IMPLEMENTED
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
