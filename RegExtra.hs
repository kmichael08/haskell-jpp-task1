-- Michał Kuźba, mk371148

module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving(Eq,Ord,Show)

infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
   x === y = (simpl x) == (simpl y)
     
instance Mon (Reg c) where
  m1 = Eps
  x <> y = x :> y

-- helper function for simpl. subexp r s means r is included in s
subexp :: Eq c => Reg c -> Reg c -> Bool
subexp Empty _ = True
subexp Eps y = nullable y
subexp (Many x) (Many y) = x `subexp` y
subexp (x :| y) r = (x `subexp` r) && (y `subexp` r)
subexp r (x :| y) = (r `subexp` x) || (r `subexp` y) 
subexp r s = r == s

-- simplifies the regular expression
simpl :: Eq c => Reg c -> Reg c
simpl (Many Empty) = Eps
simpl (Many Eps) = Eps
simpl (Many (Many x)) = simpl $ Many x
simpl (Many x) = Many $ simpl x
simpl (r :| s) = merger (simpl r) (simpl s) where
  merger Empty x = x
  merger x Empty = x
  merger Eps Eps = Eps
  merger x y
    | y `subexp` x = x
    | x `subexp` y = y
    | otherwise = x :| y
simpl (x :> (y :> z)) = simpl (x :> y :> z)
simpl (x :> y) = concat (simpl x) (simpl y) where
  concat Eps x = x
  concat x Eps = x
  concat Empty _ = Empty
  concat _ Empty = Empty
  concat r1@(Many x) r2@(Many y)
    | x == y = r1
    | otherwise = r1 :> r2
  concat x y = x :> y
simpl x = x

-- does empty word belongs to the language
nullable :: Reg c -> Bool
nullable Eps = True
nullable (Many _) = True
nullable (x :| y) = nullable x || nullable y
nullable (x :> y) = nullable x && nullable y
nullable _ = False

-- is language empty
empty :: Reg c -> Bool 
empty Empty = True
empty (x :| y) = empty x && empty y
empty (x :> y) = empty x || empty y
empty _ = False

-- derivative of expression on character c
der :: Eq c => c -> Reg c -> Reg c
der ch exp = der_h ch (simpl exp) where
  der_h c (a :| b) = simpl (der_h c a :| der_h c b)
  der_h c Empty = Empty
  der_h c Eps = Empty
  der_h c (Lit a)
    | c == a = Eps
    | otherwise = Empty
  der_h c ex@(Many sub_ex) = simpl (der_h c sub_ex :> ex)
  der_h c (a :> b) 
    | nullable a = simpl (der c b :| (simpl $ der c a :> b))
    | otherwise = simpl (der c a :> b)
      
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

-- helper function prefixes, ex. [1, 2] -> [[], [1], [1, 2]]
prefs [] = [[]]
prefs (x:xs) = []:[x:y | y <- prefs xs]

-- longest prefix of w matching expression r
match :: Eq c => Reg c -> [c] -> Maybe [c]
match r word = match_h (reverse $ prefs word) where
  match_h [] = Nothing
  match_h (w:ws)
    | accepts r w = Just w
    | otherwise = match_h ws

-- searches first (the longest subword of w accepted by r)
search :: Eq c => Reg c -> [c] -> Maybe [c]
search r []
  | nullable r = Just []
  | otherwise = Nothing
search r w@(c:cs)
  | match_pref == Nothing = search r cs
  | otherwise = match_pref
  where match_pref = match r w

-- helper function take value out of Just a
eliminate (Just a) = a

-- finds all locally longest subwords of w matching r
findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = find_h r w 0

-- helper function for findall, len is the length of the previous subword
find_h :: Eq c => Reg c -> [c] -> Int -> [[c]]
find_h r [] _ = []
find_h r w@(c:cs) len
  | pref == Nothing || (new_len <= len && len > 0) = find_h r cs (max 0 (len - 1))
  | otherwise = new_el:(find_h r cs (max 0 (new_len - 1)))
  where
    pref = match r w
    new_el = eliminate pref
    new_len = length new_el

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
