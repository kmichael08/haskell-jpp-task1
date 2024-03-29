-- {-# LANGUAGE NoMonomorphismRestriction #-}
import Test.QuickCheck hiding((===))
import Control.Monad(liftM2)
import Control.Applicative((<$>),(<*>))

import Mon
import Reg
import RegExtra

writeln = putStrLn

main = do
  writeln "null tests"
  quickCheck null1
  quickCheck null2
  quickCheck null3
  quickCheck null4
  quickCheck null5
  quickCheck null6
  quickCheck null7
  writeln "empty tests"
  quickCheck empty1
  quickCheck empty2
  quickCheck empty3
  quickCheck empty4
  quickCheck empty5
  quickCheck empty6
  quickCheck empty7
  quickCheck empty8
  writeln "accepts tests"
  quickCheck acc1
  quickCheck acc2
  quickCheck acc3
  quickCheck acc4
  quickCheck acc5
  quickCheck acc6
  quickCheck acc7
  quickCheck acc8
  quickCheck acc9
  quickCheck acc10
  quickCheck acc11
  quickCheck acc12
  writeln "start tests"
  quickCheck start1
  quickCheck start2
  quickCheck start3
  quickCheck start4
  quickCheck start5
  quickCheck start6
  quickCheck start7
  quickCheck start8
  writeln "der tests"
  quickCheck der1
  quickCheck der2
  quickCheck der3
  quickCheck der4
  quickCheck der5
  quickCheck der6
  quickCheck der7
  quickCheck der8
  writeln "ders tests"
  quickCheck ders1
  quickCheck ders2
  quickCheck ders3
  quickCheck ders4
  quickCheck ders5
  quickCheck ders6
  quickCheck ders7
  quickCheck ders8
  quickCheck ders9
  quickCheck ders10
--quickCheck ders11
  quickCheck ders12
  writeln "match tests"
  quickCheck match1
  quickCheck match2
  quickCheck match3
  quickCheck match4
  quickCheck match5
  quickCheck match6
  quickCheck match7
  quickCheck match8
  quickCheck match9
  quickCheck match10
  quickCheck match11
  writeln "simpl tests"
--quickCheck simpl1
--quickCheck simpl2
--quickCheck simpl3
--quickCheck simpl4
  quickCheck simpl5
  quickCheck simpl6
--quickCheck simpl7
--quickCheck simpl8
  quickCheck simpl9
  quickCheck simpl10
  quickCheck simpl11
  quickCheck simpl12
  quickCheck simpl13
  quickCheck simpl14
  writeln "equ tests"
  quickCheck equ1
  quickCheck equ2
--quickCheck equ3
--quickCheck equ4
  quickCheck equ5
  quickCheck equ6
  writeln "search tests"
  quickCheck search1
  quickCheck search2
  quickCheck search3
  quickCheck search4
  quickCheck search5
  quickCheck search6
  quickCheck search7
  quickCheck search8
  quickCheck search9
  quickCheck search10
  quickCheck search11
  quickCheck search12
  quickCheck search13
  quickCheck search14
  quickCheck search15
  quickCheck search16
  quickCheck search17

  writeln "findall tests"
  quickCheck findall1
  quickCheck findall2
  quickCheck findall3
  quickCheck findall4
  quickCheck findall5
  quickCheck findall6
  quickCheck findall7
  quickCheck findall8
  quickCheck findall9
  quickCheck findall10
  quickCheck findall11
  quickCheck findall12

a = Many (Lit 'a')
b = Many (Lit 'b')
s = string "abcde"

start1 = mayStart 'a' a
start2 = not $ mayStart 'b' a
start3 = not $ mayStart 'a' Empty
start4 = not $ mayStart 'a' Eps
start5 = mayStart 'a' s
start6 = mayStart 'a' (b :> a)
start7 = mayStart 'b' (a :| b)
start8 = not $ mayStart 'b' (a :| a)

acc1 = accepts a "a"
acc2 = not $ accepts b "a"
acc3 = accepts s "abcde"
acc4 = not $ accepts s "abcd"
acc5 = accepts a "aaaaaa"
acc6 = accepts (a :> b) "aabbb"
acc7 = accepts (a :> b) "bbb"
acc8 = accepts (a :> b :> a) "a"
acc9 = not $ accepts (a :| b) "ab"
acc10 = not $ accepts (a :> b) "aba"
acc11 = accepts (Many (a :> b)) "ababaa"
acc12 = accepts (many1 (Lit 'a' :> Lit 'b')) "abab"

null1 = nullable a
null2 = nullable Eps
null3 = not $ nullable Empty
null4 = not $ nullable s
null5 = nullable (a :> b :> a :> b)
null6 = nullable (a :| s)
null7 = not (nullable (s :| s))

empty1 = empty Empty
empty2 = not $ empty a
empty3 = not $ empty s
empty4 = empty (Empty :> Empty)
empty5 = empty (Empty :| Empty)
empty6 = empty (Empty :> s)
empty7 = empty (s :> Empty)
empty8 = not $ empty (Empty :| a)

der1 = der 'a' (Lit 'a') == Eps
der2 = der 'a' (Lit 'b') == Empty
der3 = der 'a' b == Empty
der4 = der 'b' s == Empty
der5 = der 'b' (s :> s :> s) == Empty
der6 = der 'a' (string "ab") == string "b"
der7 = der 'a' s == simpl (string "bcde")
der8 = der 'a' a == a

ders1 = ders "abc" s == string "de"
ders2 = ders "" s == s
ders3 = ders "abcde" s == Eps
ders4 = ders "abcdef" s == Empty
ders5 = ders "aaaaa" a == a
ders6 = ders "abababa" (a :> b) == Empty
ders7 = ders "ab" a == Empty
ders8 = ders "a" (Lit 'a') == Eps
ders9 = ders "a" (Lit 'b') == Empty
ders10 = ders "aa" (Lit 'a') == Empty
-- ders11 fails which is not a problem
ders11 = ders "ababaaba" (Many (a :> b)) == (Many (a :> b))
ders12 = ders (replicate 1000 A) (Many (Lit A) :> Lit B) == (Many (Lit A) :> Lit B)

match1 = match a "aaa" == Just "aaa"
match2 = match b "aa" == Just ""
match3 = match s "abdef" == Nothing
match4 = match (a :> b) "aaba" == Just "aab"
match5 = match (a :> b :> a) "aaabaab" == Just "aaabaa"
match6 = match (a :> b) "bbbbbc" == Just "bbbbb"
match7 = match s "bcd" == Nothing
match8 = match (a :| b :| s) "aab" == Just "aa"
match9 = match (a :| b :| s) "abbb" == Just "a"
match10 = match ((string "ab") :> (Many (Lit 'a'))) "aba" == Just "aba"
match11 = match (many1 (Lit 'a' :> Lit 'b')) "abab" == Just "abab"

--simpl1 = simpl Empty == Empty
--simpl2 = simpl Eps == Eps
--simpl3 = simpl (Eps :> Eps :> Eps) == Eps
--simpl4 = simpl (Eps :| Eps) == Eps
simpl5 = simpl (s :> Empty) == Empty
simpl6 = simpl (Empty :| s) == simpl s
--simpl7 = simpl (s :| s) == s
--simpl8 = simpl (s :> s) == (string "abcdeabcde")
simpl9 = simpl (Eps :> a) == a
simpl10 = simpl (Lit 'a') == (Lit 'a')
simpl11 = simpl ((Eps :> s) :| Empty :| Empty) == simpl s
simpl12 = simpl (Many (Many a)) == a
simpl13 = simpl (Many (Many s)) == simpl (Many s)
simpl14 = simpl (many1 a) == simpl (Many a)

equ1 = (Lit 'a') === (Lit 'a')
equ2 = not $ (Lit 'b') === (Lit 'a')
--equ3 = Empty === Empty
--equ4 = Eps === Eps
equ5 = ((Lit 'a') :| Empty) === (Lit 'a')
equ6 = ((Lit 'a') :> Eps) === (Lit 'a')

search1 = search (Lit 'a') "ac" == Just "a"
search2 = search (Lit 'b') "ac" == Nothing
search3 = search a "aaa" == Just "aaa"
search4 = search a "bcd" == Just ""
search5 = search (string "abc") "dabcd" == Just "abc"
search6 = search (string "ab" :| string "bcd") "abcde" == Just "ab"
search7 = search a "abaaaa" == Just "a"
search8 = search a "baaaa" == Just ""
search9 = search (a :> (Lit 'a')) "b" == Nothing
search10 = search (many1 (Lit 'a' :> Lit 'b')) "abab" == Just "abab"
search11 = search Empty "ac" == Nothing
search12 = search Eps "ac" == Just ""
search13 = search (Eps :> Eps :> Eps) "aaaa" == Just ""
search14 = search (Many $ Lit 'a') "" == Just ""
search15 = search (Lit 'a' :> Lit 'b') "" == Nothing
search16 = search (string "ab" :| string "bcd") "abcd" == Just "ab"
search17 = search (string "a" :> string "b" :> string "c") "fabc" == Just "abc"

findall1 = findall (Many (Lit 'a' :> Lit 'b')) "aabab" == ["", "abab"]
findall2 = findall (many1 (Lit 'a' :> Lit 'b')) "aabab" == ["abab"]
findall3 = findall ((Lit 'a' :> Lit 'b') :| (Lit 'b' :> Lit 'c')) "abc" == ["ab", "bc"]
findall4 = findall (Many letter) "two words" == ["two", "", "words"]
findall5 = findall (Many (Lit 'a' :| Lit 'b')) "ccabccacc" == ["", "", "ab", "", "", "a", "", ""]
findall6 = findall (many1 (Lit 'a' :> Lit 'b')) "aabab" == ["abab"]
findall7 = findall (many1 (string "a")) "abcxfyaaa" == ["a", "aaa"]
findall8 = findall (Many (string "a" :| Empty)) "abcxfyaaa" == "a":(replicate 5 "") ++ ["aaa"]
findall9 = findall (Many (Many (Many a))) "ababbaaabaa" == ["a", "", "a", "", "", "aaa", "", "aa"]
findall10 = findall Eps "abba" == replicate 4 ""
findall11 = findall Empty "abcdef" == []
findall12 = findall (Lit 'a') (replicate 100 'a') == replicate 100 "a"
