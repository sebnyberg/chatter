module Pattern where
import Utilities


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute w (x:xs) s 
	| x == w = s ++ substitute w xs s
	| otherwise = x : substitute w xs s


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ _ [] = Nothing
match _ [] _ = Nothing
match w xx@(x:xs) yy@(y:ys) 
	| x == w = orElse (singleWildcardMatch xx yy) (longerWildcardMatch xx yy)
	| x == y = match w xs ys
	| otherwise = Nothing


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mand (Just [x]) (match wc ps xs) 
longerWildcardMatch (wc:ps) (x:xs) = mmap ((:) x) (match wc (wc:ps) xs)


mand :: Eq a => Maybe [a] -> Maybe [a] -> Maybe [a]
mand _ Nothing = Nothing
mand Nothing _ = Nothing
mand (Just x) _ = Just x


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions


-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply w f s t = mmap (substitute w (snd t)) (match w (fst t) (f s))


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing -- shuld not happend, last pattern is "*"
transformationsApply w f (t:ts) s = orElse (transformationApply w f s t) (transformationsApply w f ts s)

