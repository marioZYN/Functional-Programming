-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   marioZhou
-- @Last Modified time: 2018-01-01 11:12:18

 
-- 1.1) Define a data structure, called Lt, for generic list of lists, where each list has a fixed length and such number is stored in the data structure.
-- 1.2) Define a function, called checkLt, that takes an Lt and returns true if it is valid (i.e. every list in it have the correct size), false otherwise.
-- 1.3) Define a function, called checklist, that takes a list t and an Lt, checks if all the sublists of t are in the given Lt, and uses Maybe to return the list of sublists of t that are not present in Lt.
-- Note: sublists must be contiguous, e.g. the sublists of size 2 of [1,2,3] are [1,2], [2,3].
-- 1.4) Make Lt an instance of Functor.
-- Note: state all the types of the defined functions.

data Lt a = Lt Int [[a]]

checkLt :: Lt a -> Bool
checkLt (Lt size []) = True
checkLt(Lt size (x:xs)) = (length x == size) && (checklist xs)

sublists :: Int -> [a] -> [[a]] -> [[a]]
sublists size lst res = 
	if length lst < size
	then res
	else sublists size (tail lst) (take size lst):res

checklist :: Eq a => [a] -> Lt a -> Maybe [[a]]
checklist lst (Lt size lol) = 
	let factors = sublists size lst []
		nfactors = [x | x <- factors, not (x `elem` lol)]
	in if nfactors == []
	   then Nothing
	   else Just nfactors