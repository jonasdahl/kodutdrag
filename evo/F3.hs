import F2
import Data.List.Split
import System.Exit
import System.IO

-- The tree data type can either be a 2-branched branch, a 3-branched branch or a leav
data Tree a = Leaf String a | Branch3 String (Tree a) (Tree a) (Tree a) | Branch2 String (Tree a) (Tree a) deriving (Eq);

-- Declares show functions that just displays the name of the part
instance Show (Tree a) where
  show (Leaf name _) = show name
  show (Branch2 name _ _) = show name
  show (Branch3 name _ _ _) = show name

-- Main function - run to start program
main :: IO ()
main = do
	(di, fi) <- readLines ([],[])
	putStrLn (formatTree (nj fi (distanceMatrix di)))

-- Fills given list with user input
readLines :: ([MolSeq], [Tree MolSeq]) -> IO ([MolSeq], [Tree MolSeq])
readLines lines = do 
	end <- isEOF
	if end then 
		return lines
	else do row <- getLine
		readLines ((fst (decodeLine row) : (fst lines)), (snd (decodeLine row) : (snd lines)))

-- Calculates S-value for given x and y values
s :: Tree MolSeq -> Tree MolSeq -> [Tree MolSeq] -> [(String, String, Double)] -> Double
s x y fi di = 
	(fromIntegral (length fi - 2)) * (pointDistance x y di) - (sumUp x di 0) - (sumUp y di 0)

-- Sums up for all elements matching x in di
sumUp :: Tree MolSeq -> [(String, String, Double)] -> Double -> Double
sumUp x [] dsum = dsum
sumUp x ((s1, s2, d):tdi) dsum = 
	if treeName x == s1 || treeName x == s2 then 
		(sumUp x tdi (dsum + d))
    else 
    	(sumUp x tdi dsum)

-- Minimizes S - returns a tuple with the trees' names and their distance
minimizeS :: [Tree MolSeq] -> [(String, String, Double)] -> (Tree MolSeq, Tree MolSeq, Double)
minimizeS (r:fi) di = minLast (sValues r fi fi (r:fi) di)
	where
        minLast ((a,b,c):xs) = 
        	if xs == [] then (a,b,c)
        	else
        		if c < z then (a,b,c)
        		else (x,y,z)
        	where 
        		(x,y,z) = minLast xs

-- Creates the S table
sValues :: Tree MolSeq -> [Tree MolSeq] -> [Tree MolSeq] -> [Tree MolSeq] -> [(String, String, Double)] -> [(Tree MolSeq, Tree MolSeq, Double)]
sValues a (b:x) z fa di = (a, b, (s a b fa di)) : (sValues a x z fa di)
sValues a [] (d:z) fa di = sValues d z z fa di
sValues _ [] [] _ _ = []

-- Executes the NJ algorithm
nj :: [Tree MolSeq] -> [(String, String, Double)] -> Tree MolSeq
nj fi di 
	| length fi > 3 =
		nj (t:f2) d2
	| length fi == 3 = 
		(Branch3 "Main" (fi!!0) (fi!!1) (fi!!2))
	| otherwise = (Leaf "Empty" (MolSeq DNA "" ""))
	where
		(a, b, _) = minimizeS fi di -- Hitta (a,b) som minimerar s
		t = Branch2 ((treeName a) ++ (treeName b)) a b
		f2 = filter (\x -> ((treeName x) /= (treeName a) && (treeName x) /= (treeName b))) fi  -- Tar bort a och b från fi
		d2 = createNewD di a b t fi di

-- Creates the new D matrix out of old one
createNewD :: [(String, String, Double)] -> Tree MolSeq -> Tree MolSeq -> Tree MolSeq -> [Tree MolSeq] -> [(String, String, Double)] -> [(String, String, Double)]
createNewD ((x,y,z):oldD) a b t fi di = 
	if x == (treeName a) || y == (treeName a) || x == (treeName b) || y == (treeName b) then
		createNewD oldD a b t fi di   	-- Skip this one, we want to delete it
	else
		(x,y,z) : (createNewD oldD a b t fi di)
createNewD [] a b t (f:fi) di = 
	if (treeName a) == (treeName f) || (treeName b) == (treeName f) then
		(createNewD [] a b t fi di)
	else
		((treeName t), (treeName f), (distanceTree a b f di)) : (createNewD [] a b t fi di)
createNewD [] _ _ t [] _ = [(treeName t, treeName t, 0)]

-- Calculates the new distance between tree A-B and F
distanceTree :: Tree MolSeq -> Tree MolSeq -> Tree MolSeq -> [(String, String, Double)] -> Double
distanceTree a b f di = ((pointDistance a f di) + pointDistance b f di) / 2

-- Tar reda på avståndet mellan två träd som finns i en lista
pointDistance :: Tree MolSeq -> Tree MolSeq -> [(String, String, Double)] -> Double
pointDistance x y ((a,b,c):di) =
	if (a == (treeName x) && b == (treeName y)) || (a == (treeName y) && b == (treeName x)) then 
		c
	else 
		pointDistance x y di
pointDistance x y [] = error ("Avståndet saknas i listan.")

-- Other useful stuff
-- Turns tree into nice tuples for presentation
formatTree :: Tree MolSeq -> String
formatTree (Leaf n (MolSeq _ name seq)) = n
formatTree (Branch3 n a b c) = '(' : formatTree a ++ (',' : formatTree b) ++ (',' : formatTree c) ++ ")"
formatTree (Branch2 n a b) = '(' : formatTree a ++ (',' : formatTree b) ++ ")"

-- Decodes the line "NAME SEQUENCE" to a (molseq, tree molseq)
decodeLine :: String -> (MolSeq, Tree MolSeq)
decodeLine line =
	if length x /= 2
		then error "Felaktig inmatning"
		else ((string2seq name seq), (Leaf name (string2seq name seq)))
	where
		x = splitOn " " line
		name = x !! 0
		seq = x !! 1

-- Gets name of tree, so we don't need to look through whole tree to compare them
treeName :: Tree MolSeq -> String
treeName (Leaf name _) = name
treeName (Branch2 name _ _) = name
treeName (Branch3 name _ _ _) = name

-- Turns list of leafs into list of the leafs' data
deleaf :: [Tree MolSeq] -> [MolSeq]
deleaf ((Leaf _ ms):xs) = ms : (deleaf xs)
deleaf [] = []