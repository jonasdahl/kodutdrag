module F2 where
import Data.List
 
-- Typer - "alias"
type Name = String
type Distance = Double
 
-- Definierar DNA elle Protein
data DNAProtein = DNA | Protein deriving (Show, Eq)

-- Definierar en sekvens av antingen DNA eller Protein
data MolSeq =  MolSeq  { molType :: DNAProtein
                       , molName :: String
                       , seq :: String
                       } deriving (Show, Eq)
 
 -- Definierar en profilmatris
data Profile = Profile { moleType :: DNAProtein
                       , numSeq :: Int
                       , matrix :: [[(Char, Int)]]
                       , profName :: String
                       } deriving (Show, Eq)
 
-- Typklass Evol samlar alla saker som kan jämföra avstånd
class Evol a where
    distance :: a -> a -> Distance
    name :: a -> Name 
    distanceMatrix :: [a] -> [(Name, Name, Distance)]
    distanceMatrix [] = []
    distanceMatrix x = distances (head x) x x (tail x)
        where
            distances a (b:x) y z = ((name a), (name b), (distance a b)) : distances a x y z
            distances a [] (c:y) (d:z) = distances d (d:z) (c:y) z
            distances _ [] _ [] = []
 
-- Instanser av Evol för de två datatyperna
instance Evol MolSeq where  
        distance a b = seqDistance a b
        name a = seqName a
 
instance Evol Profile where  
        distance a b = profileDistance a b
        name a = profileName a
 
-- Funktion som returnerar typen av sekvensen
seqType :: MolSeq -> DNAProtein
seqType (MolSeq typ _ _) = typ
 
-- Funktion som returnerar namnet av molekylen
seqName :: MolSeq -> String
seqName (MolSeq _ name _) = name

-- Funktion som returnerar sekvensen
seqSequence :: MolSeq -> String
seqSequence (MolSeq _ _ seq) = seq
 
-- Funktion som returnerar längden på en sekvens
seqLength :: MolSeq -> Int
seqLength (MolSeq _ _ seq) = length seq

-- Funktion som returnerar profilens namn
profileName :: Profile -> String
profileName (Profile _ _ _ name) = name
 
-- Funktion som skapar en MolSeq utifrån given sekvens
string2seq :: String -> String -> MolSeq
string2seq name seq = 
    if isProtein seq
        then (MolSeq Protein name seq)
        else (MolSeq DNA name seq)
 
-- Funktion som returnerar om given sträng är Protein eller DNA
isProtein :: String -> Bool
isProtein (a:seq) = 
    if not $ elem a "ACGT" 
        then True 
        else isProtein seq
isProtein s = False
 
-- Funktion som returnerar frekvensen av förekomsten av c i den givna profilens i:te teckenplats
profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency (Profile _ n m _) i c = fromIntegral (findEntry (m!!i) c) / fromIntegral n
    where
        findEntry (x:ls) c = if ((fst x) == c) then snd x else findEntry ls c
        findEntry [] c = 0
 
-- Funktion som räknar ut antalet skillnader mellan två sekvenser
difference :: [Char] -> [Char] -> Int -> Int
difference (a:seq1) (b:seq2) n = difference seq1 seq2 (if a == b then n else (n+1))
difference [] b n = n + length b
difference a [] n = n + length a
 
-- Jämför två MolSeq och returnerar avståndet
seqDistance :: MolSeq -> MolSeq -> Double
seqDistance (MolSeq DNA name1 seq1) (MolSeq DNA name2 seq2)
    | a > 0.74 = 3.3
    | otherwise = -(3/4) * log (1 - a * 4 / 3)
    where 
        a = (realToFrac (difference seq1 seq2 0)) / realToFrac (max (length seq1) (length seq2))
 
seqDistance (MolSeq Protein name1 seq1) (MolSeq Protein name2 seq2)
    | a >= 0.94 = 3.7
    | otherwise = -(19/20) * log (1 - a * 20 / 19)
    where 
        a = (realToFrac (difference seq1 seq2 0)) / realToFrac (max (length seq1) (length seq2))
seqDistance _ _ = error "DNA och Protein kan inte matchas."

nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYVX"

-- Gör en profilmatris av en lista med molseqs
makeProfileMatrix :: [MolSeq] -> [[(Char, Int)]]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
    where
        t = seqType (head sl)
        a = fromIntegral (length sl)
        defaults =
            if (t == DNA) then
                zip nucleotides (replicate (length nucleotides) 0) -- Rad (i) returnar listan [(A, 0), (C, 0), (G, 0) osv]
            else
                zip aminoacids (replicate (length aminoacids) 0) -- Rad (ii) returnar listan [(A, 0), osv]
        strs = map seqSequence sl -- Rad (iii) returnar en lista med alla sekvenser som sl-listan representerar - "deobjektiserar" 
        tmp1 = map (map (\x -> (head x, length x)) . group . sort) (transpose strs) -- Rad (iv) - gör en lambdafunktion som tar 
        -- en sorterad och grupperad (tar alla efterföljande likadana tecken och gör till en dellista) parameter-lista x, 
        -- och gör en tvåtupel av första elementet och längden på listan. Detta görs sedan på en transponerad strs-lista i två
        -- dimensioner, på grund av det sista map-anropet längst ut.
        equalFst a b = (fst a) == (fst b) -- Är sann om första värdet i tuplerna är samma
        res = map sort (map (\l -> unionBy equalFst l defaults) tmp1) 
 
-- Tar namn och en lista med MolSeqs och skapar en profil
molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile name x = (Profile (seqType (head x)) (length x) (makeProfileMatrix x) name)
 
-- Mäter avståndet mellan två profiler
profileDistance :: Profile -> Profile -> Double
profileDistance (Profile _ na a _) (Profile _ nb b _) = rows a b
    where 
        rows (a:ax) (b:bx) = (cols a b) + rows ax bx
        rows _ _ = 0
        cols (c:cx) (d:dx) = (abs (fromIntegral (snd c) / qa - fromIntegral (snd d) / qb)) + cols cx dx
        cols _ _ = 0
        qa = fromIntegral (na)
        qb = fromIntegral (nb) 