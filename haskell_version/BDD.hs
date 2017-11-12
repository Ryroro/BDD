import Data.Char
import Data.Maybe
import Data.List

type VarId = Int
type NodeId = Int
type Attributes = (VarId, NodeId, NodeId)
type HTable = [(Attributes, NodeId)]
type TTable = [(NodeId, Attributes)]

type Tokens = [String]

data Formula = Or Formula Formula
             | And Formula Formula
             | Imply Formula Formula
             | Iff Formula Formula
             | Not Formula
             | Atom String
             deriving Show

type Env = [(String, Bool)]

operations :: [String]
operations = ["and", "or", "iff", "not","imply"]


--
-- Utility
--
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp a abs = fromJust $ lookup a abs

add :: (a, b) -> [(a, b)] -> [(a, b)]
add a abs = a : abs


countAtom :: Formula -> Int
countAtom f = case f of
                Atom x -> 1
                Or f1 f2 -> countAtom f1 + countAtom f2
                And f1 f2 -> countAtom f1 + countAtom f2
                Imply f1 f2 -> countAtom f1 + countAtom f2
                Iff f1 f2 -> countAtom f1 + countAtom f2
                Not f1 -> countAtom f1

-- Get the string representation of all the atom
getAtoms :: Formula -> [String]
getAtoms f = nub $ getAtoms' f 
  where getAtoms' f  =
          case f of
            Atom x -> [x] 
            Or f1 f2 -> getAtoms f1 ++ getAtoms f2
            And f1 f2 -> getAtoms f1 ++ getAtoms f2
            Imply f1 f2 -> getAtoms f1 ++ getAtoms f2
            Iff f1 f2 -> getAtoms f1 ++ getAtoms f2
            Not f1 -> getAtoms f1

toBase :: Int -> Int -> Int -> [Int]
toBase n b c
  | c == 0 = []
  | otherwise =  n `mod` b : toBase (n `div` b) b (c-1)

allPoss :: Int -> [[Int]]
allPoss n =  init [toBase x 2 n | x <- [0..2^n]]


imply :: Bool -> Bool -> Bool
imply b1 b2
  | b1 == True && b2 == False = False
  | otherwise = True

iff :: Bool -> Bool -> Bool
iff b1 b2
  | b1 == b2 = True
  | otherwise = False


--
-- Transformation of input into Formula
--

buildNodeId :: [String] -> [(String, Int)]
buildNodeId s = zipWith (\a i -> (a, i)) s [0..]

reformat :: String -> Tokens
reformat input = (words . addSpaceBracket) input 
  where addSpaceBracket :: String -> String
        addSpaceBracket [] = []
        addSpaceBracket (x:xs)
          | x == '(' = ' ': x : ' ' : addSpaceBracket xs
          | x== ')' = ' ' : x : ' ' : addSpaceBracket xs 
          | otherwise = x : addSpaceBracket xs


transform :: String -> Formula
transform line = transform' tokens [] []
  where tokens = reformat line
        transform' [] fStack opStack =
          case null opStack of
            True -> head fStack
            False -> case (head opStack) == "not" of
                       True -> transform' [] ((Not (head fStack)) : (drop 1 fStack)) (drop 1 opStack)
                       False -> transform' [] ((opForm (head opStack) (fStack !! 1) (fStack !! 0)): (drop 2 fStack)) (drop 1 opStack)
        transform' input@(t:ts) fStack opStack 
          | t == "(" = transform' ts fStack opStack
          | t == ")" = 
              case (head opStack) == "not" of
                True -> transform' ts ((Not (head fStack)):(drop 1 fStack)) (drop 1 opStack)
                False -> transform' ts ((opForm (head opStack) (fStack !! 1) (fStack !! 0)) : (drop 2 fStack)) (drop 1 opStack)
          | t `elem` operations = transform' ts fStack (t:opStack)
          | otherwise = transform' ts ((Atom t):fStack) opStack

opForm :: String -> Formula -> Formula -> Formula
opForm op f1 f2
  | op == "and" = And f1 f2
  | op == "or" = Or f1 f2
  | op == "iff" = Iff f1 f2
  | op == "imply" = Imply f1 f2

eval :: Formula -> Env -> Bool
eval f env = eval' f
  where eval' f
          = case f of
              Or f1 f2    -> eval' f1 || eval' f2
              And f1 f2   -> eval' f1 && eval' f2
              Imply f1 f2 -> imply (eval' f1) (eval' f2)
              Iff f1 f2   -> iff (eval' f1) (eval' f2)
              Not f1      -> not $ eval' f1
              Atom s      -> lookUp s env


getAllPoss :: [String] -> [[(String, Bool)]]
getAllPoss atoms = [ zipWith (\s b -> if b == 1
                                      then (s, True)
                                      else (s, False))
                                      atoms p | p <- allPoss']
  where n = length atoms
        allPoss' = allPoss n


buildHTable :: Formula -> HTable
buildHTable f = buildHTable' f atoms 2 [] 
  where atoms = getAtoms f
        nodeId = buildNodeId atoms
        
        buildHTable' f [a] n env
          = [((n, l, h), lookUp a nodeId)]
          where l = case eval f ((a, False):env) of
                      True -> 1
                      False -> 0
                h = case eval f ((a, True):env) of
                      True -> 1
                      False -> 0
      
        buildHTable' f (a:as) n env
          = [((n, 2*n, (2*n+1)), lookUp a nodeId)] ++
            buildHTable' f as (2*n) ((a, False):env) ++  
            buildHTable' f as (2*n+1) ((a, True):env)
         

-- simply iterate through all the possibilites
simpleEval :: Formula -> [Env]
simpleEval f = [ e | e <- poss, eval f e]
  where atoms = getAtoms f
        poss = getAllPoss atoms

-- simply iterate through all the routes to truth 
satCount1 :: HTable -> Int
satCount1 

--
-- Test Cases
--

input1 = "a and b"
input2 = "(a and b) or c"
input3 = "(c or (b and d)) imply (not d)"
input4 = "not (a and b)"
input5 = "((not (a or c)) or (d and b)) imply d"
  
f1 = transform input1
f2 = transform input2
f3 = transform input3
f4 = transform input4
f5 = transform input5
