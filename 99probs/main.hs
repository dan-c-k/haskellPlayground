import Data.Function
import Data.List

-- Problem 46

type Mybool = Bool

nand2 :: Mybool -> Mybool -> Mybool
nand2 True True = False
nand2 _ _ = True

not1 :: Mybool -> Mybool
not1 b = nand2 b b

and2 :: Mybool -> Mybool -> Mybool
and2 a b = not1 $ nand2 a b

or2 :: Mybool -> Mybool -> Mybool
or2 a b = (nand2 (nand2 a a) (nand2 b b))

nor2 :: Mybool -> Mybool -> Mybool
nor2 a b = not1 $ or2 a b

xor2 :: Mybool -> Mybool -> Mybool
xor2 a b = (or2 (and2 a (not1 b)) (and2 (not1 a) b))

xnor2 :: Mybool -> Mybool -> Mybool
xnor2 a b = not1 $ xor2 a b

table :: (Mybool -> Mybool -> Mybool) -> IO ()
table fAB = mapM_ print lOfBools
  where 
    bools = [True,False] :: [Mybool]
    lOfBools = [[a ,b ,(fAB a b)] | a <- bools, b <- bools ]



-- Problem 47


-- Problem 48
-- tableN :: [Mybool] -> ?? -> IO ()
-- tableN l ?? = mapM_ print lOfBools
--   where
--     bools = [True,False] :: [Mybool]
--     lOfBools = [| b <- l]

-- Problem 49

-- gray :: Int -> [String]
flatten :: [[String]] -> [String]
flatten [] = []
flatten (chars:charss) = chars ++ flatten charss

gray 1 = [['0'], ['1']]
gray n =   flatten [['0':g,'1':g] | g <- (gray (n -1 ))]















-- Problem 50
data HuffmanTree =  Node HuffmanTree HuffmanTree | Leaf Char
type Encoding = (Char,Int)

walkTree :: HuffmanTree -> [(Char,String)]
walkTree (Node n1 n2) = (helper "" n1) ++ (helper "" n2)
  where 
    helper acc (Leaf c) = (c,acc)
    helper acc (Node n3 n4) = (helper (acc++"0") n3) ++ (helper (acc++"1") n4)

buildTree :: [Encoding] -> HuffmanTree
buildTree encds = (buildTree (newEncd:init (init sortedEncds)) (Leaf (fst fstEncd)) (Leaf (fst sndEncd)))
  where
    sortedEncds = sortBy (compare `on` abs . snd) encds
    fstEncd = head sortedEncds
    sndEncd = head $ tail sortedEncds
    newEncd = ('*',(snd fstEncd) + (snd sndEncd))

huffman :: [Encoding] -> [(Char,String)]
huffman encds = walkTree myTree
  where myTree = buildTree encds

-- encodingSort = sortBy myPredicate 

-- huffman :: [Encoding]
-- huffman ends = ...
--    where 






main = do
    putStrLn "In Main"