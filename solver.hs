
import Data.SBV
import System.Environment (getArgs)
import Data.List (transpose, reverse, intersperse)
import Data.Maybe (isJust, fromJust)
import Data.Function (on)
import Data.List (sortBy)
import Control.Monad (forM_, forM)
import Control.Concurrent.Async (Async, waitAnyCancel, async)
import System.Random (randomRIO)
import GHC.Conc (numCapabilities)

data Args = Args {
    argH :: Int,
    argW :: Int,
    argBlock :: String
    } deriving Show

type BlockName = Char
type BlockShape = [String]

data Block = Block {blkName :: BlockName
                  , blkShape :: BlockShape
             } deriving Show

type SymUnit = SWord8
type SymPos = [SymUnit]

type SymBlock = [SymPos]
type Board = [SymBlock]
    
allBlocks :: [Block]
allBlocks = [Block 'I' ["xxxx"] 

           , Block 'O' ["xx",
                        "xx"] 

           , Block 'T' ["xxx",
                        " x "] 

           , Block 'S' [" xx",
                        "xx "] 

           , Block 'Z' ["xx ",
                        " xx"] 
         
           , Block 'J' [" x",
                        " x",
                        "xx"] 
          
           , Block 'L' ["x ",
                        "x ",
                        "xx"] 
        ] 
-- Get rotational symmetry
nRot :: Block -> Int
nRot b = length $  nRot' bs bs
    where bs = blkShape b
          nRot' bs nbs 
                |rbs == bs = [nbs]
                |otherwise = nbs : nRot' bs rbs
                where rbs = rot90 nbs
                          

numTile = length . (filter (/= ' ')) . concat 

getBlock :: BlockName -> Block
getBlock x = head $ filter ((== x) . blkName) allBlocks

rot90 = transpose . reverse

toList :: (a, a) -> [a]
toList (a, b) = [a,b]

eastOf :: SymPos -> SymPos -> SBool
eastOf [y1 , x1] [y2 , x2] = (x1 - x2 .== 1) &&& (y1 .== y2)

southOf :: SymPos -> SymPos -> SBool
southOf [y1 , x1] [y2 , x2] = (y1 - y2 .== 1) &&& (x1 .== x2)

shape :: [[a]] -> (Int, Int)
shape a = (length a, length $ head a)

reshape :: (Int, Int) -> [a] -> [[a]]
reshape (y, x) [] = []
reshape (0, x) xs = [xs]
reshape (y, x) xs = take x xs : reshape (y-1, x) (drop x xs)

varReplace :: [a] -> BlockShape -> [[Maybe [a]]]
varReplace xs bs = reshape (shape bs) $ varReplace' xs (concat bs)
    where varReplace' _ [] = []
          varReplace' xs (' ':ys) = Nothing:(varReplace' xs ys)
          varReplace' (y:x:xs) (_:ys) = (Just [y,x]):(varReplace' xs ys)
            
connLine :: (a -> a -> SBool) -> [Maybe a] -> SBool
connLine f l = bAnd $ connLine' f l
    where 
    connLine' f [] = []
    connLine' f (p:[]) = []
    connLine' f (p:Nothing:ps) = connLine' f ps
    connLine' f (Nothing:p:ps) = connLine' f (p:ps)
    connLine' f ((Just p1):(Just p2):ps) = f p1 p2 : connLine' f ((Just p2):ps)

localConstraint :: [[Maybe SymPos]] -> SBool
localConstraint sb = bAnd (map (connLine eastOf) sb)
     &&& bAnd (map (connLine southOf) $ transpose sb)

connBlock :: [SymUnit] -> Block -> SBool
connBlock xs b =  bOr $ map localConstraint allRot
    where 
        allRot = take (nRot b) $ iterate rot90 sb
        sb = varReplace xs (blkShape b) 


isValid :: Args -> [SymUnit] -> SBool
isValid args xs = blockConstraints xs bs 
              &&& rangeConstraints xs
              &&& diffConstraints xs
    where 
        w = fromIntegral $ argW args
        h = fromIntegral $ argH args
        bs = map getBlock (argBlock args)
        blockConstraints _ [] = literal True
        blockConstraints xs (b:bs) = connBlock bx b &&& blockConstraints xs' bs
            where nvar = 2 * numTile (blkShape b)
                  (bx, xs') = splitAt nvar xs
        numXs [] = []
        numXs (y:x:xs) = (y * w + x) : numXs xs
        diffConstraints = allDifferent . numXs
        rangeConstraints [] = literal True
        rangeConstraints (y:x:xs) = y .< h 
                                &&& x .< w 
                                &&& rangeConstraints xs

parseArgs :: [String] -> Args
parseArgs (h:w:blocks:_) = Args (read h) (read w) blocks



dispRes ::  Args -> Maybe [Word8] -> IO()
dispRes args Nothing = putStrLn $ "Unsolvable: " ++ show args
dispRes args (Just vs) = dispSoln args vs

dispSoln :: Args -> [Word8] -> IO()
dispSoln args vs = forM_ (fmtRes vs) putStrLn 
    where 
        nblocks = map (numTile . blkShape . getBlock) (argBlock args)
        poss vs = reshape (length vs, 2) vs
        numBlocks = concat [replicate n i | (n, i) <- zip nblocks ['a'..]]
        orderedNums vs = snd $ unzip $ sortBy (compare `on` fst) $ zip (poss vs) numBlocks 
        shapedNums vs = reshape (argH args, argW args) (orderedNums vs)
        fmtRes vs = map (intersperse ' ') (shapedNums vs)

solveBoard :: Args -> IO SatResult
solveBoard args = sat (isValid args <$> mkExistVars n)
    where n = (argW args) * (argH args) * 2 

pSolveBoard :: [Args] -> IO SatResult
pSolveBoard argss = do
    asyncArgs <- mapM (async . solveBoard) argss
    res <- waitAnyCancel asyncArgs
    return (snd res)

shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x else do
    i <- randomRIO (0, length x -1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)



main = do
    args <- parseArgs <$> getArgs
    let j = numCapabilities
        s = argBlock args
    blocks <- forM [1..j] $ \x -> shuffle s
    let pargs = map (\x -> args {argBlock = x}) blocks
    res <- extractModel <$> pSolveBoard pargs
    dispRes args res
