module PageRank where

import Test.QuickCheck
import Data.List(transpose)
import Control.Monad
import Data.IORef
import System.IO.Unsafe

data Page = Page String [Link] -- Data type Page with a pagename and a 
                               -- weighted list of links to other pages.
type Link = (Double, Page) -- A link is synonymous to a weight and a page.
type Web = [Page] -- A Web is synonymous to a list of Pages.

-- Let Haskell know how to print a webpage (just the name)
instance Show Page where
    show (Page name _) = name

-- | Helper function: returns only the links of a page
links :: Page -> [Link]
links (Page _ links) = links

-- | Helper function: returns only the weight of a link
weight :: Link -> Double
weight (w, _) = w

-- | Helper function: returns only the page of a link
page :: Link -> Page
page (_, p) = p

-- | Helper function to generate a list of pages with weights that work
-- well for Haskell.
genPages :: [Link] -> [(Int, Gen Page)]
genPages links = map reWeight links
    where 
        reWeight :: Link -> (Int, Gen Page)
        reWeight (w,l) = (round (fromIntegral (length links) * w), 
                          elements [l])

generateNames :: Int -> [String]
generateNames n = zipWith (\x y -> x ++ (show y)) (replicate n "Page ") [1..n]

-- | Given a page with links, returns one of the linked pages
-- based on the distribution of probabilities for each link.
browsePage :: Page -> IO Page
browsePage (Page _ ls) = do
    page <- generate $ frequency $ genPages ls
    return ((Page (show page) (links page)))

-- | Returns the adjacency matrix of a web of linked pages
getAdjMatrix :: Web -> [[Int]]
getAdjMatrix w  = getAdjMatrix' w w -- call helper function

-- | Helper function with a reference to the entire web to
-- compare links to
getAdjMatrix' :: Web -> Web -> [[Int]]
getAdjMatrix' _ []       = []
getAdjMatrix' web (p:ps) = [[(isLinked w p) | w <- web]] ++ getAdjMatrix' web ps
    where
        isLinked p (Page _ [])                              = 0
        isLinked p (Page name (l:ls))
          | ((show p) == (show $ page l) &&
            weight l > 0) || isLinked p (Page name ls) == 1 = 1
          | otherwise                                       = 0 

-- | Get distribution of link-weights based on number of links in 
-- input adjacency matrix.
getRankMatrix :: Double -> Web -> [[Double]]
getRankMatrix lambda web = let rows = getAdjMatrix web in
    case lambda of
        0         -> getRankMatrix' rows
        otherwise -> map (getWeights lambda (length web)) rows

-- | Helper function, used when lambda is 0
getRankMatrix' :: [[Int]] -> [[Double]]
getRankMatrix' []         = []
getRankMatrix' (row:rows) = [map (\x -> div' x (length $ filter (>0) row)) row]
                           ++ getRankMatrix' rows

-- | Helper function, distributes weights for each link if lambda>0
getWeights :: Double -> Int -> [Int] -> [Double]
getWeights lambda n row = 
    map (setWeight lambda n (length $ filter (>0) row)) row
        where
            setWeight :: Double -> Int -> Int -> Int -> Double
            setWeight l n ni x
                | x > 0     = (1-l)/(fromIntegral ni) + l/(fromIntegral n)
                | otherwise = l / (fromIntegral n) 

-- | Returns a Web from a list of page-names and a PageRank matrix
getWeb :: [String] -> [[Double]] -> Web
getWeb s m = getWeb' s s m

getWeb' :: [String] -> [String] -> [[Double]] -> Web
getWeb' s [] _               = []
getWeb' s _ []               = []
getWeb' s (n:ns) (row:rows)  = [(Page n (zip row (map (\s -> (Page s [])) s)))]
                               ++ getWeb' s ns rows

getAdjWeb :: [String] -> [[Int]] -> Web
getAdjWeb s adj = getAdjWeb' s s adj

getAdjWeb' :: [String] -> [String] -> [[Int]] -> Web
getAdjWeb' s [] _               = []
getAdjWeb' s _ []               = []
getAdjWeb' s (n:ns) (row:rows)  = [(Page n (zip (map (fromIntegral) row) (map (\s -> (Page s [])) s)))]
                                  ++ getAdjWeb' s ns rows

-- | Convenient for integer-division
div' :: Int -> Int -> Double
div' x y = (fromIntegral x) / (fromIntegral y)

-- | Given a PageRank matrix and a probability-vector,
-- returns a new probability-vector
rowByMatrix :: Num a => [[a]] -> [a] -> [a]
rowByMatrix [] v = []
rowByMatrix m v = let (col:cols) = transpose m in
    foldr (+) 0 (zipWith (*) v col) : rowByMatrix (transpose cols) v

-- | Returns probability-vector of n steps through a given web, 
-- with initial probability vector v
surfN :: Int -> Web -> [Double] -> Double -> [Double]
surfN n w v lambda = (iterate (rowByMatrix (getRankMatrix lambda w)) v)!!n

-- | Returns the n long trace of a random walk through a web of pages, 
-- given a starting page with links
surfHistory :: Int -> Page -> IO [Page]
surfHistory n p = surfHistory' n p []

surfHistory' :: Int -> Page -> [Page] -> IO [Page]
surfHistory' n p ps = do
    case compare (length ps) n of
        GT  -> return (reverse ps)
        EQ  -> return (reverse ps)
        LT  -> do
            p' <- browsePage p
            let ps' = p':ps
            surfHistory' n p' ps' 

-- | Returns equilibrium vector of a web and a given damping, if its 
-- PageRank matrix is regular. Throws error otherwise
getRanks :: Double -> Web -> [Double]
getRanks lambda w = getRanks' lambda w 100

-- | Numerically determine equilibrium vector for a web,
-- with a given lambda. Error if not found within 300 powers of matrix
getRanks' :: Double -> Web -> Int -> [Double]
getRanks' lambda w n
    | n > 300   = error "No equilibrium for first 300 powers"
    | otherwise = let v = surfN n w (1:(replicate ((length w)-1) 0)) lambda
                      v' = surfN (n+1) w v lambda in
        case compare v v' of
            EQ -> v
            LT -> getRanks' lambda w (n+1)
            GT -> getRanks' lambda w (n+1)
