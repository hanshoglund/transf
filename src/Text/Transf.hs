
{-# LANGUAGE
    GeneralizedNewtypeDeriving 
    #-}

module Text.Transf (
        -- * Core
  ) where

import Prelude hiding (mapM, readFile, writeFile)         
import Data.Traversable (mapM)
import Control.Exception
import Control.Monad.Error hiding (mapM)
import Control.Monad.Plus hiding (mapM)
import qualified Prelude as Prelude
import qualified Data.List as List

-- | A line of text.
type Line = String

-- | A relative file path.
type RelativePath = FilePath

-- | 
newtype Transf a = Transf { getTransf :: 
    ErrorT 
        String 
        IO a 
    }
    deriving (Monad, MonadPlus, MonadError String, MonadIO)

runTransf :: Transf a -> IO a
runTransf = liftM fromRight . runErrorT . getTransf

fromRight (Right a) = a


-- | Read a file.
readFile :: RelativePath -> Transf String
readFile path = do
    input <- liftIO $ try $ Prelude.readFile path
    case input of
        Left e  -> throwError $ "readFile: " ++ show (e::SomeException)
        Right a -> return a

-- appendFile   :: RelativePath -> String -> Transf ()

writeFile :: RelativePath -> String -> Transf ()
writeFile path str = do
    liftIO $ Prelude.writeFile path str


data Transformation = Transformation {
        guard     :: (Line -> Bool, Line -> Bool),
        function  :: (Line -> Transf Line)
    }

censor :: Transformation
censor = Transformation ((== "<<"), (== ">>")) (return . const "(censored)")

main = runTransf $ do
    input <- liftIO $ Prelude.readFile "in.md"  
    output <- runTransformation censor input
    liftIO $ Prelude.writeFile "out.md" output

-- foo :: String -> Transf String
-- foo = return

runTransformation :: Transformation -> String -> Transf String
runTransformation (Transformation (start,stop) f) as = do
    let bs = (sections start stop . lines $ as) :: [([Line], Maybe [Line])]
    let cs = fmap (first unlines . second (fmap unlines)) $ bs :: [(String, Maybe String)]
    ds <- mapM (secondM (mapM f)) $ cs :: Transf [(String, Maybe String)]
    return $ concatMap (\(a, b) -> a ++ maybe [] id b ++ "\n") $ ds

maybeToList (Just a) = [a]
fromJust (Just a) = a    
input = Prelude.readFile "in.md"    

-- mapMaybeM :: Monad m => (a -> m b) -> (Maybe a) -> m (Maybe b)


{-
readFile     :: RelativePath -> Transf ()
appendFile   :: RelativePath -> String -> Transf ()
writeFile    :: RelativePath -> String -> Transf ()
writeNewFile :: String -> Transf RelativePath
interpret    :: Typeable a => String -> Transf a

data Transformation = Transformation {
        transfGuard :: (TextLine -> Bool, TextLine -> Bool),
        transfProc  :: (String -> Transf String)
    }
-}
-- between :: (a -> Bool) -> (a -> Bool) -> [a] -> [([a],[a])]
-- between start stop as = 
    -- (takeWhile (not . start) , (takeWhile (not . stop) $ dropWhile (not . start)))


-- | Separate the sections delimited by the separators from their context. Returns
--      [(outside1, inside1), (outside2, inside2)...] 
--
sections :: (a -> Bool) -> (a -> Bool) -> [a] -> [([a], Maybe [a])]
sections start stop as = case (bs,cs) of
    ([], [])  -> []    
    (bs, [])  -> (bs, Nothing) : []
    (bs, [c]) -> (bs, Nothing) : []
    (bs, cs)  -> (bs, Just $ tail cs) : sections start stop (drop skip as)
    where
        (bs,cs) = sections1 start stop as                       
        skip    = length bs + length cs + 1

sections1 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a],[a])
sections1 start stop as = 
    (takeWhile (not . start) as, takeWhile (not . stop) $ dropWhile (not . start) as)


-- -- | 
-- -- Group a list into sublists whereever a predicate holds. The matched element
-- -- is the first in the sublist.
-- --
-- -- > splitWhile isSpace "foo bar baz"
-- -- >    ===> ["foo"," bar"," baz"]
-- -- >
-- -- > splitWhile (> 3) [1,5,4,7,0,1,2]
-- -- >    ===> [[1],[5],[4],[7,0,1,2]]
-- --
-- splitWhile :: (a -> Bool) -> [a] -> [[a]]
-- splitWhile p xs = case splitWhile' p xs of
--     []:xss -> xss
--     xss    -> xss
--     where
--         splitWhile' p []     = [[]]
--         splitWhile' p (x:xs) = case splitWhile' p xs of
--             (xs:xss) -> if p x then []:(x:xs):xss else (x:xs):xss
-- 
-- takeDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
-- takeDropWhile p as = (bs, cs)
--     where
--         bs = takeWhile p as
--         cs = dropWhile p as

-- | 
-- Extract the first consecutive sublist for which the predicate returns true, or
-- the empty list if no such sublist exists.
filterOnce :: (a -> Bool) -> [a] -> [a]
filterOnce p = List.takeWhile p . List.dropWhile (not . p)

first  f (a, b) = (f a, b)
second f (a, b) = (a, f b)

secondM :: Monad m => (a -> m b) -> (c, a) -> m (c, b)
secondM f (a, b) = do
    b' <-  f b
    return $ (a, b')
    
    
