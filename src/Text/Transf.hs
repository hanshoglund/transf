
{-# LANGUAGE
    GeneralizedNewtypeDeriving #-}

module Text.Transf -- (
  -- ) 
where

import Prelude hiding (mapM, readFile, writeFile)         

import Control.Exception
import Control.Monad.Error hiding (mapM)
import Control.Monad.Plus hiding (mapM)
import Data.Semigroup
import Data.Traversable (mapM)     
import Data.Typeable
import Data.Hashable
import Data.Maybe
import Language.Haskell.Interpreter
import System.IO (hPutStr, stderr)
import System.Process
import Numeric
import Music.Prelude.StringQuartet

import qualified Prelude
import qualified Data.List as List


-- | A line of text.
type Line = String

-- | A relative file path.
type RelativePath = FilePath

-- | 
newtype Transf a = Transf { getTransf :: 
    ErrorT String IO a 
    }
    deriving (Monad, MonadPlus, MonadError String, MonadIO)

runTransf :: Transf a -> IO (Either String a)
runTransf = runErrorT . getTransf

-- | Read a file.
readFile :: RelativePath -> Transf String
readFile path = do
    input <- liftIO $ try $ Prelude.readFile path
    case input of
        Left e  -> throwError $ "readFile: " ++ show (e::SomeException)
        Right a -> return a

-- appendFile   :: RelativePath -> String -> Transf ()

writeFile :: RelativePath -> String -> Transf ()
writeFile path str = liftIO $ Prelude.writeFile path str

interp :: Typeable a => String -> Transf a
interp str = do                                         
    res <- liftIO $ runInterpreter $ do
        setImports ["Prelude", "Music.Prelude.StringQuartet"]
        interpret str infer
    case res of
        Left e -> throwError $ "interp: " ++ show e
        Right a -> return a

data Transformation 
    = CompTrans {
        decomp    :: [Transformation]
    }
    | SingTrans {
        guard     :: (Line -> Bool, Line -> Bool),
        function  :: Line -> Transf Line
    }

instance Semigroup Transformation where
    a <> b = CompTrans [a,b]

censorT :: Transformation
censorT = SingTrans ((== "```censor"), (== "```")) $ \input -> do
    liftIO $ hPutStr stderr "Censoring!\n"
    return "(censored)"

printT :: Transformation
printT = SingTrans ((== "```print"), (== "```")) $ \input -> do
    liftIO $ hPutStr stderr "Passing through!\n"
    return input

evalT :: Transformation
evalT = SingTrans ((== "```eval"), (== "```")) $ \input -> do
    liftIO $ hPutStr stderr "Evaluating!\n"
    interp input

musicT :: Transformation
musicT = SingTrans ((== "```music"), (== "```")) $ \input -> do
    let name = showHex (abs $ hash input) ""
    liftIO $ hPutStr stderr "Interpreting music!\n"
    music <- interp input :: Transf (Score Note)
    liftIO $ writeLy (name++".ly") music
    liftIO $ runCommand $ "lilypond -f png "++name++".ly"
    return $ "![Output]("++name++".png)"



runTransformation :: Transformation -> String -> Transf String
runTransformation (CompTrans []) as = return as

runTransformation (CompTrans (t:ts)) as = do
    bs <- runTransformation t as
    runTransformation (CompTrans ts) bs
    
runTransformation (SingTrans (start,stop) f) as = do
    let bs = (sections start stop . lines) as                   :: [([Line], Maybe [Line])]
    let cs = fmap (first unlines . second (fmap unlines)) bs    :: [(String, Maybe String)]
    ds <- mapM (secondM (mapM f)) cs                            :: Transf [(String, Maybe String)]    
    return $ concatMap (\(a, b) -> a ++ fromMaybe b ++ "\n") ds




-- | Separate the sections delimited by the separators from their context. Returns
--      [(outside1, inside1), (outside2, inside2)...] 
--
sections :: (a -> Bool) -> (a -> Bool) -> [a] -> [([a], Maybe [a])]
sections start stop as = case (bs,cs) of
    ([], [])  -> []    
    (bs, [])  -> [(bs, Nothing)]
    (bs, [c]) -> [(bs, Nothing)]
    (bs, cs)  -> (bs, Just $ tail cs) : sections start stop (drop skip as)
    where
        (bs,cs) = sections1 start stop as                       
        skip    = length bs + length cs + 1

sections1 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a],[a])
sections1 start stop as = 
    (takeWhile (not . start) as, takeWhile (not . stop) $ dropWhile (not . start) as)


first  f (a, b) = (f a, b)
second f (a, b) = (a, f b)

secondM :: Monad m => (a -> m b) -> (c, a) -> m (c, b)
secondM f (a, b) = do
    b' <-  f b
    return (a, b')
    
    
