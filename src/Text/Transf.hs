
{-# LANGUAGE
    GeneralizedNewtypeDeriving #-}

module Text.Transf (
        Line,
        Lines,
        RelativePath,

        -- ** TF monad
        TF,
        runTF,

        -- ** Transf
        Transf,
        transformation,
        readFile,
        writeFile,
        eval,
        inform,
        censorT,
        printT,
        evalT,
        musicT,
        runTransf,
        runTransf'
) 
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
import Language.Haskell.Interpreter hiding (eval)
import System.IO (hPutStr, stderr)
import System.Process
import Numeric
import Music.Prelude.Basic

import qualified Prelude
import qualified Data.List as List


-- | A line of text.
type Line = String

-- | Multiple-line text.
type Lines = String

-- | A relative file path.
type RelativePath = FilePath

-- | 
newtype TF a = TF { getTF :: 
    ErrorT String IO a 
    }
    deriving (Monad, MonadPlus, MonadError String, MonadIO)

runTF :: TF a -> IO (Either String a)
runTF = runErrorT . getTF

-- | Read a file.
readFile :: RelativePath -> TF String
readFile path = do
    input <- liftIO $ try $ Prelude.readFile path
    case input of
        Left e  -> throwError $ "readFile: " ++ show (e::SomeException)
        Right a -> return a

-- appendFile   :: RelativePath -> String -> TF ()

writeFile :: RelativePath -> String -> TF ()
writeFile path str = liftIO $ Prelude.writeFile path str

eval :: Typeable a => String -> TF a
eval str = do                                         
    res <- liftIO $ runInterpreter $ do
        setImports ["Prelude", "Music.Prelude.Basic"]
        interpret str infer
    case res of
        Left e -> throwError $ "eval: " ++ show e
        Right a -> return a

inform :: [Char] -> TF ()
inform m = liftIO $ hPutStr stderr $ m ++ "\n"

data Transf 
    = CompTrans {
        decomp    :: [Transf]
    }
    | SingTrans {
        guard     :: (Line -> Bool, Line -> Bool),
        function  :: Lines -> TF Lines
    }

transformation :: (Line -> Bool) -> (Line -> Bool) -> (Lines -> TF Lines) -> Transf
transformation b e = SingTrans (b, e)

instance Semigroup Transf where
    a <> b = CompTrans [a,b]

censorT :: Transf
censorT = SingTrans ((== "```censor"), (== "```")) $ \input -> do
    liftIO $ hPutStr stderr "Censoring!\n"
    return "(censored)"

printT :: Transf
printT = SingTrans ((== "```print"), (== "```")) $ \input -> do
    liftIO $ hPutStr stderr "Passing through!\n"
    return input

evalT :: Transf
evalT = SingTrans ((== "```eval"), (== "```")) $ \input -> do
    liftIO $ hPutStr stderr "Evaluating!\n"
    eval input

musicT :: Transf
musicT = SingTrans ((== "```music"), (== "```")) $ \input -> do
    let name = showHex (abs $ hash input) ""
    liftIO $ hPutStr stderr "Interpreting music!\n"
    music <- eval input :: TF (Score Note)
    liftIO $ writeLy (name++".ly") music
    liftIO $ system $ "lilypond -f png -dresolution=300 "++name++".ly"
    liftIO $ system $ "convert -resize 30% "++name++".png "++name++"x.png"
    return $ "![Output]("++name++"x.png)"



runTransf :: Transf -> String -> TF String
runTransf (CompTrans []) as = return as

runTransf (CompTrans (t:ts)) as = do
    bs <- runTransf t as
    runTransf (CompTrans ts) bs
    
runTransf (SingTrans (start,stop) f) as = do
    let bs = (sections start stop . lines) as                   :: [([Line], Maybe [Line])]
    let cs = fmap (first unlines . second (fmap unlines)) bs    :: [(String, Maybe String)]
    ds <- mapM (secondM (mapM f)) cs                            :: TF [(String, Maybe String)]    
    return $ concatMap (\(a, b) -> a ++ fromMaybe [] b ++ "\n") ds

runTransf' :: Transf -> (String -> IO String) -> String -> IO String
runTransf' t handler input = do
    res <- runTF $ runTransf t input
    case res of
        Left e  -> handler e
        Right a -> return a


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
    
    
