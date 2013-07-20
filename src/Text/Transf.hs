
{-# LANGUAGE
    GeneralizedNewtypeDeriving #-}

module Text.Transf (
        Line,
        Lines,
        RelativePath,

        -- * Combinators
        Transf,
        newTransf,
        namedTransf,
        readFile,
        writeFile,
        eval,
        eval',
        inform,

        -- ** Runing transformations
        runTransf,
        runTransf',

        -- * TF monad
        TFT,
        TF,
        runTFT,
        runTF,

        -- * Predefined transformations
        printT,
        evalT,
        musicT,
        haskellT,
        musicPlusHaskellT,
        musicExtraT,
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
import Music.Prelude.Basic hiding (mapM)

import qualified Prelude
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Traversable as Traversable


-- | A line of text.
type Line = String

-- | Multiple-line text.
type Lines = String

-- | A relative file path.
type RelativePath = FilePath

-- | Transformer version of 'TF'.
newtype TFT m a = TFT { runTFT :: ErrorT String m a }
    deriving (Monad, MonadPlus, MonadError String, MonadIO)

-- | The 'TF' monad defines the context of a transformation function.
--   Think of it as a restricted version of 'IO' or 'STM'.
type TF = TFT IO

runTF :: TF a -> IO (Either String a)
runTF = runErrorT . runTFT

-- | Read a file.
readFile :: RelativePath -> TF String
readFile path = do
    input <- liftIO $ try $ Prelude.readFile path
    case input of
        Left e  -> throwError $ "readFile: " ++ show (e::SomeException)
        Right a -> return a

-- appendFile   :: RelativePath -> String -> TF ()

-- | Write to a file.
writeFile :: RelativePath -> String -> TF ()
writeFile path str = liftIO $ Prelude.writeFile path str

-- | Evaluate a Haskell expression.
eval :: Typeable a => String -> TF a
eval = eval' ["Prelude", "Music.Prelude.Basic", "Control.Monad.Plus"] -- FIXME

-- | Evaluate a Haskell expression.
eval' :: Typeable a => [String] -> String -> TF a
eval' imps str = do                                         
    res <- liftIO $ runInterpreter $ do
        setImports imps
        interpret str infer
    case res of
        Left e -> throwError $ "eval: " ++ show e
        Right a -> return a

-- | Write to the standard error stream.
inform :: String -> TF ()
inform m = liftIO $ hPutStr stderr $ m ++ "\n"

-- | A transformation function, or transformation for short.
data Transf 
    = CompTrans {
        decomp    :: [Transf]
    }
    | SingTrans {
        guard     :: (Line -> Bool, Line -> Bool),
        function  :: Lines -> TF Lines
    }

doTrans (SingTrans _ f) = f

instance Semigroup Transf where
    a <> b = CompTrans [a,b]

-- | Create a new transformation.
newTransf :: (Line -> Bool) -> (Line -> Bool) -> (Lines -> TF Lines) -> Transf
newTransf b e = SingTrans (b, e)

-- | Create a new transformation. 
--
--   This transformation processes everything in between lines containing
--
--   > ~~~name
--   > ~~~
--
--   or alternatively
--
--   > ```name
--   > ```
--
--   where @name@ is the name of the transformation.
--
namedTransf :: String -> (Lines -> TF Lines) -> Transf
namedTransf name f = newTransf (namedGuard name) (namedGuard "") f

namedGuard :: String -> String -> Bool
namedGuard name = namedGuardWithPrefix "```" name `or'` namedGuardWithPrefix "~~~" name

namedGuardWithPrefix :: String -> String -> String -> Bool
namedGuardWithPrefix prefix name = (== (prefix ++ name)) . trimEnd


printT :: Transf
printT = namedTransf "print" $ \input -> do
    return input

evalT :: Transf
evalT = namedTransf "eval" $ \input -> do
    eval input

musicT :: Transf
musicT = namedTransf "music" $ \input -> do
    let name = showHex (abs $ hash input) ""
    music <- eval input :: TF (Score Note)
    liftIO $ writeLy (name++".ly") music
    liftIO $ writeMidi (name++".mid") music
    -- liftIO $ system $ "lilypond -f png -dresolution=300 "++name++".ly"
    -- liftIO $ system $ "convert -transparent white -resize 30% "++name++".png "++name++"x.png"
    liftIO $ system $ "lilypond -f png -dresolution=200 "++name++".ly"
    liftIO $ system $ "convert -transparent white -resize 50% "++name++".png "++name++"x.png"
    let playText = "<div>" ++
                   "  <a href=\"javascript:playFile('"++name++".mid')\"><img src=\"img/play2.png\"/></a>\n" ++
                   "  <a href=\"javascript:stopPlaying()\"><img src=\"img/pause2.png\"/></a>\n" ++
                   "</div>\n"
    return $ playText ++ "![]("++name++"x.png)"
    --  -resize 30%

musicExtraT :: Transf
musicExtraT = namedTransf "music-extra" $ \_ ->
    return $ txt
        where
            txt = "<script src=\"js/jasmid/stream.js\"></script>\n" ++
                  "<script src=\"js/jasmid/midifile.js\"></script>\n" ++
                  "<script src=\"js/jasmid/replayer.js\"></script>\n" ++
                  "<script src=\"js/midi.js\"></script>\n" ++
                  "<script src=\"js/Base64.js\" type=\"text/javascript\"></script>\n" ++
                  "<script src=\"js/base64binary.js\" type=\"text/javascript\"></script>\n" ++
                  "<script src=\"js/main.js\" type=\"text/javascript\"></script>\n"

-- Just pass everything through to Pandoc
haskellT :: Transf
haskellT = namedTransf "haskell" $ \input -> do
    return $ "~~~haskell\n" ++ input ++ "\n~~~"
            
musicPlusHaskellT :: Transf
musicPlusHaskellT = namedTransf "music+haskell" $ \input -> do
    musicRes   <- (doTrans musicT) input
    haskellRes <- (doTrans haskellT) input
    return $ musicRes ++ "\n\n" ++ haskellRes


-- | Run a transformation with the given error handler and input.
runTransf :: Transf -> (String -> IO String) -> String -> IO String
runTransf t handler input = do
    res <- runTF $ runTransf' t input
    case res of
        Left e  -> handler e
        Right a -> return a


runTransf' :: Transf -> String -> TF String
runTransf' = go
    where
        go (CompTrans [])     as = return as
        go (CompTrans (t:ts)) as = do
            bs <- go t as
            go (CompTrans ts) bs
        go (SingTrans (start,stop) f) as = do
            let bs = (sections start stop . lines) as                   :: [([Line], Maybe [Line])]
            let cs = fmap (first unlines . second (fmap unlines)) bs    :: [(String, Maybe String)]
            ds <- Traversable.mapM (secondM (Traversable.mapM f)) cs    :: TF [(String, Maybe String)]    
            return $ concatMap (\(a, b) -> a ++ fromMaybe [] b ++ "\n") ds


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

trimEnd :: String -> String
trimEnd = List.dropWhileEnd Char.isSpace

secondM :: Monad m => (a -> m b) -> (c, a) -> m (c, b)
secondM f (a, b) = do
    b' <-  f b
    return (a, b')
    
or' :: (a -> Bool) -> (a -> Bool) -> a -> Bool
or' p q x = p x || q x    

