
{-# LANGUAGE
    GeneralizedNewtypeDeriving #-}

module Text.Transf (
        -- * Basic types
        Line,
        Lines,
        RelativePath,

        -- * The TF type
        TFT,
        TF,
        runTFT,
        runTF,

        -- * Transformations
        Transf,

        -- ** Creating new transformations
        newTransf,
        namedTransf,

        -- ** Running transformations
        runTransf,
        runTransf',

        -- * Combinators
        -- ** Input/output
        readFile,
        writeFile,
        inform,
        -- ** Evaluation
        eval,
        evalWith,

        -- * Transformations
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
import Control.Monad.Error
import Control.Monad.Plus
import Numeric
import Data.Maybe
import Data.Semigroup
import Data.Traversable
import Data.Typeable
import Data.Hashable
import System.IO (hPutStr, stderr)
import System.Process
import Language.Haskell.Interpreter hiding (eval)

import Music.Prelude.Basic

import qualified Prelude
import qualified Data.List          as List
import qualified Data.Char          as Char
import qualified Data.Traversable   as Traversable

-- | A line of text.
type Line = String

-- | Multiple-line text.
type Lines = String

-- | A relative file path.
type RelativePath = FilePath

-- | Transformer version of 'TF'.
newtype TFT m a = TFT { runTFT :: ErrorT String m a }
    deriving (Monad, MonadPlus, MonadError String, MonadIO)

-- | The 'TF' monad defines the context of a transform.
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
eval = evalWith ["Prelude", "Music.Prelude.Basic", "Control.Monad.Plus"] -- FIXME

-- | Evaluate a Haskell expression with the given modules in scope.
--   
--   Note that 'Prelude' is /not/ implicitly imported so it should be included
--   in the import list if you want it to be in scope.
--   
--   All requested modules must be present on the system or the computation
--   will fail. Also, the string must be a valid Haskell expression using
--   constructs which in scope after loading the given modules.
--
--   Errors can be caught using 'catchError'.
--   
evalWith :: Typeable a => [String] -> String -> TF a
evalWith imps str = do
    res <- liftIO $ runInterpreter $ do
        setImports imps
        interpret str infer
    case res of
        Left e -> throwError $ "eval: " ++ show e
        Right a -> return a

-- | Write to the standard error stream.
inform :: String -> TF ()
inform m = liftIO $ hPutStr stderr $ m ++ "\n"

-- | A transformation.
data Transf
    = CompTrans {
        decomp    :: [Transf]
    }
    | SingTrans {
        delimiters :: (Line -> Bool, Line -> Bool),
        function   :: Lines -> TF Lines
    }

doTrans (SingTrans _ f) = f

instance Semigroup Transf where
    a <> b = CompTrans [a,b]

-- | Create a new transformation. For example:
--
-- > newTransf start stop change
--
-- This creates a new transformation that searches its input for consecutive
-- sequences of lines delimited by lines accepted by the @start@ and @stop@
-- functions, and applies the given change function to these chunks.
--
-- To create a suitable change function, use the combinators defined below.
--
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
--   To create a suitable change function, use the combinators defined below.
--
namedTransf :: String -> (Lines -> TF Lines) -> Transf
namedTransf name = newTransf (namedGuard name) (namedGuard "")

namedGuard :: String -> String -> Bool
namedGuard name = namedGuardWithPrefix "```" name `oneOf` namedGuardWithPrefix "~~~" name

namedGuardWithPrefix :: String -> String -> String -> Bool
namedGuardWithPrefix prefix name = (== (prefix ++ name)) . trimEnd

----------------------------------------------------------------------------------------------------
-- Transformations
----------------------------------------------------------------------------------------------------

printT :: Transf
printT = namedTransf "print" $ \input -> return input

evalT :: Transf
evalT = namedTransf "eval" $ \input -> eval input

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
musicExtraT = namedTransf "music-extra" $ \_ -> return txt
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
haskellT = namedTransf "haskell" $ \input ->
    return $ "~~~haskell\n" ++ input ++ "\n~~~"

musicPlusHaskellT :: Transf
musicPlusHaskellT = namedTransf "music+haskell" $ \input -> do
    musicRes   <- doTrans musicT input
    haskellRes <- doTrans haskellT input
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



----------------------------------------------------------------------------------------------------

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

oneOf :: (a -> Bool) -> (a -> Bool) -> a -> Bool
oneOf p q x = p x || q x

