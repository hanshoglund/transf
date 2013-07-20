
{-# LANGUAGE
    GeneralizedNewtypeDeriving #-}

module Text.Transf (
        -- * Basic types
        Line,
        Lines,
        RelativePath,

        -- * The Context type
        Context,
        runContext,
        ContextT,
        runContextT,

        -- * Transformormations
        Transform,

        -- -- ** Creating new transformations
        namedTransform,
        -- newTransform,

        -- ** Running transformations
        runTransform,
        runTransform',

        -- * Combinators
        -- ** Input/output
        readFile,
        writeFile,
        inform,
        -- ** Evaluation
        eval,
        evalWith,

        -- * Transformormations
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

-- | 
-- A line of text.
type Line = String

-- | 
-- Multiple-line text.
type Lines = String

-- | 
-- A relative file path.
type RelativePath = FilePath

-- | 
-- Monad transformer version of Context.
newtype ContextT m a = ContextT { runContextT' :: ErrorT String m a }
    deriving (Monad, MonadPlus, MonadError String, MonadIO)

-- | 
-- The Context monad defines the context of a transform.
type Context = ContextT IO

-- | 
-- Run a computation in the Context context.
runContext :: Context a -> IO (Either String a)
runContext = runErrorT . runContextT

-- | 
-- Run a computation in the ContextT context.
runContextT = runContextT'



-- | 
-- Read a file.
readFile :: RelativePath -> Context String
readFile path = do
    input <- liftIO $ try $ Prelude.readFile path
    case input of
        Left e  -> throwError $ "readFile: " ++ show (e::SomeException)
        Right a -> return a

-- appendFile   :: RelativePath -> String -> Context ()

-- | 
-- Write to a file.
writeFile :: RelativePath -> String -> Context ()
writeFile path str = liftIO $ Prelude.writeFile path str

-- | 
-- Evaluate a Haskell expression.
eval :: Typeable a => String -> Context a
eval = evalWith ["Prelude", "Music.Prelude.Basic", "Control.Monad.Plus"] -- FIXME

-- | Evaluate a Haskell expression with the given modules in scope.
--   Note that "Prelude" is /not/ implicitly imported.
--   
--   
--   All requested modules must be present on the system or the computation
--   will fail. Also, the string must be a valid Haskell expression using
--   constructs which in scope after loading the given modules.
--
--   Errors can be caught using 'catchError'.
--   
evalWith :: Typeable a => [String] -> String -> Context a
evalWith imps str = do
    res <- liftIO $ runInterpreter $ do
        setImports imps
        interpret str infer
    case res of
        Left e -> throwError $ "eval: " ++ show e
        Right a -> return a

-- | 
-- Write to the standard error stream.
inform :: String -> Context ()
inform m = liftIO $ hPutStr stderr $ m ++ "\n"

-- | 
-- A transformation.
data Transform
    = CompTrans {
        decomp    :: [Transform]
    }
    | SingTrans {
        delimiters :: (Line -> Bool, Line -> Bool),
        function   :: Lines -> Context Lines
    }

doTrans (SingTrans _ f) = f

instance Semigroup Transform where
    a <> b = CompTrans [a,b]
instance Monoid Transform where
    mempty  = CompTrans []
    mappend = (<>)

-- | Create a new transformation. For example:
--
-- > newTransform start stop change
--
-- This creates a new transformation that searches its input for consecutive
-- sequences of lines delimited by lines accepted by the @start@ and @stop@
-- functions, and applies the given change function to these chunks.
--
-- To create a suitable change function, use the combinators defined below.
--
newTransform :: (Line -> Bool) -> (Line -> Bool) -> (Lines -> Context Lines) -> Transform
newTransform b e = SingTrans (b, e)

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
namedTransform :: String -> (Lines -> Context Lines) -> Transform
namedTransform name = newTransform (namedGuard name) (namedGuard "")

namedGuard :: String -> String -> Bool
namedGuard name = namedGuardWithPrefix "```" name `oneOf` namedGuardWithPrefix "~~~" name

namedGuardWithPrefix :: String -> String -> String -> Bool
namedGuardWithPrefix prefix name = (== (prefix ++ name)) . trimEnd

----------------------------------------------------------------------------------------------------
-- Transformormations
----------------------------------------------------------------------------------------------------

printT :: Transform
printT = namedTransform "print" $ \input -> return input

evalT :: Transform
evalT = namedTransform "eval" $ \input -> eval input

musicT :: Transform
musicT = namedTransform "music" $ \input -> do
    let name = showHex (abs $ hash input) ""
    music <- eval input :: Context (Score Note)
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

musicExtraT :: Transform
musicExtraT = namedTransform "music-extra" $ \_ -> return txt
    where
        txt = "<script src=\"js/jasmid/stream.js\"></script>\n" ++
              "<script src=\"js/jasmid/midifile.js\"></script>\n" ++
              "<script src=\"js/jasmid/replayer.js\"></script>\n" ++
              "<script src=\"js/midi.js\"></script>\n" ++
              "<script src=\"js/Base64.js\" type=\"text/javascript\"></script>\n" ++
              "<script src=\"js/base64binary.js\" type=\"text/javascript\"></script>\n" ++
              "<script src=\"js/main.js\" type=\"text/javascript\"></script>\n"

-- Just pass everything through to Pandoc
haskellT :: Transform
haskellT = namedTransform "haskell" $ \input ->
    return $ "~~~haskell\n" ++ input ++ "\n~~~"

musicPlusHaskellT :: Transform
musicPlusHaskellT = namedTransform "music+haskell" $ \input -> do
    musicRes   <- doTrans musicT input
    haskellRes <- doTrans haskellT input
    return $ musicRes ++ "\n\n" ++ haskellRes


-- | Run a transformation with the given error handler and input.
runTransform :: Transform -> (String -> IO String) -> String -> IO String
runTransform t handler input = do
    res <- runContext $ runTransform' t input
    case res of
        Left e  -> handler e
        Right a -> return a


-- | Run a transformation in the 'Context' monad.
runTransform' :: Transform -> String -> Context String
runTransform' = go
    where
        go (CompTrans [])     as = return as
        go (CompTrans (t:ts)) as = do
            bs <- go t as
            go (CompTrans ts) bs
        go (SingTrans (start,stop) f) as = do
            let bs = (sections start stop . lines) as                   :: [([Line], Maybe [Line])]
            let cs = fmap (first unlines . second (fmap unlines)) bs    :: [(String, Maybe String)]
            ds <- Traversable.mapM (secondM (Traversable.mapM f)) cs    :: Context [(String, Maybe String)]
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

