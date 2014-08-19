
{-# LANGUAGE                  
    GeneralizedNewtypeDeriving #-}

module Text.Transf (
        -- * Basic types
        Line,
        Lines,
        RelativePath,

        -- * The Context type
        Context,
        ContextT,
        runContext,
        runContextT,

        -- * Transformormations
        Transform,

        -- -- ** Creating new transformations
        transform,
        -- newTransform,

        -- ** Running transformations
        runTransform,
        -- runTransformIO,

        -- * Combinators
        -- ** Input/output
        readFile,
        writeFile,
        inform,
        -- ** Evaluation
        eval,
        evalWith,
        addPost,

        -- * Transformormations
        printT,
        evalT,   
        musicT,
        MusicOpts(..),
        musicT',
        haskellT,
        evalHaskellT,
        musicHaskellT,
        musicHaskellT',
        musicExtraT,
  ) where

import Prelude hiding (mapM, readFile, writeFile)

import Control.Applicative
import Control.Exception
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Writer hiding ((<>))
import Control.Monad.Error
import Control.Monad.Plus
import Numeric
import Data.Maybe
import Data.Default
import Data.Semigroup
import Data.Traversable
import Data.Typeable
import Data.Hashable
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process
import Language.Haskell.Interpreter hiding (eval)

import qualified Prelude
import qualified Data.List          as List
import qualified Data.Char          as Char
import qualified Data.Traversable   as Traversable
-- import qualified Music.Prelude.Basic as Music

-- |
-- A single line of text.
--
type Line = String

-- |
-- Multiple lines of text.
--
type Lines = String

-- | 
-- A relative file path.
--
type RelativePath = FilePath

-- | 
-- Action to be executed after main transf pass.
--
newtype Post m = Post [ContextT m ()]
    deriving (Monoid)

post :: ContextT m () -> Post m
post = Post . return

type PrimContextT m = ErrorT String (WriterT (Post m) m)

newtype ContextT m a = ContextT { runContextT_ :: PrimContextT m a }
    deriving ( Functor, Monad, MonadIO, MonadPlus, Applicative, 
      Alternative, MonadError String, MonadWriter (Post m) )

-- | 
-- The 'Context' monad defines the context of a transformation.
--
-- The main purpose of this type is to restrict the the number
-- of functions you can pass to 'transform'. 
--
type Context = ContextT IO

-- |
-- Run a computation in the 'Context' monad.
--
runContext :: Context a -> IO (Either String a)
runContext x = do
    (r, Post posts) <- runC x
    parallel_ (fmap ignoreErrorsAndPost posts)
    return r
    where
        runC = runWriterT . runErrorT . runContextT_

runContextT :: Monad m => ContextT m a -> m (Either String a)
runContextT = runContextT' True

runContextT' :: Monad m => Bool -> ContextT m a -> m (Either String a)
runContextT' recur x = do
    (r, Post posts) <- runC x
    if recur then runContextT' False (sequence_ posts) else return (return ())
    return r
    where
        runC = runWriterT . runErrorT . runContextT_

ignoreErrorsAndPost :: ContextT IO a -> IO ()
ignoreErrorsAndPost x = (runWriterT . runErrorT . runContextT_) x >> return ()





-- |
-- A transformation.
-- 
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

namedFence :: String -> String -> Bool
namedFence name = namedFenceWithPrefix "```" name `oneOf` namedFenceWithPrefix "~~~" name

namedFenceWithPrefix :: String -> String -> String -> Bool
namedFenceWithPrefix prefix name = (== (prefix ++ name)) . trimEnd

-- | Create a new transformation.
--
--   This transformation processes everything in between lines containing
--   a fence such as
--
--   > ~~~name
--   > ~~~
--
--   or
--
--   > ```name
--   > ```
--
--   where @name@ is the name of the transformation.
--
--   To create a suitable change function, use the combinators defined below.
--
transform :: String -> (Lines -> Context Lines) -> Transform
transform name = newTransform (namedFence name) (namedFence "")

-- | 
-- Run a transformation with the given error handler and input.
--
runTransformIO :: Transform -> (String -> IO String) -> String -> IO String
runTransformIO t handler input = do
    res <- runContext $ runTransform t input
    case res of
        Left e  -> handler e
        Right a -> return a

-- | 
-- Run a transformation in the 'Context' monad.
--
runTransform :: Transform -> String -> Context String
runTransform = go
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

-- |
-- Read a file.
--
readFile :: RelativePath -> Context String
readFile path = do
    input <- liftIO $ try $ Prelude.readFile path
    case input of
        Left e  -> throwError $ "readFile: " ++ show (e::SomeException)
        Right a -> return a

-- appendFile   :: RelativePath -> String -> Context ()

-- |
-- Write to a file.
--
writeFile :: RelativePath -> String -> Context ()
writeFile path str = liftIO $ Prelude.writeFile path str

-- |
-- Evaluate a Haskell expression.
--
eval :: Typeable a => String -> Context a
eval = evalWith ["Prelude", "Music.Prelude.Basic"] 
    -- FIXME hardcoded
    -- For some reason, Pitch needs to be in scope (type synonym exported from Music.Prelude.Basic)

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
        set [languageExtensions := [OverloadedStrings, NoMonomorphismRestriction]]
        setImports imps
        interpret str infer
    case res of
        Left e -> throwError $ "Could not evaluate: " ++ str ++ "\n" ++ showIE e
        Right a -> return a
    where
        showIE (WontCompile xs)  = "    " ++ List.intercalate "\n    " (fmap errMsg xs)
        showIE (UnknownError x)  = x
        showIE (NotAllowed x)    = x
        showIE (GhcException x)  = x

-- |
-- Write to the standard error stream.
--
inform :: String -> Context ()
inform m = liftIO $ hPutStr stderr $ m ++ "\n"

-- |
-- Register an action to be run after text processing has finished.
-- This can be used to optimize tasks such as external file generations.
--
-- Note that addPost does not work trasitively, i.e. post actions of
-- post actions are thrown away.
-- 
addPost :: Context () -> Context ()
addPost = tell . post


----------------------------------------------------------------------------------------------------

-- |
-- This named transformation posts its input to the standard error stream
-- and returns nothing.
--
printT :: Transform
printT = transform "print" $ \input -> inform input >> return ""

-- |
-- This named transformation evaluates its input as a Haskell expression of
-- type 'String' and returns the value of the expression.
--
-- For example the input
--
-- > ~~~haskell
-- > "The number is " ++ show $ 3 + 2
-- > ~~~
--
-- Will be transformed into
--
-- > The number is 6
--
evalT :: Transform
-- evalT = transform "eval" $ \input -> evalWith ["Prelude"] input
evalT = transform "eval" $ \input -> do
  (exit, out, err) <- liftIO $ readProcessWithExitCode "runhaskell2" [] input
  inform err
  return out

-- TODO move to separate module and/or package

data MusicOpts = MusicOpts {
        format     :: String,
        resolution :: Int,
        resize     :: Int,
        prelude    :: String
    }
instance Default MusicOpts where def = MusicOpts {
        format     = "png",
        resolution = 200,
        resize     = 45,
        prelude     = "basic"
    }

-- |
-- This named transformation evaluates its input as a music expression.
--
-- The music is rendered as an @.ly@ file and a @.mid@ fiel, then @lilypond@ and @convert@
-- is run to render a @.png@ file. A markdown image tag and a HTML play and stop button
-- is returned.
--
-- The expression must return a value of type @Score Note@. The "Music.Prelude.Basic"
-- module is implicitly imported.
--
musicT :: Transform
musicT = musicT' def

musicT' :: MusicOpts -> Transform
musicT' opts = transform "music" $ \input -> do
    let prel = prelude opts
    let name = showHex (abs $ hash input) ""

    {-
    music <- eval input :: Context (Music.Score Music.BasicNote)

    liftIO $ let handler ex = hPutStrLn stderr $ "transf (music+ly): " ++ show (ex::SomeException) ++ "\n" ++ show input
        in handler `handle` (Music.writeLilypond (name++".ly") music)

    liftIO $ Music.writeMidi (name++".mid") music
    -- liftIO $ void $ readProcess "timidity" ["-Ow", name++".mid"] ""
-}

    -- Use music2... wrappers rather than hint
    -- Note that the use of readProcess will propagate error messages from stderr
    -- (including both parse and type errors).

    currentFile <- liftIO $ tryMaybe $ Prelude.readFile (name++".music")
    unless (currentFile == Just input) $ do
      writeFile (name++".music") input
      liftIO $ void $ readProcess "music2ly"   ["--prelude", prel, "-o", name++".ly",  name++".music"] ""
      liftIO $ void $ readProcess "music2midi" ["--prelude", prel, "-o", name++".mid", name++".music"] ""

      let makeLy = do
          (exit, out, err) <- readProcessWithExitCode "lilypond" [
              "-f", format opts, 
              "-dresolution=" ++ show (resolution opts) ++ "", name++".ly"
              ] mempty
          hPutStr stderr out
          hPutStr stderr err
          return ()
    
      let makePng = when (format opts == "png") $ void $ system $ 
              "convert -transparent white -resize " 
                  ++ show (resize opts) ++"% "
                  ++ name ++".png "
                  ++ name ++ "x.png"

      addPost (liftIO $ makeLy >> makePng)

    -- let playText = ""

    -- Play generated MIDI file
    let playText = "<div class='haskell-music-listen'><a href='"++name++".mid'>[listen]</a></div>"
    
    -- Play generated WAV file
    -- let playText = "<div class='haskell-music-listen'><a href='"++name++".wav'>[listen]</a></div>"

    -- Use Web MIDI player
    -- let playText = "<div>" ++
    --                "  <a href=\"javascript:playFile('"++name++".mid')\">[play]</a>\n" ++
    --                "  <a href=\"javascript:stopPlaying()\">[stop]</a>\n" ++
    --                "</div>\n"
                                                   
    let ending = if format opts == "png" then "x" else ""
    return $ playText ++ "\n\n" ++ "![](" ++ name ++ ending ++ "." ++ format opts ++ ")"
    --  -resize 30%

-- |
-- This named transformation includes stuff needed for music playback.
--
-- It should be used exactly once in the document.
--
musicExtraT :: Transform
musicExtraT = transform "music-extra" $ \_ -> return txt
    where
        txt = "<script src=\"js/jasmid/stream.js\"></script>\n" ++
              "<script src=\"js/jasmid/midifile.js\"></script>\n" ++
              "<script src=\"js/jasmid/replayer.js\"></script>\n" ++
              "<script src=\"js/midi.js\"></script>\n" ++
              "<script src=\"js/Base64.js\" type=\"text/javascript\"></script>\n" ++
              "<script src=\"js/base64binary.js\" type=\"text/javascript\"></script>\n" ++
              "<script src=\"js/main.js\" type=\"text/javascript\"></script>\n"

{-
    <script src="js/jasmid/stream.js"></script>
    <script src="js/jasmid/midifile.js"></script>
    <script src="js/jasmid/replayer.js"></script>
    <script src="js/midi.js"></script>
    <script src="js/Base64.js" type="text/javascript"></script>
    <script src="js/base64binary.js" type="text/javascript"></script>
    <script src="js/main.js" type="text/javascript"></script>
-}

    
-- |
-- This named transformation passes everything through and retains the source.
-- 
haskellT :: Transform
haskellT = transform "haskell" $ \input ->
    return $ "```haskell\n" ++ input ++ "\n```"

-- |
-- This named transformation runs the 'music' transformation and retains the source.
-- 
musicHaskellT :: Transform
musicHaskellT = musicHaskellT' def

musicHaskellT' :: MusicOpts -> Transform
musicHaskellT' opts = transform "music+haskell" $ \input -> do
    let begin    = "<div class='haskell-music'>"
    let end      = "</div>"
    musicRes   <- doTrans (musicT' opts) input
    haskellRes <- doTrans haskellT input
    return $ begin ++ "\n\n" ++ musicRes ++ "\n\n" ++ haskellRes ++ "\n\n" ++ end


evalHaskellT :: Transform
evalHaskellT = transform "eval+haskell" $ \input -> do
    evalRes    <- doTrans evalT input
    haskellRes <- doTrans haskellT input
    return $ "\n\n" ++ evalRes ++ "\n\n" ++ haskellRes ++ "\n\n"


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

parallel_ :: [IO ()] -> IO ()
parallel_ = foldb concurrently_ (return ())

concurrently_ :: IO a -> IO b -> IO ()
concurrently_ = concurrentlyWith (\x y -> ())

concurrentlyWith :: (a -> b -> c) -> IO a -> IO b -> IO c
concurrentlyWith f x y = uncurry f <$> x `concurrently` y

foldb :: (a -> a -> a) -> a -> [a] -> a
foldb f z []  = z  
foldb f z [x] = x 
foldb f z xs = let (as,bs) = split xs
    in foldb f z as `f` foldb f z bs
    where
        split xs = (take n xs, drop n xs) where n = length xs `div` 2


tryMaybe :: IO a -> IO (Maybe a)
tryMaybe action = do
  r <- try action
  return $ case r of
    Left e  -> let e' = (e::SomeException) in Nothing
    Right x -> Just x

