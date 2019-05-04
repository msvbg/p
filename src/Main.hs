{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Prelude                 hiding ( map )
import           Data.ByteString                ( map )
import           Data.ByteString.Char8          ( unpack )
import           System.Entropy
import           Data.Maybe
import           Data.Foldable                  ( for_ )
import           Options.Applicative
import           Parser
import           Config
import           System.Hclip
import           Control.Monad.Reader
import           System.Directory
import           System.FilePath                ( (</>) )
import           Util
import Data.Char (chr)

-- | Retrieves a password from a file on disk
getPassword :: (MonadReader Config m, MonadIO m) => FilePath -> m String
getPassword name = do
    config   <- ask
    password <- liftIO $ readFile (passwordsPath config </> name)
    return $ rstrip password

-- | Generates a random password of length `length`
gen :: (MonadReader Config m, MonadIO m) => GenOptions -> m ()
gen cmdOptions = do
    config <- ask
    liftIO $ do
        password <- generatePassword (genAlphaNum cmdOptions) (genLength cmdOptions)
        -- Store the password if a name has been provided
        for_
            (genName cmdOptions)
            (\name -> writePassword (passwordsPath config </> name) password)
        setClipboard password
        putStrLn $ "Generated password: " ++ password
        putStrLn "Password was copied to clipboard"
  where
    encode s | s < (126 - 33) = 33 + s
             | otherwise      = encode $ s `mod` (126 - 33)
    encodeAlphaNumeric s | s < 10 = s + 48
                         | s < 10 + 26 = (s - 10) + 97
                         | s < 10 + 26 + 26 = (s - 10 - 26) + 65
                         | otherwise      = encodeAlphaNumeric $ (s + 1) `mod` (10 + 26 + 26)
    generatePassword :: Bool -> Int -> IO String
    generatePassword alphaNumeric length =
        let enc = if alphaNumeric then encodeAlphaNumeric else encodeAlphaNumeric in
        getEntropy length >>= return . unpack . map enc
    writePassword path password = do
        writeFile path password
        putStrLn $ "Password written to " ++ path

-- | Lists all stored passwords
ls :: (MonadReader Config m, MonadIO m) => LsOptions -> m ()
ls cmdOptions = do
    config <- ask
    dirs   <- liftIO $ listDirectory (passwordsPath config)
    liftIO $ forM_ dirs putStrLn

-- | Prints a stored password
cat :: (MonadReader Config m, MonadIO m) => CatOptions -> m ()
cat cmdOptions = do
    password <- getPassword $ catName cmdOptions
    liftIO $ putStrLn password

-- | Copies a stored password to the clipboard
copy :: (MonadReader Config m, MonadIO m) => CopyOptions -> m ()
copy cmdOptions = do
    password <- getPassword $ copyName cmdOptions
    liftIO $ setClipboard password

-- | Removes a stored password
rm :: (MonadReader Config m, MonadIO m) => RmOptions -> m ()
rm cmdOptions = do
    config <- ask
    liftIO $ removeFile (passwordsPath config </> rmName cmdOptions)

-- | Maps between parsed commands and executions of commands
execCommand :: (MonadReader Config m, MonadIO m) => Command -> m ()
execCommand cmd = case cmd of
    Gen  genOptions  -> gen genOptions
    Ls   lsOptions   -> ls lsOptions
    Cat  catOptions  -> cat catOptions
    Copy copyOptions -> copy copyOptions
    Rm   rmOptions   -> rm rmOptions

main :: IO ()
main = do
    config <- getConfig
    opts   <- customExecParser (prefs showHelpOnEmpty) pParser
    runReaderT (execCommand $ optCommand opts) config
    return ()
