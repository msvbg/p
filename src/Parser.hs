module Parser where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

data GenOptions = GenOptions { genName :: Maybe String, genLength :: Int }
data CopyOptions = CopyOptions { copyName :: String }
data LsOptions = LsOptions
data CatOptions = CatOptions { catName :: String }
data RmOptions = RmOptions { rmName :: String }

data Command = Gen GenOptions | Cat CatOptions | Copy CopyOptions | Ls LsOptions | Rm RmOptions

data POptions = POptions { optCommand :: Command }

commands :: Parser POptions
commands = POptions <$> hsubparser
    (  (command "gen" (info genOptions (progDesc "Generates a new password")))
    <> (command "cat" (info catOptions (progDesc "Prints a password")))
    <> (command
           "copy"
           (info copyOptions (progDesc "Copies a password to the clipboard"))
       )
    <> (command "ls" (info lsOptions (progDesc "Lists all password domains")))
    <> (command "rm" (info rmOptions (progDesc "Removes a password domain")))
    )
  where
    genOptions :: Parser Command
    genOptions =
        Gen
            <$> (   GenOptions
                <$> (optional
                        (argument
                            str
                            (metavar "NAME" <> help
                                "Name of domain to generate password for"
                            )
                        )
                    )
                <*> option
                        auto
                        (  long "length"
                        <> metavar "LENGTH"
                        <> showDefault
                        <> value 20
                        <> help "The length of the password"
                        )
                )
    catOptions =
        Cat
            .   CatOptions
            <$> (argument
                    str
                    (  metavar "NAME"
                    <> help "Name of domain to print password for"
                    )
                )
    copyOptions =
        Copy
            .   CopyOptions
            <$> (argument
                    str
                    (  metavar "NAME"
                    <> help "Name of domain to copy password from"
                    )
                )
    lsOptions = pure $ Ls LsOptions
    rmOptions =
        Rm
            .   RmOptions
            <$> (argument
                    str
                    (  metavar "NAME"
                    <> help "Name of domain to remove password for"
                    )
                )

pParser = info
    (commands <**> helper)
    (fullDesc <> progDesc "p" <> header "p is a simple password management tool"
    )
