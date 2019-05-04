module Parser where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

data GenOptions = GenOptions { genName :: Maybe String, genLength :: Int, genAlphaNum :: Bool }
data CopyOptions = CopyOptions { copyName :: String }
data LsOptions = LsOptions
data CatOptions = CatOptions { catName :: String }
data RmOptions = RmOptions { rmName :: String }

data Command = Gen GenOptions | Cat CatOptions | Copy CopyOptions | Ls LsOptions | Rm RmOptions

data POptions = POptions { optCommand :: Command }

commands :: Parser POptions
commands = POptions <$> commandParser
 where
  commandParser = hsubparser $ mconcat
    [ command "gen" (info genOptions (progDesc "Generates a new password"))
    , command "cat" (info catOptions (progDesc "Prints a password"))
    , command
      "copy"
      (info copyOptions (progDesc "Copies a password to the clipboard"))
    , command "ls" (info lsOptions (progDesc "Lists all password domains"))
    , command "rm" (info rmOptions (progDesc "Removes a password domain"))
    ]

  genOptions = Gen <$> (GenOptions <$> nameDesc <*> lengthDesc <*> alphaDesc)
   where
    nameDesc = optional $ argument str $ mconcat
      [metavar "NAME", help "Name of domain to generate password for"]
    lengthDesc = option auto $ mconcat
      [ long "length"
      , metavar "LENGTH"
      , showDefault
      , value 20
      , help "The length of the password"
      ]
    alphaDesc = switch
      $ mconcat [long "alphanum", help "Only use alphanumeric characters"]

  catOptions = Cat . CatOptions <$> nameDesc
   where
    nameDesc = argument str
      $ mconcat [metavar "NAME", help "Name of domain to print password for"]

  copyOptions = Copy . CopyOptions <$> nameDesc
   where
    nameDesc = argument str
      $ mconcat [metavar "NAME", help "Name of domain to copy password from"]

  lsOptions = pure $ Ls LsOptions
  rmOptions = Rm . RmOptions <$> nameDesc
   where
    nameDesc = argument str
      $ mconcat [metavar "NAME", help "Name of domain to remove password for"]

pParser = info
  (commands <**> helper)
  (fullDesc <> progDesc "p" <> header "p is a simple password management tool")
