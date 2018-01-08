#! /usr/bin/env nix-shell
#! nix-shell -i "stack --nix --resolver lts-9 --install-ghc --silent runghc --package optparse-applicative --package text --" -p stack

{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup ((<>))
import Data.Text as T
import Options.Applicative

data Command = Command
  { name      :: String
  , firstname :: String
  , org       :: String
  , mail      :: String
  , twitter   :: String
  , clean     :: Bool
  , smudge    :: Bool
  }

parseExec :: Parser Command
parseExec = Command
  <$> strOption
  (long "name"
    <> short 'n'
    <> metavar "NAME"
    <> help "Name")
  <*> strOption
  (long "firstname"
    <> short 'f'
    <> metavar "FIRSTNAME"
    <> help "First name")
  <*> strOption
  (long "org"
    <> short 'o'
    <> metavar "ORG"
    <> help "Organisation")
  <*> strOption
  (long "mail"
    <> short 'm'
    <> metavar "MAIL"
    <> help "Mail")
  <*> strOption
  (long "twitter"
    <> short 't'
    <> metavar "TWITTER"
    <> help "Twitter account")
  <*> switch
  (long "clean"
    <> short 'c'
    <> help "clean")
  <*> switch
  (long "smudge"
    <> short 's'
    <> help "smudge")

main :: IO ()
main = cleanid =<< execParser opts
  where
    opts = info (parseExec <**> helper)
           (fullDesc <> progDesc "git clean identity")

cleanid :: Command -> IO ()
cleanid (Command name fname org mail twitter True False) = getContents
  >>= cleanOrg
  >>= cleanFirstName
  >>= cleanName
  >>= cleanMail
  >>= cleanTwitter
  >>= putStr
  where
    cleanOrg stdin = return $ T.unpack $ T.replace (T.pack org) "@ORG@" (T.pack stdin)
    cleanFirstName stdin = return $ T.unpack $ T.replace (T.pack fname) "@FIRSTNAME@" (T.pack stdin)
    cleanName stdin = return $ T.unpack $ T.replace (T.pack name) "@NAME@" (T.pack stdin)
    cleanMail stdin = return $ T.unpack $ T.replace (T.pack mail) "@MAIL@" (T.pack stdin)
    cleanTwitter stdin = return $ T.unpack $ T.replace (T.pack mail) "@TWITTER@" (T.pack stdin)

cleanid (Command name fname org mail twitter False True) = getContents
  >>= smudgeOrg
  >>= smudgeFirstName
  >>= smudgeName
  >>= smudgeMail
  >>= putStr
  where
    smudgeOrg stdin = return $ T.unpack $ T.replace "@ORG@" (T.pack org) (T.pack stdin)
    smudgeFirstName stdin = return $ T.unpack $ T.replace "@FIRSTNAME@" (T.pack fname) (T.pack stdin)
    smudgeName stdin = return $ T.unpack $ T.replace "@NAME@" (T.pack name) (T.pack stdin)
    smudgeMail stdin = return $ T.unpack $ T.replace "@MAIL@" (T.pack mail) (T.pack stdin)
    smudgeTwitter stdin = return $ T.unpack $ T.replace "@TWITTER@" (T.pack mail) (T.pack stdin)

cleanid (Command _ _ _ _ _ False False) = return ()
cleanid (Command _ _ _ _ _ True True) = return ()
