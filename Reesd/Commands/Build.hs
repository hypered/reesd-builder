{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | cmdargs definitions for reesd-build.
module Reesd.Commands.Build where

-- TODO docker push (done but check if it works).
-- TODO slack notification
-- TODO write artifacts file
-- TODO Read arguments as json on stdin.
-- TODO Respect branch/tag names.

import Control.Monad (when)
import Data.Aeson
  ( encode, fromJSON, object, Value(Object), Result(..), FromJSON(..), ToJSON(..), (.:)
  , (.=) )
import qualified Data.ByteString.Lazy as LB
import Data.Either (isRight, rights)
import Data.Version (showVersion)
import Paths_reesd_builder (version)
import System.Console.CmdArgs.Explicit
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive, renameFile)
import System.FilePath ((</>), (<.>))
import System.FilePath.Find
import System.Process (rawSystem, readProcessWithExitCode)

import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Attoparsec.ByteString (Parser, many', parseOnly, (<?>))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as C


------------------------------------------------------------------------------
-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "reesd-build " ++ showVersion version ++ " - Copyright 2016 Vo Minh Thu."


------------------------------------------------------------------------------
-- | Process the command-line choice.
-- TODO Move the runCmd implementation in another module.
processCmd :: Cmd -> IO ()
processCmd Help = print (helpText [] HelpFormatDefault buildModes)

processCmd Version = putStrLn versionString

processCmd None = do
  processCmd Version
  processCmd Help

processCmd cmd = runCmd cmd


------------------------------------------------------------------------------
-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdClone
    { cmdRepository :: String
    , cmdGrafts :: [String]
    }
  | CmdBuild
    { cmdRepository :: String
    , cmdGrafts :: [String]
    , cmdImage :: String
    , cmdClone :: Bool
    }
  | Help
  | Version
  | None
  deriving Show

buildModes :: Mode Cmd
buildModes = (modes "reesd-build" None "Build Dockerfiles."
  [ buildCloneMode
  , buildBuildMode
  ])
  { modeGroupFlags = toGroup
    [ flagHelpSimple (const Help)
    , flagVersion (const Version)
    ]
  }

buildCloneMode :: Mode Cmd
buildCloneMode = mode' "clone" buildClone
  "Clone a Git repository."
  buildCommonFlags

buildBuildMode :: Mode Cmd
buildBuildMode = mode' "build" buildBuild
  "Build a Dockerfile."
  (buildCommonFlags ++ buildBuildFlags)

buildClone = CmdClone
  { cmdRepository = ""
  , cmdGrafts = []
  }

buildBuild = CmdBuild
  { cmdRepository = ""
  , cmdGrafts = []
  , cmdImage = ""
  , cmdClone = False
  }

buildCommonFlags =
  [ flagReq ["repository"]
      (\x r -> Right (r { cmdRepository = x }))
      "REPOSITORY"
      "Repository name."
  , flagReq ["graft"]
      (\x r -> Right (r { cmdGrafts = cmdGrafts r ++ [x] }))
      "REPOSITORY"
      "Additional repository to checkout inside the main one."
  ]

buildBuildFlags =
  [ flagReq ["image"]
      (\x r -> Right (r { cmdImage = x }))
      "IMAGE"
      "Image name."
  , flagBool ["clone"]
      (\x r -> r { cmdClone = x })
      "Clone repositories (don't assume they already exist)."
  ]


runCmd :: Cmd -> IO ()
runCmd CmdClone{..} = do
  let mgitUrl = parseOnly gitUrlParser (BC.pack cmdRepository)
      mgitUrls = map (parseOnly gitUrlParser . BC.pack) cmdGrafts
  case (mgitUrl, all isRight mgitUrls) of
    (Left err, _) -> putStrLn err
    (_, False) -> error "TODO"
    (Right gu@GitUrl{..}, True) -> do
      cloneOrUpdate gu
      mapM_ cloneOrUpdate (rights mgitUrls)


runCmd CmdBuild{..} = do
  let mgitUrl = parseOnly gitUrlParser (BC.pack cmdRepository)
      mgitUrls = map (parseOnly gitUrlParser . BC.pack) cmdGrafts
  case (mgitUrl, all isRight mgitUrls) of
    (Left err, _) -> putStrLn err
    (_, False) -> error "TODO"
    (Right gu@GitUrl{..}, True) -> do

      when cmdClone $ do
        cloneOrUpdate gu
        mapM_ cloneOrUpdate (rights mgitUrls)

      -- TODO Exit if clone/update failed.

      e <- doesDirectoryExist "/home/worker/checkout"
      when e (removeDirectoryRecursive "/home/worker/checkout")

      checkout False gu
      dockerfiles <- find always (fileName ==? "Dockerfile") "/home/worker/checkout"
      putStrLn "Found Dockerfile:"
      mapM_ (putStrLn . ("  " ++)) dockerfiles

      mapM_ (checkout True) (rights mgitUrls)

      buildDockerfile gu cmdImage

      -- TODO Exit if build failed.

      maybePushImage cmdImage

      -- TODO Exit if push failed.

      maybeNotifySlack gitUrlRepository gitUrlBranch cmdImage


------------------------------------------------------------------------------
-- | Clone a repository, or if it was already clone, update it.
-- The clone is in /home/worker/gits/<repository-name>.
-- To clone from GitHub, SSH keys are expected to be found in
-- /home/worker/ssh-keys/<repository-name>/id_rsa.
cloneOrUpdate GitUrl{..} = do
      putStrLn ("Cloning/updating GitHub repository " ++ gitUrlUsername ++ "/" ++ gitUrlRepository ++ "...")

      -- Try to not overwrite existing keys.
      -- TODO Use exception brackets.
      f <- doesFileExist "/home/worker/.ssh/id_rsa"
      f' <- doesFileExist "/home/worker/.ssh/id_rsa.pub"
      f'' <- doesFileExist "/home/worker/.ssh/known_hosts"
      when f (renameFile "/home/worker/.ssh/id_rsa" "/home/worker/.ssh/id_rsa.original")
      when f' (renameFile "/home/worker/.ssh/id_rsa.pub" "/home/worker/.ssh/id_rsa.pub.original")
      when f'' (renameFile "/home/worker/.ssh/known_hosts" "/home/worker/.ssh/known_hosts.original")

      copyFile ("/home/worker/ssh-keys" </> gitUrlRepository </> "id_rsa") "/home/worker/.ssh/id_rsa"
      copyFile ("/home/worker/ssh-keys/known_hosts") "/home/worker/.ssh/known_hosts"

      e <- doesDirectoryExist ("/home/worker/gits" </> gitUrlRepository <.> "git")
      if e
        then do
          -- Update when the repository has already been cloned.
          putStrLn ("/home/worker/gits" </> gitUrlRepository <.> "git" ++ " exists, fetching updates...")
          (code, out, err) <- readProcessWithExitCode "git"
            [ "--git-dir", "/home/worker/gits" </> gitUrlRepository <.> "git"
            , "fetch", "-q", "--tags"
            ]
            ""
          putStrLn out
          putStrLn err
        else do
          -- Clone when it is new.
          putStrLn ("/home/worker/gits" </> gitUrlRepository <.> "git" ++ " doesn't exist, cloning...")
          (code, out, err) <- readProcessWithExitCode "git"
            [ "clone", "--mirror", "-q"
            , "git@github.com:" ++ gitUrlUsername </> gitUrlRepository
            , "/home/worker/gits" </> gitUrlRepository <.> "git"
            ]
            ""
          putStrLn out
          putStrLn err

      -- Restore original keys if any.
      when f (renameFile "/home/worker/.ssh/id_rsa.original" "/home/worker/.ssh/id_rsa")
      when f' (renameFile "/home/worker/.ssh/id_rsa.pub.original" "/home/worker/.ssh/id_rsa.pub")
      when f'' (renameFile "/home/worker/.ssh/known_hosts.original" "/home/worker/.ssh/known_hosts")


-- | Checkout a Git repository to /home/worker/checkout. If `graft` is True,
-- then create the checkout in a sub-directory. The goal is to have the main
-- Dockerfile "see" the graft checkouts and include them in its build context.
checkout :: Bool -> GitUrl -> IO ()
checkout graft GitUrl{..} = do
      putStrLn ("Creating checkout...")
      let dir_ = "/home/worker/checkout"
          dir = if graft then (dir_ </> gitUrlRepository) else  dir_
      -- The work-tree directory must exist before calling `git checkout`.
      createDirectoryIfMissing False dir
      (code, out, err) <- readProcessWithExitCode "git"
        [ "--git-dir", "/home/worker/gits" </> gitUrlRepository <.> "git"
        , "--work-tree", dir
        , "checkout", gitUrlBranch, "--", "."
        ]
        ""
      putStrLn out
      putStrLn err

-- | Run `docker build`. The Dockerfile is assumed to be at the root of
-- /home/worker/checkout.
buildDockerfile GitUrl{..} imagename = do
      let dir_ = "/home/worker/checkout"

      -- TODO Test whether there is a Dockerfile.

      -- Write a BUILD-INFO file.
      -- TODO Pretty-print the json.
      LB.writeFile (dir_ </> "BUILD-INFO") (encode BuildInfo
        { biRepository = "git@github.com:" ++
            (gitUrlUsername </> gitUrlRepository) ++ ".git"
        , biBranch = gitUrlBranch
        , biCommit = "TODO"
        , biImage = imagename
        })
      appendFile (dir_ </> "Dockerfile") "ADD BUILD-INFO /"

      -- Run docker build.
      -- TODO Give a parameter for --no-cache.
      (code, out, err) <- readProcessWithExitCode "sudo"
        [ "docker", "build", "--force-rm", "--no-cache", "-t", imagename, dir_
        ]
        ""
      putStrLn out
      putStrLn err

-- | Possibly push an image if credentials are given in /home/worker/.dockercfg.
maybePushImage imagename = do
  f <- doesFileExist "/home/worker/.dockercfg"
  when f $ do
    putStrLn ("Pushing image " ++ imagename ++ "...")
    (code, out, err) <- readProcessWithExitCode "sudo"
      [ "docker", "push", imagename
      ]
      ""
    putStrLn out
    putStrLn err

maybeNotifySlack repository branch imagename = do
  -- TODO Use env var instead.
  f <- doesFileExist "/slack-hook-url.txt"
  when f $ do
    putStrLn ("Notifying Slack for " ++ imagename ++ "...")
    hookUrl <- readFile "/slack-hook-url.txt"
    let sn = slackNotification repository branch imagename
    -- curl -s -X POST --data-urlencode payload@/tmp/payload.txt ${HOOK}
    -- post hookUrl (toJSON sn)
    return ()

------------------------------------------------------------------------------
data GitUrl = GitUrl
  { gitUrlUsername :: String
  , gitUrlRepository :: String
  , gitUrlBranch :: String
  }

gitUrlParser = GitUrl
  <$> ((C.string "git@github.com" <?> "GitHub git/ssh only for now")
  *> (C.char ':' <?> "Missing colon")
  *> (BC.unpack <$> C.takeWhile1 (C.inClass "-a-zA-Z0-9" )))
  <* (C.char '/')
  <*> (BC.unpack <$> C.takeWhile1 (C.inClass "-a-zA-Z0-9" ))
  <* (C.option ".git" (C.string ".git"))
  <*> (C.option "master"
    (C.char '#' *> (BC.unpack <$> C.takeWhile1 (C.inClass "-a-zA-Z0-9" ))))
  <* C.endOfInput


------------------------------------------------------------------------------
data BuildInfo = BuildInfo
  { biRepository :: String
  , biBranch :: String
  , biCommit :: String
  , biImage :: String
  }
  deriving Show

instance ToJSON BuildInfo where
  toJSON BuildInfo{..} = object
    [ "repository" .= biRepository
    , "branch" .= biBranch
    , "commit" .= biCommit
    , "image" .= biImage
    ]


------------------------------------------------------------------------------
data SlackNotification = SlackNotification
  { snChannel :: String
  , snUsername :: String
  , snIconEmoji :: String
  , snText :: String
  , snAttachments :: [SlackAttachment]
  }

data SlackAttachment = SlackAttachment
  { saColor :: String
  , saFields :: [SlackField]
  , saMarkdownIn :: [String]
  }

data SlackField = SlackField
  { sfMaybeTitle :: Maybe String
  , sfValue :: String
  , sfShort :: Bool
  }

instance ToJSON SlackNotification where
  toJSON SlackNotification{..} = object
    [ "channel" .= snChannel
    , "username" .= snUsername
    , "icon_emoji" .= snIconEmoji
    , "text" .= snText
    , "attachments" .= snAttachments
    ]

instance ToJSON SlackAttachment where
  toJSON SlackAttachment{..} = object
    [ "color" .= saColor
    , "fields" .= saFields
    , "mrkdwn_in" .= saMarkdownIn
    ]

instance ToJSON SlackField where
  toJSON SlackField{..} = object $
      maybe [] ((:[]) . ("title" .=)) sfMaybeTitle ++
      ["value" .= sfValue] ++
      if sfShort then ["short" .= True] else []

slackNotification repository branch imagename = SlackNotification
  { snChannel = "@thu"
  , snUsername = "Zoidberg"
  , snIconEmoji = ":zoidberg:"
  , snText = "A new Docker image is available:"
  , snAttachments =
    [ SlackAttachment
      { saColor = "#7CF197"
      , saFields =
        [ SlackField
          { sfMaybeTitle = Just "Repository"
          , sfValue = repository
          , sfShort = True
          }
        , SlackField
          { sfMaybeTitle = Just "Branch"
          , sfValue = branch
          , sfShort = True
          }
        , SlackField
          { sfMaybeTitle = Nothing
          , sfValue = ("```docker pull " ++ imagename ++ "```")
          , sfShort = False
          }
        ]
      , saMarkdownIn = ["fields"]
      }
    ]
  }


------------------------------------------------------------------------------
-- | Same as `mode` but without an `Arg a` argument.
mode' :: Name -> a -> Help -> [Flag a] -> Mode a
mode' name value help flags = (modeEmpty value)
  { modeNames = [name]
  , modeHelp = help
  , modeArgs = ([], Nothing)
  , modeGroupFlags = toGroup flags
  }
