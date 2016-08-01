{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | cmdargs definitions for reesd-build.
module Reesd.Commands.Build where

-- TODO write artifacts file
-- TODO Respect branch/tag names.

import Control.Monad (mzero, when)
import Data.Aeson
  ( decode, encode, object, Value(Object), FromJSON(..), ToJSON(..), (.:)
  , (.=), (.:?) )
import qualified Data.ByteString.Lazy as LB
import Data.Either (isRight, rights)
import Data.Version (showVersion)
import Paths_reesd_builder (version)
import Network.Wreq (post)
import System.Console.CmdArgs.Explicit
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive, renameFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath (dropFileName, (</>), (<.>))
import System.FilePath.Find
import System.Process (readProcessWithExitCode)

import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>))
import qualified Data.ByteString.Char8 as BC
import Data.Attoparsec.ByteString (parseOnly, takeByteString, (<?>), Parser)
import qualified Data.Attoparsec.ByteString.Char8 as C


------------------------------------------------------------------------------
-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "reesd-build " ++ showVersion version ++ " - Copyright 2016 Vo Minh Thu."


------------------------------------------------------------------------------
-- | Process the command-line choice.
processCmd :: Cmd -> IO ()
processCmd Help = print (helpText [] HelpFormatDefault buildModes)

processCmd Version = putStrLn versionString

processCmd None = do
  processCmd Version
  processCmd Help

processCmd CmdClone{..} = do
  let mgitUrl = parseOnly gitUrlParser (BC.pack cmdRepository)
      mgitUrls = map (parseOnly graftParser . BC.pack) cmdGrafts
  case (mgitUrl, all isRight mgitUrls) of
    (Left err, _) -> putStrLn err
    (_, False) -> error "TODO"
    (Right gu@GitUrl{..}, True) -> do
      cloneOrUpdate gu
      mapM_ cloneOrUpdate (takeGitUrlGrafts (rights mgitUrls))


processCmd CmdBuild{..} = do
  let mgitUrl = parseOnly gitUrlParser (BC.pack cmdRepository)
      mgitUrls = map (parseOnly graftParser . BC.pack) cmdGrafts
  case (mgitUrl, all isRight mgitUrls) of
    (Left err, _) -> putStrLn err
    (_, False) -> error "TODO"
    (Right gu@GitUrl{..}, True) ->
      build cmdChannel cmdClone gu (rights mgitUrls) cmdImage cmdDockerfile cmdMangleFrom cmdCache

processCmd CmdInput{..} = do
  content <- LB.readFile "/input.json"
  case decode content of
    Nothing -> putStrLn "Can't decode stdin input."
    Just BuildInput{..} ->
      build inChannel True inGitUrl inGrafts inImage inDockerfile inMangleFrom {- TODO -} inCache


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
    , cmdDockerfile :: String
    , cmdMangleFrom :: Maybe String
    , cmdClone :: Bool
    , cmdCache :: Bool
    , cmdChannel :: String
    }
  | CmdInput
  | Help
  | Version
  | None
  deriving Show

buildModes :: Mode Cmd
buildModes = (modes "reesd-build" None "Build Dockerfiles."
  [ buildCloneMode
  , buildBuildMode
  , buildInputMode
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

buildInputMode :: Mode Cmd
buildInputMode = mode' "input" buildInput
  "Build a Dockerfile."
  []

buildClone = CmdClone
  { cmdRepository = ""
  , cmdGrafts = []
  }

buildBuild = CmdBuild
  { cmdRepository = ""
  , cmdGrafts = []
  , cmdImage = ""
  , cmdDockerfile = "Dockerfile"
  , cmdMangleFrom = Nothing
  , cmdClone = False
  , cmdCache = False
  , cmdChannel = "#general"
  }

buildInput = CmdInput

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
  , flagReq ["dockerfile"]
      (\x r -> Right (r { cmdDockerfile = x }))
      "DOCKERFILE"
      "Path to the Dockerfile to build."
  , flagReq ["mangle-from"]
      (\x r -> Right (r { cmdMangleFrom = Just x }))
      "TAG"
      "Alter the FROM Dockerfile instruction by adding the given tag."
  , flagBool ["clone"]
      (\x r -> r { cmdClone = x })
      "Clone repositories (don't assume they already exist)."
  , flagBool ["cache"]
      (\x r -> r { cmdCache = x })
      "Don't pass --no-cache to docker build."
  , flagReq ["channel"]
      (\x r -> Right (r { cmdChannel = x }))
      "CHANNEL"
      "Slack channel where to post (default to #general)."
  ]


------------------------------------------------------------------------------
build channel clone gu grafts image dockerfile mangle cache = do
  -- TODO Sanitize input, e.g. branch name.
  let branch = gitUrlBranch gu
      tag = if branch == "master" then "latest" else branch
      imagename = if ':' `elem` image then image else image ++ ":" ++ tag

      ggrafts = takeGitUrlGrafts grafts
      lgrafts = takeLocalGrafts grafts

  hasArtifactsDir <- doesDirectoryExist "/home/worker/artifacts"
  when hasArtifactsDir $ do
    LB.writeFile "/home/worker/artifacts/artifacts.json" (encode $ object ["tag" .= ("failure" :: String)])

  when clone $ do
    b <- cloneOrUpdate gu
    bs <- mapM cloneOrUpdate ggrafts

    when (any (== False) (b:bs)) $ do
      maybeNotifySlack channel (gitUrlRepository gu) (gitUrlBranch gu) imagename Nothing (Just "Can't clone repository.")
      error "Can't clone repository."

  e <- doesDirectoryExist "/home/worker/checkout"
  when e (removeDirectoryRecursive "/home/worker/checkout")

  commit <- checkout Nothing gu
  dockerfiles <- find always (fileName ==? "Dockerfile") "/home/worker/checkout"
  putStrLn "Found Dockerfile:"
  mapM_ (putStrLn . ("  " ++)) dockerfiles

  mapM_ (checkout (Just branch)) ggrafts
  mapM_ copyLocalGraft lgrafts

  b <- buildDockerfile gu imagename dockerfile mangle cache commit
  when (not b) $ do
    maybeNotifySlack channel (gitUrlRepository gu) (gitUrlBranch gu) imagename (Just commit) (Just "Can't build Dockerfile.")
    error "Can't build Dockerfile."

  b <- maybePushImage imagename
  when (not b) $ do
    maybeNotifySlack channel (gitUrlRepository gu) (gitUrlBranch gu) imagename (Just commit) (Just "Can't push image.")
    error "Can't push image."

  when hasArtifactsDir $ do
    LB.writeFile "/home/worker/artifacts/artifacts.json" (encode $ object ["tag" .= ("success" :: String)])

  maybeNotifySlack channel (gitUrlRepository gu) (gitUrlBranch gu) imagename (Just commit) Nothing


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
  code <- if e
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
      return code
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
      return code

  -- Restore original keys if any.
  when f (renameFile "/home/worker/.ssh/id_rsa.original" "/home/worker/.ssh/id_rsa")
  when f' (renameFile "/home/worker/.ssh/id_rsa.pub.original" "/home/worker/.ssh/id_rsa.pub")
  when f'' (renameFile "/home/worker/.ssh/known_hosts.original" "/home/worker/.ssh/known_hosts")

  case code of
    ExitSuccess -> return True
    ExitFailure _ -> return False


-- | Checkout a Git repository to /home/worker/checkout. If `graft` is Just,
-- then create the checkout in a sub-directory. The goal is to have the main
-- Dockerfile "see" the graft checkouts and include them in its build context.
-- The value within Just is used as a branch. If the branch doesn't exist, the
-- GitUrl is used as-is.
checkout :: Maybe String -> GitUrl -> IO String
checkout mgraft GitUrl{..} = do
  let dir_ = "/home/worker/checkout"
      dir = if mgraft /= Nothing then (dir_ </> gitUrlRepository) else  dir_
  -- The work-tree directory must exist before calling `git checkout`.
  createDirectoryIfMissing False dir

  br <- case mgraft of
    Nothing -> return gitUrlBranch
    Just branch -> do
      (code, out, err) <- readProcessWithExitCode "git"
        [ "--git-dir", "/home/worker/gits" </> gitUrlRepository <.> "git"
        , "--work-tree", dir
        , "rev-parse", "--verify", branch
        ]
        ""
      case code of
        ExitSuccess -> return branch
        _ -> return gitUrlBranch

  putStrLn ("Creating checkout of branch " ++ br ++ "...")
  (code, out, err) <- readProcessWithExitCode "git"
    [ "--git-dir", "/home/worker/gits" </> gitUrlRepository <.> "git"
    , "--work-tree", dir
    , "checkout", br, "--", "."
    ]
    ""
  putStrLn out
  putStrLn err

  putStrLn ("Reading branch SHA1...")
  (code, out, err) <- readProcessWithExitCode "git"
    [ "--git-dir", "/home/worker/gits" </> gitUrlRepository <.> "git"
    , "rev-parse", gitUrlBranch
    ]
    ""
  putStrLn out
  putStrLn err
  return (head (words out)) -- TODO

-- | Same as checkout but for local directories instead of repositories.
copyLocalGraft :: FilePath -> IO ()
copyLocalGraft p = do
  let dir_ = "/home/worker/checkout"
  (_, out, err) <- readProcessWithExitCode "cp"
    [ "-r", p, dir_
    ]
    ""
  putStrLn out
  putStrLn err

-- | Run `docker build`. The Dockerfile path is given relative to
-- /home/worker/checkout.
buildDockerfile gu@GitUrl{..} imagename dockerfile mangle cache commit = do
  let dir_ = "/home/worker/checkout"

  -- TODO Test whether there is a Dockerfile.
  f <- doesFileExist (dir_ </> dockerfile)
  if f
    then do
      writeBuildInfo gu imagename dockerfile commit
      _ <- maybe (return True) (mangleDockerfile (dir_ </> dockerfile)) mangle -- TODO test return value
      appendFile (dir_ </> dockerfile) "ADD BUILD-INFO /"

      -- Run docker build.
      (code, out, err) <- readProcessWithExitCode "sudo"
        ([ "docker", "build"
        , "-f", (dir_ </> dockerfile)
        , "--force-rm"
        ] ++
        (if cache then [] else ["--no-cache"]) ++
        [ "-t", imagename
        , dir_ </> dropFileName dockerfile
        ])
        ""
      putStrLn out
      putStrLn err
      case code of
        ExitSuccess -> return True
        ExitFailure _ -> return False

    else do
      putStrLn ("Can't find Dockerfile " ++ (dir_ </> dockerfile) ++ ".")
      return False

-- TODO Pretty-print the json.
writeBuildInfo GitUrl{..} imagename dockerfile commit = do
  let dir_ = "/home/worker/checkout"
  LB.writeFile (dir_ </> dropFileName dockerfile </> "BUILD-INFO")
    (encode BuildInfo
      { biRepository = "git@github.com:" ++
          (gitUrlUsername </> gitUrlRepository) ++ ".git"
      , biBranch = gitUrlBranch
      , biCommit = commit
      , biImage = imagename
      })

-- | Modify the FROM instruction to specify a given tag.
mangleDockerfile path tag = do
  content <- BC.readFile path
  case parseOnly dockerfileParser content of
    Left err -> putStrLn ("Can't parse Dockerfile " ++ path) >> return False
    Right ints -> do
      let ints' = map mangleFrom ints
          content' = formatDockerfile ints'
      BC.writeFile (path ++ ".mangle") content'
      renameFile (path ++ ".mangle") path
      return True

  where mangleFrom (FromInstruction n _) = FromInstruction n (Just tag)
        mangleFrom int = int

-- | Possibly push an image if credentials are given in /home/worker/.dockercfg.
maybePushImage imagename = do
  f <- doesFileExist "/home/worker/.dockercfg"
  if f
    then do
      putStrLn ("Pushing image " ++ imagename ++ "...")
      (code, out, err) <- readProcessWithExitCode "sudo"
        [ "docker", "push", imagename
        ]
        ""
      putStrLn out
      putStrLn err
      case code of
        ExitSuccess -> return True
        ExitFailure _ -> return False
    else return True

maybeNotifySlack channel repository branch imagename mcommit merror = do
  slackHookUrl <- lookupEnv "SLACK_HOOK_URL"
  case slackHookUrl of
    Nothing -> return ()
    Just hookUrl -> do
      putStrLn ("Notifying Slack for " ++ imagename ++ "...")
      let sn = slackNotification channel repository branch imagename mcommit merror
      -- TODO This can raise an exception if the URL is invalid.
      post hookUrl (toJSON sn)
      return ()


------------------------------------------------------------------------------
-- | Dockerfile instructions
data Instruction =
    FromInstruction String (Maybe String) -- ^ Image name + optional tag.
  | Other String -- ^ A line as-is.
  deriving Show

fromParser = FromInstruction
  <$> ((C.string "from " <|> C.string "FROM ")
  *> (BC.unpack <$> C.takeWhile1 (C.inClass "-a-zA-Z0-9_./" )))
  <*> (C.option Nothing
    (C.char ':' *> ((Just . BC.unpack) <$> C.takeWhile1 (C.inClass "-a-zA-Z0-9_." ))))
  <* C.char '\n'

otherParser = Other
  <$> (BC.unpack <$> C.takeTill (== '\n'))
  <* C.char '\n'

dockerfileParser :: Parser [Instruction]
dockerfileParser = C.many' (fromParser <|> otherParser)

formatDockerfile :: [Instruction] -> BC.ByteString
formatDockerfile ints =
  let ints' = map f ints
  in BC.unlines ints'
  where f (FromInstruction n t) = BC.unwords (["FROM", BC.pack (n ++ maybe "" (":" ++) t)])
        f (Other s) = BC.pack s

------------------------------------------------------------------------------
data GitUrl = GitUrl
  { gitUrlUsername :: String
  , gitUrlRepository :: String
  , gitUrlBranch :: String
  }
  deriving Show

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

formatGitUrl GitUrl{..} =
  "git@github.com:" ++ gitUrlUsername ++ "/" ++ gitUrlRepository
  ++ ".git#" ++ gitUrlBranch

parseGitUrl = parseOnly gitUrlParser . BC.pack


------------------------------------------------------------------------------
data Graft =
    GitUrlGraft GitUrl -- ^ The graft is provided by a GitHub repository.
  | LocalGraft FilePath -- ^ The graft is provided by a local directory.
  deriving Show

localParser = do
  c <- (C.char '/')
  cs <- takeByteString
  return (BC.unpack (c `BC.cons` cs))

graftParser :: Parser Graft
graftParser =
  (GitUrlGraft <$> gitUrlParser)
  <|> (LocalGraft <$> localParser)

formatGraft (GitUrlGraft gu) = formatGitUrl gu
formatGraft (LocalGraft p) = p

parseGraft = parseOnly graftParser . BC.pack

takeGitUrlGrafts :: [Graft] -> [GitUrl]
takeGitUrlGrafts [] = []
takeGitUrlGrafts (LocalGraft _:gs) = takeGitUrlGrafts gs
takeGitUrlGrafts (GitUrlGraft g:gs) = g : takeGitUrlGrafts gs

takeLocalGrafts :: [Graft] -> [FilePath]
takeLocalGrafts [] = []
takeLocalGrafts (LocalGraft p:gs) = p : takeLocalGrafts gs
takeLocalGrafts (GitUrlGraft _:gs) = takeLocalGrafts gs

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
-- | This is similar to the `build` command flags (i.e. `--repo`, `--graft`).
-- `--clone` is implied. Grafts are given as a list of repositories
-- For example:
-- { "image": "images.reesd.com/reesd/hello",
--   "repository": "git@github.com:hypered/reesd-hello.git#master"
-- }
data BuildInput = BuildInput
  { inGitUrl :: GitUrl
  , inBranch :: String -- TODO This overrides the branch in inGitUrl, maybe a GitUrl variant without the branch would be better.
  , inImage :: String
  , inGrafts :: [Graft]
  , inDockerfile :: String
  , inMangleFrom :: Maybe String
  , inCache :: Bool
  , inChannel :: String
  }
  deriving Show

instance ToJSON BuildInput where
  toJSON BuildInput{..} = object $
    [ "repository" .= formatGitUrl inGitUrl
    , "branch" .= gitUrlBranch inGitUrl
    , "image" .= inImage
    , "grafts" .= map formatGraft inGrafts
    , "dockerfile" .= inDockerfile
    , "cache" .= inCache
    , "channel" .= inChannel
    ] ++
    (maybe [] ((:[]) . ("mangle-from" .=)) inMangleFrom)

instance FromJSON BuildInput where
  parseJSON (Object v) = do
    repository <- v .: "repository"
    mbranch <- v .:? "branch"
    image <- v .: "image"
    mgrafts <- v.:? "grafts"
    mdockerfile <- v.:? "dockerfile"
    mmangle <- v.:? "mangle-from"
    mcache <- v.:? "cache"
    mchannel <- v.:? "channel"

    let mgitUrl = parseOnly gitUrlParser (BC.pack repository)
        mgitUrls = maybe [] (map (parseOnly graftParser . BC.pack)) mgrafts
    case (mgitUrl, all isRight mgitUrls) of
      (Left err, _) -> mzero
      (_, False) -> mzero
      (Right gu, True) -> return BuildInput
        { inGitUrl = maybe gu (\b -> gu { gitUrlBranch = b }) mbranch
        , inBranch = maybe (gitUrlBranch gu) id mbranch
        , inImage = image
        , inGrafts = rights mgitUrls
        , inDockerfile = maybe "Dockerfile" id mdockerfile
        , inMangleFrom = mmangle
        , inCache = maybe False id mcache
        , inChannel = maybe "#general" id mchannel
        }
  parseJSON _ = mzero

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

-- `channel` can be e.g. "@thu" or "#general".
slackNotification channel repository branch imagename mcommit merror = SlackNotification
  { snChannel = channel
  , snUsername = "Zoidberg"
  , snIconEmoji = ":zoidberg:"
  , snText = "A new Docker image is available:"
  , snAttachments =
    [ SlackAttachment
      { saColor = maybe "#7CF197" (const "#FF0000") merror
      , saFields =
        (maybe [] (\err -> [ SlackField
          { sfMaybeTitle = Just "Error"
          , sfValue = err
          , sfShort = False
          }
        ]) merror)
        ++
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
        ]
        ++
        (maybe [] (\commit -> [ SlackField
          { sfMaybeTitle = Just "Commit"
          , sfValue = commit
          , sfShort = False
          }
        ]) mcommit)
        ++
        [ SlackField
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
