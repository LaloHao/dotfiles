{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
module XMobar.Task where
import XMonad.Actions.ShowText
import Control.Monad.State
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Text (toLower, pack, unpack)
import Data.Text.Internal.Search
import Data.Char (isDigit)
import XMonad.Prompt
import XMonad.Prompt.Input
import Data.Unique
import Control.Lens hiding (indices)
import XMonad.Core (Typeable(..), ExtensionClass(..), StateExtension(..), X)
import qualified XMonad.Util.ExtensibleState as XS
-- import Text.ParserCombinators.Parsec
-- import           Text.ParsecT (Parser)
import           Text.Parsec
import           Text.Parsec.String (Parser)
import           Text.Parsec.Language
import           Text.Parsec.Token
import Data.Time.Clock

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser haskellDef

strParser :: Parser String
strParser = stringLiteral lexer

parseString :: String -> Either ParseError String
parseString = parse strParser ""

data TaskState =
    TaskStopped
  | TaskRunning
  deriving (Read, Show, Eq)

makePrisms ''TaskState

data Task = Task {
    _taskId   :: Int
  , _taskState :: TaskState
  , _taskName :: String
  , _taskTime :: Int
  , _taskLastTick :: Maybe UTCTime
  } deriving (Typeable, Read, Show)

makeLenses ''Task

-- parseTask :: Parser Task
-- parseTask = do
--   string "Task"
--   space
--   id <- many1 digit
--   space
--   st <- try string "TaskStopped" <|> string "TaskRunning"
--   space
--   name <- parseString
--   space
--   time <- many1 digit
--   return $ task id name time

-- instance Read Task where
--   readsPrec _ s = [(parseTask s, "")]

newtype TaskList = TaskList [Task] deriving (Typeable, Read, Show)

instance ExtensionClass TaskList where
  initialValue = TaskList []
  extensionType = PersistentExtension

task :: Int -> TaskState -> String -> Int -> Maybe UTCTime -> Task
task = Task

updateTask p f t | not (p t) = t
                 | otherwise = f t

updateTaskTimer :: UTCTime -> Task -> Task
updateTaskTimer now t@Task { _taskState = st, _taskTime = time, _taskLastTick = before }
  | st    == TaskStopped = t                                                       & taskLastTick .~ Nothing
  | time  == 0           = t                            & taskState .~ TaskStopped & taskLastTick .~ Nothing
  | otherwise            = t & taskTime  .~ left                                   & taskLastTick .~ Just now
  where left = time - diff
        diff = floor $ diffUTCTime now (fromJust before)

type MaybeX = MaybeT X

maybeRead :: String -> MaybeX String
maybeRead prompt = MaybeT (inputPrompt def prompt)

failIfEmpty :: (MonadFail m, Monad m) => String -> m ()
failIfEmpty str = when (str == "") (fail "Empty output")

failIfZero :: (MonadFail m, Monad m) => Int -> m ()
failIfZero n = when (n <= 0) (fail "Invalid time")

maybeReadTime :: MaybeT X Int
maybeReadTime = do
  time <- maybeRead "Task time"
  failIfEmpty time
  if all isDigit time
    then return $ read time
    else return 0

type TaskNames = [String]

createTaskT :: TaskNames -> MaybeT X Task
createTaskT tasks = do
  name' <- maybeRead "Task name"
  candidates <- liftIO $ mkCompletions' (==) tasks name'

  let μ    = not $ null candidates
      name = if μ then name' ++ " (1)" else name'

  failIfEmpty name
  time <- maybeReadTime
  failIfZero time
  tid <- liftIO newUnique
  return $ task (hashUnique tid) TaskStopped name time Nothing

createTask :: X ()
createTask = do
  tasks <- allTasks
  newTask <- runMaybeT (createTaskT (fmap _taskName tasks))
  case newTask of
    Nothing -> return ()
    (Just task) -> do
      XS.modify $ \(TaskList tasks) -> TaskList (tasks ++ [task])

clearTasks :: X ()
clearTasks = XS.put (TaskList [])

allTasks :: X [Task]
allTasks = do
  (TaskList tasks) <- XS.get
  return tasks

maybeReadTask :: String -> (String -> IO [String]) -> MaybeX String
maybeReadTask prompt compl = MaybeT (inputPromptWithCompl def prompt compl)

type Predicate = String -> String -> Bool

mkCompletions :: [String] -> String -> IO [String]
mkCompletions = mkCompletions' (∈)

mkCompletions' :: Predicate -> [String] -> String -> IO [String]
mkCompletions' _ tasks [] = return tasks
mkCompletions' f tasks s =
  return $ filter (f s) tasks
  -- return $ map unpack $ filter (s' ∈) tasks'
  -- where s' = toLower $ pack s
  --       tasks' = map (toLower . pack) tasks

(∈) :: Predicate
(∈) x c = not $ null $ indices x' c'
  where
    x' = toLower $ pack x
    c' = toLower $ pack c

infix ∈

type TaskAction = [String] -> UTCTime -> Task -> Task

withTaskPrompt :: String -> TaskAction -> X ()
withTaskPrompt prompt f = do
  tasks <- allTasks
  let
    taskNames :: [String]
    taskNames = _taskName <$> tasks
    compl = mkCompletions taskNames
  promptedTask <- runMaybeT (maybeReadTask prompt compl)
  case promptedTask of
    Nothing -> return ()
    (Just task) -> do
      completions <- liftIO $ compl task
      now <- liftIO getCurrentTime
      tasks <- allTasks
      let tasks' = fmap (updateTaskTimer now) tasks
      XS.modify $ \(TaskList tasks) -> TaskList $ fmap (f completions now) tasks'

byName = flip (∈) . _taskName

startTask :: X ()
startTask = withTaskPrompt "Iniciar tarea" $ \match now -> do
  updateTask ((head match ∈) . _taskName) $ \task -> do
    task & taskState    .~ TaskRunning
         & taskLastTick .~ Just now

stopTask :: X ()
stopTask = withTaskPrompt "Pausar tarea" $ \match _ -> do
  updateTask ((head match ∈) . _taskName) $ \task -> do
    task & taskState    .~ TaskStopped
         & taskLastTick .~ Nothing

-- stopTask :: X ()
-- stopTask = withTaskPrompt "Pausar tarea" $ \match -> updateTask ((head match ∈) . _taskName) (set taskState TaskStopped)

incTask :: X ()
incTask = do
  (Just time) <- runMaybeT maybeReadTime
  failIfZero time
  withTaskPrompt "Aumentar tiempo" $ \match _ -> updateTask ((head match ∈) . _taskName) (over taskTime (+ time))
