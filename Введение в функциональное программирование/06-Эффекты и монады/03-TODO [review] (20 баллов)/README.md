# TODO [review]

Напишите программу для работы со списком задач. Хранить задачи нужно в виде файлов в папке `rootFolder`. Вы можете сами выбрать формат имени и содержимого файлов.

Вам понадобятся
- [System.Directory](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
- [Работа с файлами в IO](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)

Скачать задачу можно [здесь](TodoList.zip).


Все тесты пройдены, решение ожидает код-ревью:
```hs
{-# LANGUAGE DuplicateRecordFields #-}

module TodoList where

import System.Directory
import System.IO
import System.FilePath
import Data.List
import System.Random

newtype TodoList = TodoList FilePath deriving (Eq, Show)
newtype Id = Id String deriving (Eq, Show, Read)
newtype Title = Title String deriving (Eq, Show, Read)
newtype Deadline = Deadline String deriving (Eq, Show, Read, Ord)
newtype Content = Content String deriving (Eq, Show, Read)

data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show, Read)

instance Ord Todo where
  compare (Todo _ _ _ deadline1 _) (Todo _ _ _ deadline2 _) = compare deadline1 deadline2

data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  createDirectory rootFolder
  return $ TodoList rootFolder

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo (TodoList rootFolder) title content deadline = do
  (fileName, file) <- openTempFile rootFolder "" 
  let
    todoId = Id $ takeFileName fileName
  hPutStr file $ show $ Todo todoId title content deadline False
  hClose file
  return todoId

getTodoFilePath :: TodoList -> Id -> FilePath
getTodoFilePath (TodoList rootFolder) (Id fileName) = joinPath [rootFolder, fileName]

readTodoStr :: TodoList -> Id -> IO String
readTodoStr todoList todoId = readFile $ getTodoFilePath todoList todoId

readTodo :: TodoList -> Id -> IO Todo
readTodo todoList todoId = do
  todo <- readTodoStr todoList todoId
  return $ read todo

showTodo :: TodoList -> Id -> IO ()
showTodo todoList todoId = do
  todo <- readTodoStr todoList todoId
  putStrLn todo

removeTodo :: TodoList -> Id -> IO ()
removeTodo todoList todoId = removeFile $ getTodoFilePath todoList todoId

replaceTodo :: TodoList -> Id -> Todo -> IO ()
replaceTodo todoList@(TodoList rootFolder) todoId todo = do
  (tempName, tempFile) <- openTempFile rootFolder "temp"
  hPutStr tempFile $ show todo
  hClose tempFile 
  let
    filePath = getTodoFilePath todoList todoId
  removeFile filePath
  renameFile tempName filePath

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList todoId (TodoEdit title content deadline) = do
  todo <- readTodo todoList todoId
  replaceTodo todoList todoId todo {title = title, content = content, deadline = deadline}

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList todoId = do
  todo <- readTodo todoList todoId
  replaceTodo todoList todoId todo {isDone = True}

readAllTodo :: TodoList -> IO [Todo]
readAllTodo todoList@(TodoList rootFolder) = do
  files <- listDirectory rootFolder
  todo <- mapM (readTodo todoList . Id) files
  return $ sort todo

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
  allTodo <- readAllTodo todoList
  return $ filter isUnfinished allTodo
  where
    isUnfinished (Todo _ _ _ _ isDone) = not isDone

showTodos :: [Todo] -> IO ()
showTodos = mapM_ $ putStrLn . show

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = readAllTodo todoList >>= showTodos

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = readUnfinishedTodo todoList >>= showTodos
```
