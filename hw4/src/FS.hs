{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module FS where

import Control.Lens
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath  (takeFileName, replaceExtension, splitPath, (</>))


data FS
    = Dir
          { _name     :: FilePath  -- название папки, не полный путь
          , _contents :: [FS]
          }
    | File
          { _name     :: FilePath  -- название файла, не полный путь
          } deriving (Eq, Show)

getDirectory :: FilePath -> IO FS
getDirectory dir = do
    isFile <- doesFileExist dir
    isDir <- doesDirectoryExist dir
    if isFile then return $ File $ takeFileName dir
    else if isDir then do
        let dirName = last $ splitPath dir
        fss <- listDirectory dir >>= mapM (getDirectory . (</>) dir)
        return $ Dir (last $ splitPath dirName) fss
    else error $ "No such file or directory " ++ dir

makeLenses ''FS
makePrisms ''FS

cd :: FilePath -> Traversal' FS FS
cd dir = contents.traversed.filtered(\x -> isn't _File x && x ^. name == dir)

ls :: Traversal' FS FilePath
ls = contents.traversed.name

file :: String -> Traversal' FS String
file s = contents.traversed.filtered(\x -> isn't _Dir x && x ^. name == s).name

--------------------------------------------------------------------

myReplaceExtension :: FS -> String -> FS
myReplaceExtension fs newExtension = fs & contents.traversed.filtered(isn't _Dir).name %~ flip replaceExtension newExtension

lsRec :: FS -> [FilePath]
lsRec dir = do
    let curr = dir ^.. ls
    let rec = concatMap lsRec (dir ^.. contents.traversed)
    curr ++ rec

rm :: FS -> FilePath -> FS
rm fs dir = fs & contents .~ (fs ^.. contents.traversed.filtered(\x -> x^.name /= dir || x^.contents /= []))

move :: FilePath -> Traversal' FS FS
move path func obj = do
        let nom = obj^.name
        func (obj & cd path.name %~ (nom </>))
