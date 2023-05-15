{-# LANGUAGE LambdaCase #-}

module Lib
  ( someFunc,
  )
where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Parser
import Language.Java.SourceSpan (dummySourceSpan)
import Language.Java.Syntax
import System.IO (IOMode (ReadMode), char8, hGetContents, hSetEncoding, openFile)

someFunc :: IO ()
someFunc = do
  let path1 = "/home/niklas/github/transform-AST/src/java/ExampleA.java"
  (path2, file) <- readFileCustom path1
  let ecUnit = parser compilationUnit path2 file
  case ecUnit of
    Right cUnit -> do
      print (testClass cUnit)
    Left _ -> putStrLn "parse failed"
  putStrLn "someFunc"

readFileCustom :: FilePath -> IO (FilePath, String)
readFileCustom path = do
  inputHandle <- openFile path ReadMode
  hSetEncoding inputHandle char8
  file <- hGetContents inputHandle
  return (path, file)

identsfromDecls :: [Decl p] -> [Ident]
identsfromDecls decls = do
  universeBi
    ( map
        (\(VarDecl _ id _) -> id)
        ( concatMap
            ( \case
                (MemberDecl (FieldDecl _ _ _ vars)) -> vars
                _ -> []
            )
            decls
        )
    )

classFields :: ClassDecl Parsed -> [Ident]
classFields (ClassDecl _ _ _ _ _ _ (ClassBody decls)) = identsfromDecls decls
classFields (RecordDecl _ _ _ _ _ _ (ClassBody decls)) = identsfromDecls decls
classFields (EnumDecl _ _ _ _ (EnumBody _ decls)) = identsfromDecls decls

identsFromParams :: [FormalParam p] -> [Ident]
identsFromParams params = do
  universeBi (map (\(FormalParam _ _ _ _ vardeclId) -> vardeclId) params)

methodParams :: MemberDecl Parsed -> [Ident]
methodParams (MethodDecl _ _ _ _ _ params _ _ _) = identsFromParams params
methodParams (ConstructorDecl _ _ _ _ params _ _) = identsFromParams params
methodParams _ = []

testClass :: CompilationUnit Parsed -> [(Ident, [Ident])]
testClass cUnit = do
  classDecl <- universeBi cUnit
  return (className classDecl, classFields classDecl)

className :: ClassDecl Parsed -> Ident
className (ClassDecl _ _ id _ _ _ _) = id
className (RecordDecl _ _ id _ _ _ _) = id
className (EnumDecl _ _ id _ _) = id

testMethod :: CompilationUnit Parsed -> [(Ident, [Ident])]
testMethod cUnit = do
  memberDecl <- universeBi cUnit
  return (memberName memberDecl, methodParams memberDecl)

memberName :: MemberDecl Parsed -> Ident
memberName (MethodDecl _ _ _ _ id _ _ _ _) = id
memberName (ConstructorDecl _ _ _ id _ _ _) = id
memberName _ = Ident dummySourceSpan "not a method"
