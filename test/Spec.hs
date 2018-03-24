{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import CommandHistory

main :: IO ()
main = do
  -- setup files
  writeFile "empty.txt" ""
  writeFile "commands.txt" "ls\npwd\ncd test\nls -l\ncd ..\n"

  hspec $ do
    describe "toPrevCommand" $ do
      it "returns Nothing on empty file" $ do
        ch <- openCommandHistory "empty.txt"
        result <- toPrevCommand ch 
        result `shouldBe` Nothing

      it "returns Nothing when first command reached" $ do
        ch <- openCommandHistory "commands.txt"
        toPrevCommand ch 
        toPrevCommand ch 
        toPrevCommand ch 
        toPrevCommand ch 
        toPrevCommand ch 
        result <- toPrevCommand ch 
        result `shouldBe` Nothing

      it "returns last command in nonempty file" $ do
        ch <- openCommandHistory "commands.txt"
        result <- toPrevCommand ch 
        result `shouldBe` Just "ls"

      it "returns correct command in nonempty file" $ do
        ch <- openCommandHistory "commands.txt"
        toPrevCommand ch 
        toPrevCommand ch 
        result <- toPrevCommand ch 
        result `shouldBe` Just "cd test"

    describe "toNextCommand" $ do
      it "returns Nothing on empty file" $ do
        ch <- openCommandHistory "empty.txt"
        result <- toNextCommand ch 
        result `shouldBe` Nothing
        
      it "returns Nothing when last command reached" $ do
        ch <- openCommandHistory "commands.txt"
        toPrevCommand ch 
        toPrevCommand ch 
        toNextCommand ch 
        toNextCommand ch 
        result <- toNextCommand ch 
        result `shouldBe` Nothing

      it "returns Nothing on last command in nonempty file" $ do
        ch <- openCommandHistory "commands.txt"
        result <- toNextCommand ch 
        result `shouldBe` Nothing

      it "returns correct command in nonempty file" $ do
        ch <- openCommandHistory "commands.txt"
        toPrevCommand ch 
        toPrevCommand ch 
        toPrevCommand ch 
        toPrevCommand ch 
        toNextCommand ch
        result <- toNextCommand ch 
        result `shouldBe` Just "pwd"

    describe "setCurrCommand" $ do
      it "doesn't affect history" $ do
        ch <- openCommandHistory "commands.txt"
        setCurrCommand ch "cmd1"
        setCurrCommand ch "cmd2"
        setCurrCommand ch ""
        result <- toPrevCommand ch
        result `shouldBe` Just "ls"

      it "changes history after a new command" $ do
        ch <- openCommandHistory "commands.txt"
        setCurrCommand ch "cmd1"
        setCurrCommand ch "cmd2"
        setCurrCommand ch "cmd3"
        startNewCommand ch
        result <- toPrevCommand ch
        result `shouldBe` Just "cmd3"

      it "changes completion results" $ do
        ch <- openCommandHistory "commands.txt"
        setCurrCommand ch "p"
        result <- toPrevCommand ch
        result `shouldBe` Just "pwd"
        setCurrCommand ch "pa"
        result <- toPrevCommand ch
        result `shouldBe` Nothing

    describe "startNewCommand" $ do
      it "adds to history" $ do
        ch <- openCommandHistory "commands.txt"
        setCurrCommand ch "test"
        startNewCommand ch
        result <- toPrevCommand ch
        result `shouldBe` Just "test"
    
      it "adds empty commands to history" $ do
        ch <- openCommandHistory "commands.txt"
        setCurrCommand ch ""
        startNewCommand ch
        result <- toPrevCommand ch
        result `shouldBe` Just ""
    
