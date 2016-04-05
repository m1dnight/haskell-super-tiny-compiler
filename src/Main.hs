-- Author : Christophe De Troyer
-- Date   : 01/01/2016
-- E-mail : cdetroye@vub.ac.be
--  ____                        
-- / ___| _   _ _ __   ___ _ __ 
-- \___ \| | | | '_ \ / _ \ '__|
--  ___) | |_| | |_) |  __/ |   
-- |____/ \__,_| .__/ \___|_|   
--             |_|              
--  _____ _             
-- |_   _(_)_ __  _   _ 
--   | | | | '_ \| | | |
--   | | | | | | | |_| |
--   |_| |_|_| |_|\__, |
--                |___/ 
--   ____                      _ _           
--  / ___|___  _ __ ___  _ __ (_) | ___ _ __ 
-- | |   / _ \| '_ ` _ \| '_ \| | |/ _ \ '__|
-- | |__| (_) | | | | | | |_) | | |  __/ |   
--  \____\___/|_| |_| |_| .__/|_|_|\___|_|   
--                      |_|                  

module Main where
import Data.Char
import Control.Monad.State
import Text.Printf

main :: IO ()
main = putStrLn "foo"

--------------------------------------------------------------------------------
--- TOKENS ---------------------------------------------------------------------
--------------------------------------------------------------------------------

data Token = TkNum Int | TkRPar | TkLPar | TkName String deriving (Eq, Show)

--------------------------------------------------------------------------------
--- TOKENIZER ------------------------------------------------------------------
--------------------------------------------------------------------------------
type TkState = (String, [Token])

tokenize :: String -> [Token]
tokenize input = let (_, tokens) = execState tokenizer (input, [])
                 in
                   reverse tokens


tokenizer :: State TkState ()
tokenizer = do (input, ts) <- get
               case input of
                 []     -> return ()
                 (s:ss) ->
                  case () of
                    _ | s == ')'      -> put (ss, TkRPar:ts) >> tokenizer
                    _ | s == '('      -> put (ss, TkLPar:ts) >> tokenizer
                    _ | isDigit s     -> let (res, num) = tokenizeNumber (s:ss)
                                         in
                                           put (res, num:ts) >> tokenizer
                    _ | isAlpha s     -> let (res, name) = tokenizeName (s:ss)
                                         in
                                           put (res, name:ts) >> tokenizer
                    _ | isSeparator s -> put (ss, ts) >> tokenizer
                    _ | otherwise -> error $ printf "Could not match char: < %s >" (show s)
                    

tokenizeNumber :: String -> (String, Token)
tokenizeNumber input = let num = takeWhile isDigit input
                           res = dropWhile isDigit input
                       in
                         (res, TkNum $ read num)

                         
tokenizeName :: String -> (String, Token)
tokenizeName input = let nam = takeWhile isAlpha input
                         res = dropWhile isAlpha input
                     in
                       (res, TkName nam)                         

--------------------------------------------------------------------------------
--- PARSER ---------------------------------------------------------------------
--------------------------------------------------------------------------------
