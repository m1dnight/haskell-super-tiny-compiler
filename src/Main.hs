--                      |_|                  

module Main where
import Data.Char
import Control.Monad.State
import Text.Printf
import Data.List

main :: IO ()
main = putStrLn "foo"

--------------------------------------------------------------------------------
--- TOKENIZER ------------------------------------------------------------------
--------------------------------------------------------------------------------

data Token = TkNum Int | TkRPar | TkLPar | TkName String deriving (Eq, Show)

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

data Node
  = NumberLit Int
  | CallExpr String [Node]
  | Program [Node]
  | Empty
  deriving (Show, Eq)
           

type PrState = ([Token], [Node])

parse :: [Token] -> Node
parse [] = Program []
parse ts = let (node, ts')  = parseToken ts
               (Program ns) = parse ts'
           in
             Program $ node:ns
             


parseToken :: [Token] -> (Node, [Token])
parseToken (t:ts) = case t of
                      TkNum i -> (NumberLit i, ts)
                      TkLPar  -> let (as, ts') = parseFunc ts
                                 in
                                   (as, ts')


parseFunc :: [Token] -> (Node, [Token])
parseFunc (t:ts) = case t of
                     TkName f -> let fname      = f
                                     (ops, ts') = parseOperands ts
                                 in
                                   (CallExpr fname ops, ts')
                         
parseOperands :: [Token] -> ([Node], [Token])
parseOperands (TkRPar:ts) = ([], ts)
parseOperands ts = let (op, ts')   = parseToken ts
                       (ops, ts'') = parseOperands ts' 
                   in
                     (op:ops, ts'')

--------------------------------------------------------------------------------
--- TRANSFORMER ----------------------------------------------------------------
--------------------------------------------------------------------------------

transform :: Node -> Node -> AST
transform (NumberLit i)     _   = NumberLiteral i
transform (CallExpr s pars) par = let callee = Identifier s
                                      parms  = map (`transform` (CallExpr s pars)) pars
                                      result = CallExpression callee parms
                                  in
                                    case par of
                                      (CallExpr _ _) -> result
                                      _              -> Expression result
                                    
transform (Program ns)      _   = Root $ map (`transform` Empty) ns
                                   
--------------------------------------------------------------------------------
--- CODE GENERATOR -------------------------------------------------------------
--------------------------------------------------------------------------------

data AST
 = Root [AST]
 | Expression AST
 | CallExpression AST [AST]
 | Identifier String
 | NumberLiteral Int
 deriving(Show, Eq)
           
          
generateCode :: AST -> String
generateCode (Root ns)               = foldl (++) "" (intersperse "\n" (map generateCode ns))
generateCode (Expression e)          = printf "%s;" (generateCode e)
generateCode (CallExpression f pars) = let name  = generateCode f
                                           pars' = foldl (++) "" (intersperse ", " (map generateCode pars))
                                       in
                                         printf "%s(%s)" name pars'
generateCode (Identifier s)          = s
generateCode (NumberLiteral n)       = show n
