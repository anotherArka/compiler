{
module Lambda_happy where
import Data.Char
}

%name calc
%tokentype { Token }
%error { parseError }

%token
  let    { TokenLet }
  var    { TokenVar $$ }
  '/'    { TokenLambda }
  '.'    { TokenDot }
  '='    { TokenEq }
  '('    { TokenOB }
  ')'    { TokenCB }

%%

Exp  : let var '=' Term  { Let $2 $4 }
Term : '/' var '.' Term  { Raw_lambda $2 $4 }
     | '(' Term Term ')' { Raw_app $2 $3 }
     | var               { Var $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = Let String Raw_term
  deriving Show
  
data Raw_term = Var String
          | Raw_app Raw_term Raw_term
          | Raw_lambda String Raw_term
  deriving Show

data Token =
    TokenLet |
    TokenEq |
    TokenVar String |
    TokenOB |
    TokenCB |
    TokenLambda |
    TokenDot
  deriving Show
  
lexer :: String -> [Token]
lexer [] = []
lexer ('=':cs)       = TokenEq     : (lexer cs)
lexer ('(':cs)       = TokenOB     : (lexer cs)
lexer (')':cs)       = TokenCB     : (lexer cs)
lexer ('/':cs)       = TokenLambda : (lexer cs)
lexer ('.':cs)       = TokenDot    : (lexer cs)
lexer (c : cs)
  | isSpace c = lexer cs
  | isAlpha c = lexVar (c : cs)

lexVar cs = case span isAlpha cs of
  ("let", rest) -> TokenLet       : (lexer rest)
  (var  , rest) -> (TokenVar var) : (lexer rest)

parse_lambda = calc . lexer    
  
main = getContents >>= print . calc . lexer
}   
