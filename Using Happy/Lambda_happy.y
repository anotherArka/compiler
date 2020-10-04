{
module Lambda_happy where
import Data.Char
import MonadE
}

%name calc
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }

%token
  let      { TokenLet     }
  eval     { TokenEval    }
  eval_def { TokenEvalDef }
  print    { TokenPrint   }
  load     { TokenLoad    }
  var      { TokenVar $$  }
  num      { TokenNum $$  }
  '/'      { TokenLambda  }
  '.'      { TokenDot     }
  '='      { TokenEq      }
  '('      { TokenOB      }
  ')'      { TokenCB      }
  unknown  { TokenUnknown }

%%

Exp  : let var '=' Term   { Let $2 $4        }
     | eval num Term      { Eval $2 $3       }
     | eval_def num var   { Eval_def $2 $3   }
     | print Term         { Print $2         }
     | load var           { Load_file $2     }
Term : '/' var '.' Term   { Raw_lambda $2 $4 }
     | '(' Term Term ')'  { Raw_app $2 $3    }
     | var                { Var $1           }

{

parseError :: [Token] -> (E a)
parseError _ = Failed "Parse error"

data Exp =
    Let String Raw_term  |
    Eval Int Raw_term    |
    Eval_def Int String  |
    Print Raw_term       |
    Load_file String
  deriving Show
  
data Raw_term = Var String
          | Raw_app Raw_term Raw_term
          | Raw_lambda String Raw_term
  deriving Show

data Token =
    TokenLet        |
    TokenEq         |
    TokenEval       |
    TokenEvalDef    |
    TokenPrint      |
    TokenLoad       |
    TokenVar String |
    TokenNum Int    |
    TokenOB         |
    TokenCB         |
    TokenLambda     |
    TokenUnknown    |
    TokenDot
  deriving Show
  
lexer :: String -> [Token]
lexer [] = []
lexer ('=':cs)       = TokenEq      : (lexer cs)
lexer ('(':cs)       = TokenOB      : (lexer cs)
lexer (')':cs)       = TokenCB      : (lexer cs)
lexer ('/':cs)       = TokenLambda  : (lexer cs)
lexer ('.':cs)       = TokenDot     : (lexer cs)
lexer (c : cs)
  | isSpace c = lexer cs
  | isAlpha c = lexVar (c : cs)
  | isDigit c = lexNum (c : cs)
lexer (c : cs)       = TokenUnknown : (lexer cs)   

lexVar cs = case span isAlphaNum cs of
  ("let"     , rest) -> TokenLet     : (lexer rest)
  ("eval"    , rest) -> TokenEval    : (lexer rest)
  ("evalDef" , rest) -> TokenEvalDef : (lexer rest)
  ("eval"    , rest) -> TokenEval    : (lexer rest)
  ("print"   , rest) -> TokenPrint   : (lexer rest)
  ("load"    , rest) -> TokenLoad    : (lexer rest)
  (var  , rest) -> (TokenVar var)    : (lexer rest)
  
lexNum cs = case span isDigit cs of
  (val, rest) -> (TokenNum (read val)) : (lexer rest)  

parse_lambda = calc . lexer    
  
main = getContents >>= print . calc . lexer
}   
