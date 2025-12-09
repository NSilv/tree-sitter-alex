{
module Lexer 
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan

  , Range (..)
  , RangedToken (..)
  , Token (..)
  , scanMany
  , Display(..)
  , Delim(..)
  , DelimType(..)
  , DelimState(..)
  ) where
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Pretty.Simple (pPrint)
import Debug.Trace (traceShow)
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]
$lpar = \(
$rpar = \)


@arr = '->'
@identifier = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*
@operator = [!@\#\$\%\^&\*\-=\+\/\?\,\.]+
@integer = $digit+


tokens :- 

<0> $white+ ;

<0> do { token_k Do }
<0> "(" { tok_delim Opened Paren }
<0> ")" { tok_delim Closed Paren }
<0> "[" { tok_delim Opened Square }
<0> "]" { tok_delim Closed Square }
<0> "{" { tok_delim Opened Brace }
<0> "}" { tok_delim Closed Brace }
<0> ":" { token_k Colon } 
<0> "->" { token_k Arrow }
<0> def { token_k FunDef_def }
<0> \= { token_k Equals }
<0> end { token_k End }
<0> fun { token_k FunDef_fun }
<0> sig { token_k FunDef_sig }
<0> match { token_k PatMatch_match }
<0> with { token_k PatMatch_with }

<0> @identifier { tok_id }
<0> @operator { tok_op }
<0> @integer { tok_int }

{
data AlexUserState = AlexUserState 
  { !comment_nest_level :: Int
  }
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 
  { comment_nest_level = 0
  }

-- TODO: figure out if I want to use mtl or not

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexEOF :: Alex RangedToken
alexEOF = do 
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Span = Span 
  { offset :: !Int 
  , line   :: !Int 
  , column :: !Int 
  }
  deriving (Eq, Show)
data Range = Range 
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq)

instance Show Range where 
  show (Range start end) = 
    mconcat 
      [ "Range{start = "
      , formatPos start 
      , ", end = "
      , formatPos end     
      ]
    where 
      formatPos (AlexPn offset line col) = 
        mconcat [show line, ":", show col, " @ ", show offset]

-- move this to another file 

class Display a where 
  display :: a -> IO () 





data RangedToken = RangedToken 
  { rt_token :: Token
  , rt_range :: Range
  } deriving (Eq, Show)

data DelimType
  = Paren 
  | Square 
  | Brace 
  deriving (Eq)

instance Show DelimType where 
  show Paren = "()"
  show Square = "[]"
  show Brace = "{}"
data DelimState 
  = Opened
  | Closed
  deriving (Eq, Show)
data Delim = Delim DelimState DelimType 
  deriving (Eq)

instance Show Delim where 
  show (Delim s t) = 
    ['"', paren ,'"']
    where 
      [opened, closed] = show t 
      paren = case s of 
        Opened -> opened
        Closed -> closed




data Token 
  = Identifier ByteString 
  | String ByteString 
  | Integer Integer 
  | Operator ByteString
  | Do 
  | Delimiter Delim 
  | Colon 
  | Arrow 
  | FunDef_def
  | FunDef_sig 
  | FunDef_fun
  | PatMatch_with
  | PatMatch_match
  | End
  | Equals
  | EOF
  deriving (Eq, Show)

instance (Show a) => Display (Either a [RangedToken]) where
  display e@(Left _) = print e
  display toks@(Right _) = 
    pPrint $ fmap (fmap rt_token) $ toks
mkRange :: AlexInput -> Int64 -> Range 
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where 
    stop = BS.foldl' alexMove start $ BS.take len str 

token_from :: (ByteString -> a) -> (a -> Token) -> AlexAction RangedToken
token_from mapping constr = 
  \inp@(_, _, str, _) len ->
    pure RangedToken
      { rt_token = constr $ mapping $ BS.take len str 
      , rt_range = mkRange inp len
      }


token_k :: Token -> AlexAction RangedToken 
token_k tok = token_of (const tok) 

token_of :: (ByteString -> Token) -> AlexAction RangedToken
token_of = token_from id 

tok_id :: AlexAction RangedToken 
tok_id = 
  token_of Identifier 

tok_op :: AlexAction RangedToken 
tok_op = 
  token_of Operator 

tok_int :: AlexAction RangedToken 
tok_int =
  token_from (read . BS.unpack) Integer

tok_delim :: DelimState -> DelimType -> AlexAction RangedToken
tok_delim s t = 
  token_k $ Delimiter $ Delim s t 

-- for testing purposes 
scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rt_token output == EOF
        then pure [output]
        else (output :) <$> go

}
