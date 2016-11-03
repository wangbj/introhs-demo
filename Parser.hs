module Parser
    (
      Parser (..)
    , Input
    , ParseError (..)
    , ParseResult (..)
    , isErrorResult
    , unexpectedCharParser
    , valueParser
    , failed
    , anyChar
    , satisfy
    , digit
    , char
    , string
    , choice
    , eof
    , count
    ) where

import Control.Applicative
import Control.Monad
import Data.Char

type Input = String

data ParseError = UnexpectedEof
                | ExpectedEof Input
                | UnexpectedChar Char
                | Failed
                deriving Eq

instance Show ParseError where
  show UnexpectedEof =
    "Unexpected end of stream"
  show (ExpectedEof i) =
    concat ["Expected end of stream, but got >", show i, "<"]
  show (UnexpectedChar c) =
    concat ["Unexpected character: ", show [c]]
  show Failed = "Parse failed"  

data ParseResult a = ErrorResult ParseError
                   | Result Input a
                   deriving Eq

instance Functor ParseResult where
  fmap f (ErrorResult e) = ErrorResult e
  fmap f (Result input a) = Result input (f a)

instance (Show a) => Show (ParseResult a) where
  show (ErrorResult e) = show e
  show (Result i a) = concat [ "Result >", i, "< ", show a]

isErrorResult :: ParseResult a -> Bool
isErrorResult (ErrorResult _) = True
isErrorResult (Result _ _)    = False

-- ^A parser consumes ``Input``, parse it to a result as well as
-- remaining input, or return a error on failed parsing.
newtype Parser a = P {
    parse :: Input -> ParseResult a
  }

unexpectedCharParser :: Char -> Parser a
unexpectedCharParser c = P $ \_ -> ErrorResult (UnexpectedChar c)

valueParser :: a -> Parser a
valueParser x = P (\i -> Result i x)

failed :: Parser a
failed = P (\_ -> ErrorResult Failed)

anyChar :: Parser Char
anyChar = P $ \s -> case s of
                [] -> ErrorResult UnexpectedEof
                (c:cs) -> Result cs c

instance Functor Parser where
  fmap f p = P $ \input -> case parse p input of
    ErrorResult e -> ErrorResult e
    Result cs c   -> Result cs (f c)

instance Applicative Parser where
  pure x = P $ \input -> Result input x
  p <*> q = P $ \input -> case parse p input of
    ErrorResult e -> ErrorResult e
    Result input' f -> case parse q input' of
      ErrorResult e' -> ErrorResult e'
      Result input'' a -> Result input'' (f a)

instance Monad Parser where
  return = pure
  P p >>= f = P $ \s -> case p s of
    ErrorResult e   -> ErrorResult e
    Result s' a -> parse (f a) s'

instance Alternative Parser where
  empty = P $ \_ -> ErrorResult Failed
  (P f) <|> (P g) = P $ \s -> case f s of
    ErrorResult e -> g s
    Result s' a   -> Result s' a

many1 :: Parser a -> Parser [a]
many1 = some

satisfy :: (Char -> Bool)
        -> Parser Char
satisfy pred = P $ \s -> case s of
  []     -> ErrorResult UnexpectedEof
  (c:cs) -> if pred c then Result cs c else ErrorResult (UnexpectedChar c)

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

string :: String -> Parser String
string = mapM (\x -> char x)

choice :: [Parser a] -> Parser a
choice = foldl (<|>) failed

eof :: Parser ()
eof = P $ \s -> case s of
  []     -> Result [] ()
  (c:cs) -> ErrorResult (UnexpectedChar c)

count :: Int -> Parser a -> Parser [a]
count k = replicateM k

--

-- parse and return a valid phone number
-- valid phone number can be either one of
-- xxx-yyy-zzzz       858-123-4567
-- xxxyyyzzzz         8581234567
-- xxyyy              12345       Local number
-- xxx yyy zzzz       858 123 4567

localnumber :: Parser String
localnumber = count 5 digit

-- ^ parse "8581234567"
number10_1 :: Parser String
number10_1 = count 10 digit

-- ^ parse "858-123-4567"
number10_2 :: Parser String
number10_2 = do
  p1 <- count 3 digit
  char '-'
  p2 <- count 3 digit
  char '-'
  p3 <- count 4 digit
  return (p1++p2++p3)

-- ^ parse "858 123 4567"
number10_3 :: Parser String
number10_3 = do
  p1 <- count 3 digit
  char ' '
  p2 <- count 3 digit
  char ' '
  p3 <- count 4 digit
  return (p1++p2++p3)

phoneNumber = number10_1 <|> number10_2 <|> number10_3 <|> localnumber

-- ghci> parse phoneNumber "1234567890"
-- Result >< "1234567890"
