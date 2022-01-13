module JsonParser where

import Control.Monad
import Control.Applicative

data JsonValue = JsonNull
                | JsonBool Bool
                | JsonString String
                | JsonNumber Int
                | JsonArray [JsonValue]
                | JsonObject [(String, JsonValue)]
                deriving (Show, Eq)

newtype Parser a = Parser
    {
        runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (input', x) <- p input
        Just (input', (f x))

    
instance Applicative Parser where
    pure a = Parser $ \input -> Just (input, a)

    (Parser f) <*> (Parser g) = Parser $ \input -> do
        (input', p) <-  (f input)
        (input'', a) <- (g input')
        Just $ (input'', (p a))

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser r) <|> (Parser s) = Parser $ \input ->
        (r input) <|> (s input)

jsonNull :: String -> Parser JsonValue
jsonNull s = (\_ -> JsonNull) <$> stringP "null" 

jsonBool :: String -> Parser JsonValue
jsonBool s = f <$> (stringP "true" <|> stringP "false")
    where
        f "true" = JsonBool True
        f "false" = JsonBool False
        f _ = undefined

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> 
    let (token, rest) = span f input
        in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull p = Parser $ \input -> do
    (rest, tok) <- runParser p $ input
    case null tok of
        True -> Nothing
        False -> Just (rest, tok)

jsonNumber :: Parser JsonValue
jsonNumber = 
    f <$> notNull (spanP isDigit) 
        where f ns = JsonNumber $ read ns  



charP :: Char -> Parser Char
charP x = Parser $ \input -> 
    case input of
        a:as 
            | a == x -> Just (as, x)
            | otherwise -> Nothing

stringP :: String -> Parser String
stringP xs = sequenceA $ map charP xs

jsonValue :: String -> Parser JsonValue
jsonValue = jsonNull <|> jsonBool 