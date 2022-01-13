module JsonParser where

import Control.Monad
import Control.Functor
import Control.Applicative
import Control.Alternative

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
    (<$>) f (Parser p) = Parser $ \input -> do
        (input', x) <- p input
        Just (input', (f x))

    
instance Applicative Parser where
    pure a = Parser $ \input -> Just (input, a)

    (Parser f) <*> (Parser g) = Parser $ \input do
        (input', p) <-  (f input)
        (input'', a) <- (g input')
        Just $ (input'', )


charP :: Char -> Parser Char
charP x = Parser $ \input -> 
    case input of
        a:as 
            | a == x -> Just (as, x)
        [] -> Nothing

stringP :: String -> Parser String
stringP xs = sequenceA $ map charP xs

