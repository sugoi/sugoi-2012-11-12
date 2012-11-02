module Sugoi.Types.Internal where

lensNamingRule :: String -> Maybe String
lensNamingRule "_runDB" = Nothing
lensNamingRule ('_':xs) = Just xs
lensNamingRule _        = Nothing
