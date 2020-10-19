module Infernal.Utils where

whenIsJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenIsJust (Just x) f = f x
whenIsJust Nothing _ = return ()

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : xs) = Just x
firstJust (_ : xs) = firstJust xs