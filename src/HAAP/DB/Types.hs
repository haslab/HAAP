module HAAP.DB.Types where

type History k v = Map k (Either HaapException v)
    
updateHistory :: HasSourceState db (History k v) => k -> Haap p db v -> Haap p args db a
updateHistory k = do
    