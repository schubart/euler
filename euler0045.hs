import Data.List

trias  = map (\n -> n * (n + 1) `div` 2)     [1..]
pentas = map (\n -> n * (3 * n - 1) `div` 2) [1..]
hexas  = map (\n -> n * (2 * n - 1))         [1..]

commons (t:ts) (p:ps) (h:hs)
    | t == p && p == h = t : commons ts ps hs
    | smallest == t    = commons ts (p:ps) (h:hs)
    | smallest == p    = commons (t:ts) ps (h:hs)
    | smallest == h    = commons (t:ts) (p:ps) hs
    where
      smallest = minimum [t, p, h]

main = print $ find (> 40755) $ commons trias pentas hexas