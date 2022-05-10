mz2 a b c
    | a == 0 = error "To nie jest funkcja kwadratowa!"
    | delta == 0 = [x1]
    | delta > 0 = [x1, x2]
    | otherwise = error "brak miejsc zerowych"
    where delta = (b ^ 2 - 4 * a * c)
          x1 = (-b - sqrt(delta))/(2 * a)
          x2 = (-b + sqrt(delta))/(2 * a)
sign x =
 case x of
      0 -> 0
      _ -> if x < 0 then -1 else 1
myComp :: Ord a => a -> a -> Ordering
myComp a b
    | a == b = EQ
    | a < b = LT
    | otherwise = GT
