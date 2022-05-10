neg :: Int -> Int
neg x = -x
pierw x = if x < 0 then error "Tak sie nie robi" else sqrt x
delta a b c = sqrt(b ^ 2 - 4 * a * c)
pdelta a b c = let delta = (b ^ 2 - 4 * a * c) in
                if delta > 0 then sqrt(delta)
                 else 
                 if delta == 0 then 0
                 else error "Nie da sie"
mz a b c = if a == 0 then error "To nie jest funkcja kwadratowa!"
                     else if delta < 0 then error "Nie da sie"
                                      else
                                      if delta == 0 then [x1]
                                                    else [x1, x2]
    where delta = (b ^ 2 - 4 * a * c)
          x1 = (-b - sqrt(delta))/(2 * a)
          x2 = (-b + sqrt(delta))/(2 * a)
