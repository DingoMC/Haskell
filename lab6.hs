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
                     else
                     let delta = (b ^ 2 - 4 * a * c) in
                         if delta < 0 then error "Nie da sie"
                                      else
                                      if delta == 0 then -b/(2 * a)
                                                    else [(-b - sqrt(delta))/(2 * a), (-b - sqrt(delta))/(2 * a)]
