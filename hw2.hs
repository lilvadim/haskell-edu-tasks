sqx 0 0 _ = error "Not an equation"
sqx 0 _ 0 = [0]
sqx 0 b c = [((-c)/b)]
sqx a b c | d > 0 = [x1, x2]
          | d == 0 = [x1]
          | d < 0 = error "D < 0"
          | otherwise = error "Incorrect equation"
      where 
            d = b * b - 4 * a * c
            x1 = ((-b) + sqrt(d)) / (2 * a)
            x2 = ((-b) - sqrt(d)) / (2 * a)
