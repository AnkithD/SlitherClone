data Shape = Rectangle Float Float Float Float
intersect :: Shape -> Shape -> Bool
intersect (Rectangle x1 y1 x2 y2) (Rectangle x3 y3 x4 y4)
       | x1 > x4 = False
       | x2 < x3 = False
       | y1 > y4 = False
       | y2 < y3 = False
       | otherwise = True