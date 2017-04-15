data Shape = Rectangle Float Float Float Float

coveringRect :: float -> float -> float -> Shape
coveringRect x y r = x-r y-r x+r y+r 
