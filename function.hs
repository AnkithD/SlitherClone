data Shape = Rectangle Float Float Float Float

function :: float -> float -> float -> Shape

function x y r = x-r y-r x+r y+r 
