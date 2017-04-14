package main

import (
	"fmt"
	"math"
)

func Position(X, Y float64, x, y float64, d float64) (float64, float64) {
	var m, n float64
	first := math.Pow(X-x, 2)
	second := math.Pow(Y-y, 2)
	dist := math.Sqrt(first + second)
	var r float64
	r = d / (dist - d)
	m = (r*x + X) / (r + 1)
	n = (r*y + Y) / (r + 1)
	return m, n
}

func main() {
	fmt.Println(Position(8, 0, 6, 0, 1))
}
