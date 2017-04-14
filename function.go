package main

import (
	"fmt"
	"math"
)

func GetAlongLine(p1 engo.Point, p2 engo.Point, d float32) engo.Point {
	X, Y := float64(p1.X), float64(p1.Y)
	x, y := float64(p2.X), float64(p2.Y)
	var m, n float32
	first := math.Pow(X-x, 2)
	second := math.Pow(Y-y, 2)
	dist := math.Sqrt(first + second)
	var r float64
	r = d / (dist - d)
	m = float32((r*x + X) / (r + 1))
	n = float32((r*y + Y) / (r + 1))
	return engo.Point{m, n}
}

func main() {
	fmt.Println(Position(0, 0, 3, 4, 2.5))
}
