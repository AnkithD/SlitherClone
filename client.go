package main

import (
	"image/color"
	"runtime"

	"engo.io/ecs"
	"engo.io/engo"
	"engo.io/engo/common"
	"github.com/AnkithD/SlitherClone/systems"
)

type myScene struct{}

// Place holder methods to satisfy interface
func (*myScene) Type() string { return "myGame" }

func (*myScene) Preload() {

}

func (*myScene) Setup(world *ecs.World) {
	world.AddSystem(&common.RenderSystem{})
	systems.RegisterButtons()
	systems.InitializeVariables()
	systems.CacheActiveSystems(world)

	world.AddSystem(&systems.SlitherSystem{})

	common.SetBackground(color.RGBA{182, 204, 104, 255})
}

func main() {
	runtime.GOMAXPROCS(8)
	opts := engo.RunOptions{
		Title:         "AOE Clone",
		Width:         1280,
		Height:        768,
		ScaleOnResize: true,
		MSAA:          2,
		VSync:         true,
	}

	engo.Run(opts, new(myScene))
}
