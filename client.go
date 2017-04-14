package main

import (
	"fmt"
	"image/color"
	"net"
	"runtime"

	"engo.io/ecs"
	"engo.io/engo"
	"engo.io/engo/common"
	"github.com/AnkithD/SlitherClone/systems"
)

var (
	ServerAddr string
	Connection net.Conn
)

type myScene struct{}

// Place holder methods to satisfy interface
func (*myScene) Type() string { return "myGame" }

func (*myScene) Preload() {
	err := engo.Files.Load(
		"background.png",
		"food.png",
	)
	if err != nil {
		panic(err)
	}
}

func (*myScene) Setup(world *ecs.World) {
	world.AddSystem(&common.RenderSystem{})
	systems.RegisterButtons()
	systems.CacheActiveSystems(world)
	systems.InitializeVariables()

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

	fmt.Print("Enter Server IP (host:port) : ")
	fmt.Scanln(&ServerAddr)
	fmt.Println("Trying to connect to Server @", ServerAddr, "...")

	con, err := net.Dial("tcp", ServerAddr)
	Connection = con
	if err != nil {
		fmt.Println("Failed to connect to server @ ", ServerAddr)
		panic(err)
	} else {
		fmt.Println("Successfully Connected!")
	}

	engo.Run(opts, new(myScene))
	fmt.Println("Done Running")
	Connection.Write([]byte("NOQUIT\n"))
	Connection.Write([]byte("QUIT\n"))
	Connection.Close()
	fmt.Println("Disconnected with server")
}
