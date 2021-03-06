package systems

import (
	"engo.io/ecs"
	"engo.io/engo"
	"engo.io/engo/common"
	"fmt"
	"net"
)

// Button mappings
var ()

type ActiveSystemsStruct struct {
	RenderSys *common.RenderSystem
	MouseSys  *common.MouseSystem
	CameraSys *common.CameraSystem
}

// File Names

var (
	BackgroundImage = "background.png"
	FoodSprite      = "food.png"
)

// Other Variables
var (
	ActiveSystems ActiveSystemsStruct
	WorldBounds   engo.AABB
	Connection    net.Conn
)

// Functions

// Get the mouse position adjusted for zoom
func GetAdjustedMousePos(WRTWindow bool) (float32, float32) {
	CamSys := ActiveSystems.CameraSys
	x := engo.Input.Mouse.X * CamSys.Z() * (engo.GameWidth() / engo.CanvasWidth())
	y := engo.Input.Mouse.Y * CamSys.Z() * (engo.GameHeight() / engo.CanvasHeight())

	if !WRTWindow {
		x += CamSys.X() - (engo.GameWidth()/2)*CamSys.Z()
		y += CamSys.Y() - (engo.GameHeight()/2)*CamSys.Z()
	}

	return x, y

}

func WithinGameWindow(x, y float32) bool {
	CamSys := ActiveSystems.CameraSys
	cx, cy := CamSys.X()-engo.WindowWidth()/2, CamSys.Y()-engo.WindowHeight()/2
	ymin := cy
	ymax := cy + engo.WindowHeight()

	return (cx <= x && x <= cx+engo.WindowWidth() && ymin <= y && y <= ymax)
}

func RegisterButtons() {
	fmt.Println("Registered Buttons")
}

func CacheActiveSystems(world *ecs.World) {
	for _, system := range world.Systems() {
		switch sys := system.(type) {
		case *common.RenderSystem:
			ActiveSystems.RenderSys = sys
		case *common.MouseSystem:
			ActiveSystems.MouseSys = sys
		case *common.CameraSystem:
			ActiveSystems.CameraSys = sys
		}
	}

	fmt.Println("Cached Important System References")
}

func InitializeVariables() {
	WorldBounds = engo.AABB{Min: engo.Point{0, 0}, Max: engo.Point{2000, 2000}}
	BackgroundTex, err := common.LoadedSprite(BackgroundImage)
	if err != nil {
		panic("Error making texture from background")
	}

	for i := 0; i < 10; i++ {
		for j := 0; j < 10; j++ {
			bc := ecs.NewBasic()
			rc := common.RenderComponent{
				Drawable: BackgroundTex,
			}
			rc.SetZIndex(0)
			w, h := rc.Drawable.Width(), rc.Drawable.Height()
			sc := common.SpaceComponent{
				Position: engo.Point{float32(i) * w, float32(j) * h},
				Width:    w,
				Height:   h,
			}
			ActiveSystems.RenderSys.Add(&bc, &rc, &sc)
		}
	}

	for i := 0; i < 1000; i++ {

	}

	fmt.Println("Initialized variables")
}

func ReadFromServer() string {
	ReadBuffer := make([]byte, 1024)
	num, err := Connection.Read(ReadBuffer)
	if err != nil {
		fmt.Print("Error reading from server, ERR:")
		panic(err)
	}
	StringBuffer := string(ReadBuffer[:num])
	for StringBuffer[len(StringBuffer)-1] == '\n' {
		StringBuffer = StringBuffer[:len(StringBuffer)-1]
	}
	//fmt.Println("Read", StringBuffer)

	return StringBuffer
}

func WriteToServer(msg string) {
	if msg[len(msg)-1] != '\n' {
		msg += "\n"
	}
	//fmt.Println("Writing", msg)
	_, err := Connection.Write([]byte(msg))
	//fmt.Println("Return of Write is", num, "length of msg is", len(msg))
	if err != nil {
		fmt.Print("Error writing to server, ERR:")
		panic(err)
	}
}
