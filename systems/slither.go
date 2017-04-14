package systems

import (
	"engo.io/ecs"
	"engo.io/engo"
	"engo.io/engo/common"
	"fmt"
	"image/color"
	"math"
	"math/rand"
	//"net"
	"strconv"
	"strings"
	"time"
)

type SlitherSystem struct {
	world         *ecs.World
	Slithers      []*SlitherEntity
	Player        *SlitherEntity
	BaseColors    []color.RGBA
	DefaultRadius float32
}

func (ss *SlitherSystem) Remove(ecs.BasicEntity) {}
func (ss *SlitherSystem) New(w *ecs.World) {
	//rand.Seed(time.Now().Unix())
	rand.Seed(time.Now().Unix())
	ss.world = w
	ss.BaseColors = []color.RGBA{
		color.RGBA{220, 20, 60, 255},
		color.RGBA{205, 50, 120, 255},
		color.RGBA{139, 0, 139, 255},
		color.RGBA{75, 0, 130, 255},
		color.RGBA{0, 0, 205, 255},
		color.RGBA{0, 134, 139, 255},
		color.RGBA{0, 255, 0, 255},
		color.RGBA{205, 205, 0, 255},
		color.RGBA{238, 154, 0, 255},
		color.RGBA{255, 154, 0, 255},
	}
	ss.DefaultRadius = 20

	ss.Player = &SlitherEntity{
		BaseColor:  ss.BaseColors[rand.Int()%(len(ss.BaseColors))],
		ColorCount: 1,
		NodeRadius: ss.DefaultRadius,
		NodeGap:    ss.DefaultRadius / 2,
		Body:       make([]*SlitherNode, 0),
		MoveSpeed:  200,
	}

	rand_x := rand.Int() % (int(WorldBounds.Max.X - 200))
	rand_x += 100
	rand_y := rand.Int() % (int(WorldBounds.Max.Y - 200))
	rand_y += 100

	tempnode := &SlitherNode{
		BasicEntity: ecs.NewBasic(),
		SpaceComponent: common.SpaceComponent{
			Position: engo.Point{float32(rand_x), float32(rand_y)},
			Width:    ss.Player.NodeRadius * 2,
			Height:   ss.Player.NodeRadius * 2,
		},
		RenderComponent: common.RenderComponent{
			Drawable: common.Circle{},
			Color:    ss.Player.BaseColor,
		},
	}
	tempnode.SetZIndex(1000)

	ss.Player.Body = append(ss.Player.Body, tempnode)
	ActiveSystems.RenderSys.Add(
		&(ss.Player.Body[0].BasicEntity),
		&(ss.Player.Body[0].RenderComponent),
		&(ss.Player.Body[0].SpaceComponent),
	)
	for i := 0; i < 5; i++ {
		ss.Elongate(ss.Player)
	}
	ss.world.AddSystem(&common.EntityScroller{
		SpaceComponent: &ss.Player.Body[0].SpaceComponent,
		TrackingBounds: WorldBounds,
	})

	InitMsg := "0|"
	tempx := strconv.FormatFloat(float64(rand_x), 'f', 2, 32)
	tempy := strconv.FormatFloat(float64(rand_y), 'f', 2, 32)
	InitMsg += tempx + "|" + tempy + "|"
	tempcol := ss.Player.BaseColor
	InitMsg += strconv.Itoa(int(tempcol.R)) + "|" + strconv.Itoa(int(tempcol.G)) + "|" + strconv.Itoa(int(tempcol.B))
	WriteToServer(InitMsg)

	StringBuffer := ReadFromServer()
	var EnemyBaseColor color.RGBA
	var EnemyPos engo.Point
	msg := strings.Split(StringBuffer, "|")
	fmt.Println("Splitted: ", msg)
	if msg[0] == "0" {
		x, e1 := strconv.ParseFloat(msg[1], 32)
		y, e2 := strconv.ParseFloat(msg[2], 32)
		r, e3 := strconv.Atoi(msg[3])
		g, e4 := strconv.Atoi(msg[4])
		b, e5 := strconv.Atoi(msg[5])
		if e1 != nil || e2 != nil || e3 != nil || e4 != nil || e5 != nil {
			panic("Error parsing Init packet")
		}
		EnemyPos = engo.Point{float32(x), float32(y)}
		EnemyBaseColor = color.RGBA{uint8(r), uint8(g), uint8(b), 255}
	} else {
		fmt.Println("msg: ", msg)
		panic("Expected init packet but did not get it")
	}
	ss.AddSlither(EnemyPos, EnemyBaseColor)

	fmt.Println("Initialized Slither System")
}
func (ss *SlitherSystem) Update(dt float32) {
	mx, my := GetAdjustedMousePos(false)
	ss.Player.MoveTowards = engo.Point{mx, my}

	UpdtMsg := "1|"
	tempx := strconv.FormatFloat(float64(mx), 'f', 2, 32)
	tempy := strconv.FormatFloat(float64(my), 'f', 2, 32)
	UpdtMsg += tempx + "|" + tempy
	WriteToServer(UpdtMsg)

	StringBuffer := ReadFromServer()
	msg := strings.Split(StringBuffer, "|")

	//fmt.Println("splitted: ", msg)
	if msg[0] == "1" {
		x, e1 := strconv.ParseFloat(msg[1], 32)
		y, e2 := strconv.ParseFloat(msg[2], 32)
		if e1 != nil || e2 != nil {
			panic("Error parsing Update Packet")
		}
		ss.Slithers[0].MoveTowards = engo.Point{float32(x), float32(y)}
	} else {
		fmt.Println("msg: ", msg)
		panic("Expected update packet but did not get it")
	}

	ss.Player.Move(dt)
	ss.Slithers[0].Move(dt)
}

func (ss *SlitherSystem) AddSlither(pos engo.Point, col color.RGBA) {
	tempSlither := &SlitherEntity{
		BaseColor:  col,
		ColorCount: 1,
		NodeRadius: ss.DefaultRadius,
		NodeGap:    ss.DefaultRadius / 2,
		Body:       make([]*SlitherNode, 0),
		MoveSpeed:  200,
	}
	tempnode := &SlitherNode{
		BasicEntity: ecs.NewBasic(),
		SpaceComponent: common.SpaceComponent{
			Position: engo.Point{pos.X, pos.Y},
			Width:    tempSlither.NodeRadius * 2,
			Height:   tempSlither.NodeRadius * 2,
		},
		RenderComponent: common.RenderComponent{
			Drawable: common.Circle{},
			Color:    col,
		},
	}
	tempnode.SetZIndex(1000)
	tempSlither.Body = append(tempSlither.Body, tempnode)

	ActiveSystems.RenderSys.Add(
		&tempSlither.Body[0].BasicEntity,
		&tempSlither.Body[0].RenderComponent,
		&tempSlither.Body[0].SpaceComponent,
	)
	for i := 0; i < 5; i++ {
		ss.Elongate(tempSlither)
	}

	ss.Slithers = append(ss.Slithers, tempSlither)
}

func (ss *SlitherSystem) Elongate(sl *SlitherEntity) {
	pos := sl.Body[len(sl.Body)-1].Position

	new_node := &SlitherNode{
		BasicEntity: ecs.NewBasic(),
		SpaceComponent: common.SpaceComponent{
			Position: pos,
			Width:    sl.NodeRadius * 2,
			Height:   sl.NodeRadius * 2,
		},
		RenderComponent: common.RenderComponent{
			Drawable: common.Circle{},
			Color:    ss.GetShade(sl.BaseColor, sl.ColorCount),
		},
	}
	new_node.RenderComponent.SetZIndex(float32(1000 - len(sl.Body)))
	sl.Body = append(sl.Body, new_node)
	sl.ColorCount += 1

	ActiveSystems.RenderSys.Add(
		&new_node.BasicEntity,
		&new_node.RenderComponent,
		&new_node.SpaceComponent,
	)
}

func (ss *SlitherSystem) GetShade(BaseCol color.RGBA, Count int) color.RGBA {
	r, g, b := BaseCol.R, BaseCol.G, BaseCol.B
	ratio := 0.5 + (math.Abs(float64(10-(Count%20))) / float64(20))
	r = uint8(float64(r) * ratio)
	g = uint8(float64(g) * ratio)
	b = uint8(float64(b) * ratio)

	return color.RGBA{r, g, b, 255}
}

type SlitherEntity struct {
	BaseColor   color.RGBA
	ColorCount  int
	Body        []*SlitherNode
	NodeGap     float32
	NodeRadius  float32
	MoveTowards engo.Point
	MoveSpeed   float32
}

type SlitherNode struct {
	ecs.BasicEntity
	common.RenderComponent
	common.SpaceComponent
}

func (se *SlitherEntity) Move(dt float32) {
	for i := 0; i < len(se.Body); i++ {
		pos := &se.Body[i].SpaceComponent.Position
		if i == 0 {
			*pos = GetAlongLine(*pos, se.MoveTowards, dt*se.MoveSpeed)
		} else {
			*pos = GetAlongLine(se.Body[i-1].SpaceComponent.Position, *pos, se.NodeGap)
		}
	}
}

// func (se *SlitherEntity) Serialize() string {
// 	r, g, b = se.BaseColor.R, se.BaseColor.G, se.BaseColor.B
// 	result := string(r) + "|" + string(g) + "|" + string(b) + "|"
// 	for i := 0; i < len(se.Body); i++ {

// 	}
// }

func GetAlongLine(p1 engo.Point, p2 engo.Point, d float32) engo.Point {
	X, Y := float64(p1.X), float64(p1.Y)
	x, y := float64(p2.X), float64(p2.Y)

	if X == x && Y == y {
		panic("p1 and p2 coincide!")
	}

	var m, n float32
	first := math.Pow(X-x, 2)
	second := math.Pow(Y-y, 2)
	dist := math.Sqrt(first + second)
	var r float64

	r = float64(d) / (dist - float64(d))
	m = float32((r*x + X) / (r + 1))
	n = float32((r*y + Y) / (r + 1))
	//fmt.Printf("%0.2f from (%0.2f, %0.2f) towards (%0.2f, %0.2f) is (%0.2f, %0.2f)\n", d, p1.X, p1.Y, p2.X, p2.Y, m, n)
	return engo.Point{m, n}
}

type FoodNode struct {
	ecs.BasicEntity
	common.SpaceComponent
	common.RenderComponent
}
