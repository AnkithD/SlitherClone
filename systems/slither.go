package systems

import (
	"engo.io/ecs"
	"engo.io/engo"
	"engo.io/engo/common"
	"fmt"
	"image/color"
	"math"
	"math/rand"
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
	}

	ss.Player.Body = append(ss.Player.Body, &SlitherNode{
		BasicEntity: ecs.NewBasic(),
		SpaceComponent: common.SpaceComponent{
			Position: engo.Point{640, 384},
			Width:    ss.Player.NodeRadius * 2,
			Height:   ss.Player.NodeRadius * 2,
		},
		RenderComponent: common.RenderComponent{
			Drawable: common.Circle{},
			Color:    ss.Player.BaseColor,
		},
	})
	ActiveSystems.RenderSys.Add(
		&(ss.Player.Body[0].BasicEntity),
		&(ss.Player.Body[0].RenderComponent),
		&(ss.Player.Body[0].SpaceComponent),
	)

	fmt.Println("Initialized Slither System")
}
func (ss *SlitherSystem) Update(dt float32) {}
func (ss *SlitherSystem) AddSlither(pos engo.Point, col color.RGBA) {
	tempSlither := &SlitherEntity{
		BaseColor:  col,
		ColorCount: 1,
		NodeRadius: ss.DefaultRadius,
		NodeGap:    ss.DefaultRadius / 2,
		Body:       make([]*SlitherNode, 0),
	}
	tempSlither.Body = append(tempSlither.Body, &SlitherNode{
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
	})

	ActiveSystems.RenderSys.Add(
		&tempSlither.Body[0].BasicEntity,
		&tempSlither.Body[0].RenderComponent,
		&tempSlither.Body[0].SpaceComponent,
	)

	ss.Slithers = append(ss.Slithers, tempSlither)
}

func (ss *SlitherSystem) GetNextShade(BaseCol color.RGBA, Count int) color.RGBA {
	r, g, b, a := BaseCol.R, BaseCol.G, BaseCol.B, BaseCol.A
	ratio := (math.Abs(float64(10-(Count%20))) / float64(10))
	r = uint8(float64(r) * ratio)
	g = uint8(float64(g) * ratio)
	b = uint8(float64(b) * ratio)

	return color.RGBA{r, g, b, a}
}

type SlitherEntity struct {
	BaseColor  color.RGBA
	ColorCount int
	Body       []*SlitherNode
	NodeGap    float32
	NodeRadius float32
}

type SlitherNode struct {
	ecs.BasicEntity
	common.RenderComponent
	common.SpaceComponent
}
