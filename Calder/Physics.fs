namespace Calder

module Physics =

    type Direction = {
        DX: float
        DY: float
        }
        with
        static member (*) (scalar: float, direction: Direction) =
            {
                DX = scalar * direction.DX
                DY = scalar * direction.DY
            }
        static member (+) (dir1: Direction, dir2: Direction) =
            {
                DX = dir1.DX + dir2.DX
                DY = dir1.DY + dir2.DY
            }
        static member Zero = { DX = 0.0; DY = 0.0 }
        member this.Length =
            pown this.DX 2 + pown this.DY 2
            |> sqrt

    type Point = {
        X: float
        Y: float
        }
        with
        static member (+) (pt: Point, direction: Direction) =
            { X = pt.X + direction.DX; Y = pt.Y + direction.DY }
        static member (-) (pt1: Point, pt2: Point) =
            { DX = pt2.X - pt1.X; DY = pt2.Y - pt1.Y }

    let distance (p1: Point) (p2: Point) =
        (p1 - p2).Length

    type Force =
        abstract member applyFrom: origin: Point -> target: Point -> Direction

    let Neutral = {
        new Force with
            member this.applyFrom _ _ = Direction.Zero
        }

    type Spring = {
        Length: float
        Stiffness: float
        }
        with
        interface Force with
            member this.applyFrom origin target =
                let direction = target - origin
                let strength = this.Stiffness * (direction.Length - this.Length)
                strength * direction

    // Coulomb-style repulsion force
    type CoulombRepulsor = {
        Repulsion: float
        }
        with
        interface Force with
            member this.applyFrom origin target =
                let direction = target - origin
                let strength =
                    this.Repulsion / (pown direction.Length 2)
                - strength * direction

    type LinearRepulsor = {
        Length: float
        }
        with
        interface Force with
            member this.applyFrom origin target =
                let direction = target - origin
                let strength =
                    this.Length - direction.Length
                    |> max 0.0
                - strength * direction

    type Attractor = {
        Strength: float
        }
        with
        interface Force with
            member this.applyFrom origin target =
                let direction = target - origin
                this.Strength * direction
