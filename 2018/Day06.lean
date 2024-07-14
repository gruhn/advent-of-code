
structure Point where
  x : Int
  y : Int

instance : Inhabited Point where
  default := { x := default, y := default }

instance : ToString Point where
  toString p := s!"({p.x},{p.y})"

/-- 
  Manhattan distance between `p1` and `p2`.
-/
def dist (p1: Point) (p2: Point): Nat := 
  (p1.x - p2.x).natAbs + (p1.y - p2.y).natAbs

def regionSize (ps: List Point) (p: Point): Option Nat :=
  _

def belongs_to (p: Point) (center: Point) (other_centers: List Point) : Prop :=
  forall center', Not (center = center') /\ List.Mem center' other_centers -> dist p center < dist p center'

theorem regionOf_sound : forall (p: Point) (ps : List Point) (x y : Int),
  Iff 
  regionOf p ps

def parseLine (line: String): Point :=
  let coords_str := String.splitOn line ", ";
  let coords_int := coords_str.mapM String.toInt?;
  match coords_int with
  | some [x, y] => { x, y }
  | _                => panic "parse error"

def parseFile (input: String): List Point := 
  String.splitOn input (sep := "\n")
    |> List.map parseLine

def main : IO Unit := do
  let input <- String.trim <$> IO.FS.readFile "input/06.txt"
  let points := parseFile input

  IO.print "Part 1: "
  IO.println <| points

  IO.print "Part 2: "
