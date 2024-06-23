instance : Monad List where
  pure x := [x]
  bind mx f := (mx.map f).join
    
structure Cycle (a : Type) where
  left  : List a
  right : List a

namespace Cycle 

  def ofList (items : List a) : Cycle a :=
    { left := [], right := items }

  def toList (cycle : Cycle a) : List a :=
    cycle.right ++ cycle.left.reverse

  def turnRight (cycle : Cycle a) : Cycle a :=
    match cycle.right with
      | []      => ofList cycle.left.reverse
      | (a::as) => { left := a :: cycle.left, right := as }

  def length (cycle : Cycle a) :=
    cycle.left.length + cycle.right.length

end Cycle

def react (chars: Cycle Char) : Option (Cycle Char) :=
  match chars with
  | { left := l::ls, right := r::rs } =>
    if l != r && l.toLower == r.toLower then
      some { left := ls, right := rs }
    else
      none
  | _ => none

def reactAll (str : String) : String :=
  let rec go (chars : Cycle Char) : Nat -> Cycle Char 
    | Nat.zero       => chars
    | Nat.succ steps =>
      match react chars with
      | none        => go chars.turnRight steps
      | some chars' => go chars' steps
  go (Cycle.ofList str.toList) str.length
    |> Cycle.toList
    |> List.asString

def main : IO Unit := do
  let input <- String.trim <$> IO.FS.readFile "input/05.txt"

  IO.print "Part 1: "
  IO.println <| String.length <| reactAll input

  IO.print "Part 2: "
  IO.println <| List.minimum? <| do
    let char <- "abcdefghijklmnopqrstuvwxyz".toList
    let input' := input.toList.filter (fun c => Char.toLower c != char)
    return String.length <| reactAll input'.asString
