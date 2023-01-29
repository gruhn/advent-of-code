import Lean.Data.Parsec
import Lean.Data.HashSet
import Lean.Data.Stream
 
def toInt (str : String) : Option Int :=
  match String.front str with
    | '+' => String.toNat? (String.drop str 1)
    | '-' => Int.negOfNat <$> String.toNat? (String.drop str 1)
    | _   => none
    
def parse (input : String) : List Int :=
  String.splitOn input "\n"
    |> List.filterMap toInt
    
def scanl (f : b -> a -> b) (init : b) : List a -> List b
  | []      => [init]
  | x :: xs => (f init x) :: scanl f (f init x) xs
  
-- #check Lean.Parsec
-- #check Parsec
-- #check Lean.HashSet
  
def firstDuplicateAux [BEq a] [Hashable a] (seen : Lean.HashSet a) (list : List a) : Option a :=
  match list with
    | []      => none
    | x :: xs => 
      if seen.contains x then
        some x
      else
        firstDuplicateAux (seen.insert x) xs

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "input/01.txt"

  IO.print "Part 1: "
  IO.println <| 
    List.foldl (fun a b => a + b) 0 input
    
  IO.print "Part 2: "
  Lean.Stream.toStream input
    |> scanl (fun a b => a + b) 0 input
    -- <| firstDuplicateAux Lean.HashSet.empty

    |> IO.println
