import Lean.Data.HashSet
import Gen
 
def toInt (str : String) : Option Int :=
  match String.front str with
    | '+' => String.toNat? (String.drop str 1)
    | '-' => Int.negOfNat <$> String.toNat? (String.drop str 1)
    | _   => none
    
def parse (input : String) : List Int :=
  String.splitOn input "\n"
    |> List.filterMap toInt

def findFirstDuplicate (seen : Lean.HashSet Int) : Gen.Gen Int -> Option Int
  | Gen.Gen.end            => none
  | Gen.Gen.next head tail => 
      if seen.contains head then
        head
      else 
        findFirstDuplicate (seen.insert head) (tail ())

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "input/01.txt"

  IO.print "Part 1: "
  IO.println <| 
    List.foldl (fun a b => a + b) 0 input
    
  IO.print "Part 2: "
  IO.println <| 
    (Gen.fromList input
      |> Gen.cycle
      |> Gen.scanl (fun a b => a + b) 0
      |> findFirstDuplicate Lean.HashSet.empty
    )
