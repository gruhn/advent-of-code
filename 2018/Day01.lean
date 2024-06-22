import Lean.Data.HashSet
import Batteries.Data.LazyList
 
def toInt (str : String) : Option Int :=
  match String.front str with
    | '+' => String.toNat? (String.drop str 1)
    | '-' => Int.negOfNat <$> String.toNat? (String.drop str 1)
    | _   => none
    
def parse (input : String) : List Int :=
  String.splitOn input "\n"
    |> List.filterMap toInt

def findFirstDuplicate (seen : Lean.HashSet Int) : LazyList Int -> Option Int
  | LazyList.nil            => none
  | LazyList.cons head tail => 
      if seen.contains head then
        head
      else 
        findFirstDuplicate (seen.insert head) tail.get

def scanl (f : b -> a -> b) (state : b) : LazyList a -> LazyList b
  | LazyList.nil            => LazyList.nil
  | LazyList.cons head tail => LazyList.cons state <| scanl f (f state head) tail.get

partial def cycleAux (original : LazyList a) : LazyList a -> LazyList a
  | LazyList.nil            => cycleAux original original
  | LazyList.cons head tail => LazyList.cons head <| cycleAux original tail.get

def cycle (gen : LazyList a) : LazyList a := cycleAux gen gen

def main : IO Unit := do
  let input <- parse <$> IO.FS.readFile "input/01.txt"

  IO.print "Part 1: "
  IO.println <| 
    List.foldl (fun a b => a + b) 0 input
    
  IO.print "Part 2: "
  IO.println <| 
    (LazyList.ofList input
      |> cycle
      |> scanl (fun a b => a + b) 0
      |> findFirstDuplicate Lean.HashSet.empty
    )
