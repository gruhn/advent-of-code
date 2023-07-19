
namespace Gen

  inductive Gen (a : Type u) where
    | end 
    | next (head : a) (tail : Unit -> Gen a)

  def head : Gen a -> Option a 
    | Gen.end         => none
    | Gen.next head _ => some head

  def fromList : List a -> Gen a
    | []      => Gen.end
    | x :: xs => Gen.next x (fun () => Gen.fromList xs)

  def toList : Gen a -> List a
    | Gen.end => []
    | Gen.next head tail => head :: Gen.toList (tail ())

  #eval fromList [1,2,3] |> toList

  def map (f : a -> b) : Gen a -> Gen b 
    | Gen.end            => Gen.end
    | Gen.next head tail => Gen.next (f head) (fun () => Gen.map f (tail ()))

  instance : Functor Gen where
    map := map

  def take (n : Nat) : Gen a -> Gen a 
    | Gen.end => Gen.end
    | Gen.next head tail => 
        if n == 0 then
          Gen.end
        else 
          Gen.next head (fun () => take (n-1) (tail ()))

  def Unfoldr a b := (b -> Option (Prod a b)) -> b -> Gen a

  instance : Inhabited (Unfoldr a b) where
    default := fun _ _ => Gen.end

  partial def unfoldr : Unfoldr a b := fun f state =>
    match f state with
      | none                => Gen.end
      | some (item, state') => Gen.next item (fun _ => unfoldr f state')

  partial def iterate (f : a -> a) (start : a) : Gen a :=
    unfoldr (fun state => (state, f state)) start

  partial def range (start : Nat) (to : Nat) : Gen Nat :=
    iterate (fun x => x+1) start |> take (to+1)

  #eval (toList <| range 0 10)

  def concat (gen1 : Gen a) (gen2 : Gen a) : Gen a :=
    match gen1 with
      | Gen.end => gen2
      | Gen.next head tail => Gen.next head (fun () => concat (tail ()) gen2)

  #eval toList <| concat (fromList [1,2]) (fromList [3,4])

  partial def cycleAux (original : Gen a) : Gen a -> Gen a
    | Gen.end            => cycleAux original original
    | Gen.next head tail => Gen.next head (fun _ => cycleAux original (tail ()))

  def cycle (gen : Gen a) : Gen a := cycleAux gen gen

  #eval fromList [1,2,3] |> cycle |> take 10 |> toList

  def scanl (f : b -> a -> b) (state : b) : Gen a -> Gen b
    | Gen.end            => Gen.end
    | Gen.next head tail => Gen.next state (fun _ => scanl f (f state head) (tail ()))

end Gen
