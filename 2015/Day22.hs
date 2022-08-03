module Main where
import Lens.Micro (lens, over, set, (^.), Lens')
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Foldable (find)
import Data.Tree (unfoldForest, Forest, Tree (Node, subForest), unfoldTree, drawTree, drawForest)
import Data.Maybe (fromMaybe)
import Data.List (mapAccumR)

data Spell = MagicMissle | Drain | Shield | Poison | Recharge
    deriving (Eq, Show)

data Effect = Effect
    { _timer :: Int
    , _spell :: Spell
    } deriving Show

data GameState = GS
    { _mana :: Int
    , _hitPoints :: Int
    , _bossDamage :: Int
    , _bossHitPoints :: Int
    , _effects :: [Effect]
    } deriving Show

timer :: Lens' Effect Int
timer = lens _timer (\state v -> state { _timer = v })

spell :: Lens' Effect Spell
spell = lens _spell (\state v -> state { _spell = v })

mana :: Lens' GameState Int
mana = lens _mana (\state v -> state { _mana = v })

hitPoints :: Lens' GameState Int
hitPoints = lens _hitPoints (\state v -> state { _hitPoints = v })

bossDamage :: Lens' GameState Int
bossDamage = lens _bossDamage (\state v -> state { _bossDamage = v })

bossHitPoints :: Lens' GameState Int
bossHitPoints = lens _bossHitPoints (\state v -> state { _bossHitPoints = v })

effects :: Lens' GameState [Effect]
effects = lens _effects (\state v -> state { _effects = v })

castSpell :: Spell -> GameState -> GameState
-- Magic Missile costs 53 mana. It instantly does 4 damage.
castSpell MagicMissle state = state
    & over mana (+ (-53))
    & over bossHitPoints (+ (-4))
-- Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
castSpell Drain state = state
    & over mana (+ (-73))
    & over bossHitPoints (+ (-2))
    & over hitPoints (+2)
-- Shield costs 113 mana. It starts an effect that lasts for 6 turns. 
-- While it is active, your armor is increased by 7.
castSpell Shield state = state
    & over mana (+ (-113))
    & over effects (Effect 6 Shield :)
-- Poison costs 173 mana. It starts an effect that lasts for 6 turns. 
-- At the start of each turn while it is active, it deals the boss 3 damage.
castSpell Poison state = state
    & over mana (+ (-173))
    & over effects (Effect 6 Poison :)
-- Recharge costs 229 mana. It starts an effect that lasts for 5 turns. 
-- At the start of each turn while it is active, it gives you 101 new mana.
castSpell Recharge state = state
    & over mana (+ (-229))
    & over effects (Effect 5 Recharge :)

applyEffect :: Effect -> GameState -> GameState
applyEffect (Effect _ Poison) state = state
    & over bossHitPoints (+ (-3))
applyEffect (Effect _ Recharge) state = state
    & over mana (+ 101)
applyEffect _ state = state

applyEffects :: GameState -> GameState
applyEffects state =
    foldr applyEffect state (state ^. effects)

decrementTimers :: GameState -> GameState
decrementTimers state = state
    & over (effects . traverse . timer) (+ (-1))
    & over effects (filter ((>0) . (^. timer)))

armor :: GameState -> Int
armor state =
    let getArmor (Effect _ Shield) = 7
        getArmor _ = 0
    in  state ^. effects <&> getArmor & sum

damage :: GameState -> Int
damage state =
     state ^. bossDamage - armor state

bossAttack :: GameState -> GameState
bossAttack state =
    over hitPoints (+ (- damage state)) state

movesPerRound :: Spell -> [GameState -> GameState]
movesPerRound spell =
    [ applyEffects, decrementTimers, castSpell spell -- player turn
    , applyEffects, decrementTimers, bossAttack ]    -- boss turn

movesPerGame :: [Spell] -> [GameState -> GameState]
movesPerGame = concatMap movesPerRound

isBossWin :: GameState -> Bool
isBossWin state =
    let bossDead   s = s ^. bossHitPoints <= 0
        playerDead s = s ^. hitPoints <= 0
        manaEmpty  s = s ^. mana < 0
    in not (bossDead state) && (playerDead state || manaEmpty state)

isPlayerWin :: GameState -> Bool
isPlayerWin state =
    let bossDead   s = s ^. bossHitPoints <= 0
        playerDead s = s ^. hitPoints <= 0
    in  bossDead state && not (playerDead state)

isWinState :: GameState -> Bool
isWinState state =
    isBossWin state || isPlayerWin state

endState :: [GameState] -> GameState
endState states =
    let winState = find isWinState states
    in  fromMaybe (last states) winState

play :: [Spell] -> GameState -> GameState
play strategy state0 = strategy
    & movesPerGame
    & scanl (flip ($)) state0
    & endState

strategySpace :: GameState -> Forest Spell
strategySpace state0 =
    let spells = [ Shield, Drain, Poison, MagicMissle, Recharge ]

        build :: (GameState, Spell) -> ((GameState, Spell), [(GameState, Spell)])
        build (state0, spell) =
            let stateN = play [spell] state0
            in  ((stateN, spell), zip (repeat stateN) spells)

        prune :: Forest (GameState, Spell) -> Forest (GameState, Spell)
        prune [] = []
        prune (Node (state, spell) children : trees)
            | isBossWin state = prune trees
            | isPlayerWin state = Node (state, spell) [] : prune trees
            | otherwise = case prune children of
                []        -> prune trees
                children' -> Node (state, spell) children' : prune trees

        space = zip (repeat state0) spells
            & fmap (unfoldTree build)
            & prune
            & fmap (fmap snd)

    in  space

pruneBottomUp :: (a -> Bool) -> Forest a -> Forest a
pruneBottomUp reject [] = []
pruneBottomUp reject (Node label [] : trees)
    | reject label = pruneBottomUp reject trees
    | otherwise = Node label [] : pruneBottomUp reject trees
pruneBottomUp reject (Node label children : trees)
    | reject label = pruneBottomUp reject trees
    | otherwise = case pruneBottomUp reject children of
        [] -> pruneBottomUp reject trees
        cs -> Node label cs : pruneBottomUp reject trees

search :: Forest Spell -> (Int, Forest Spell)
search forest =
    let manaCost :: Spell -> Int
        manaCost MagicMissle = 53
        manaCost Drain = 73
        manaCost Shield = 113
        manaCost Poison = 173
        manaCost Recharge = 229

        withPathCost :: (a -> Int) -> Int -> Forest a -> Forest (Int, a)
        withPathCost f ancestorCost [] = []
        withPathCost f ancestorCost (Node label children : trees) = 
            let children' = withPathCost f (ancestorCost + f label) children
                node' = Node (ancestorCost + f label, label) children' 
            in  node' : withPathCost f ancestorCost trees

        minPath :: Int -> Forest (Int, a) -> Int
        minPath best [] = best
        minPath best (Node (cost, label) [] : trees)
            | cost >= best = minPath best trees
            | otherwise    = minPath cost trees
        minPath best (Node (cost, label) children : trees)
            | cost >= best = minPath best trees
            | otherwise    = minPath (minPath best children) trees

        withPathCost' = withPathCost manaCost 0 forest
        minPathCost = minPath 10000 withPathCost' 
        forest' = (snd <$>) <$> pruneBottomUp ((> minPathCost) . fst) withPathCost'

    in  (minPathCost, forest')

main :: IO ()
main = do

    let initialState = GS
            { _mana = 500
            , _hitPoints = 50
            , _bossDamage = 9
            , _bossHitPoints = 51
            , _effects = []
            }

   -- let initialState = GS
    --         { _mana = 250
    --         , _hitPoints = 10
    --         , _bossDamage = 8
    --         , _bossHitPoints = 14
    --         , _effects = []
    --         }

    -- print $ play [ Recharge, Shield, Drain, Poison, MagicMissle ] initialState
    -- print $ play [Drain, Shield] initialState

    -- putStr $ drawForest 
    --     $ fmap (fmap show) 
    --     $ snd . search
    --     $ strategySpace initialState

    print 
        $ fst . search
        $ strategySpace initialState


-- 854 too low