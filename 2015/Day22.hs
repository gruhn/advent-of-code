module Main where
import Lens.Micro (lens, over, set, (^.), Lens')
import Data.Function ((&))
import Data.Functor ((<&>))

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

playerTurn :: Spell -> GameState -> GameState
playerTurn spell state = state
    & decrementTimers
    & applyEffects
    & castSpell spell

armor :: GameState -> Int
armor state = 
    let getArmor (Effect _ Shield) = 7
        getArmor _ = 0
    in  state ^. effects <&> getArmor & sum

damage :: GameState -> Int
damage state = 
     state ^. bossDamage - armor state

bossTurn :: GameState -> GameState
bossTurn state = state
    & decrementTimers
    & applyEffects
    & over hitPoints (+ (- damage state))

play :: [Spell] -> GameState -> GameState
play [] state = state
play (spell:spells) state = state
    & playerTurn spell
    & bossTurn
    & play spells

main :: IO ()
main = do 
    -- let initialState = GS
    --         { _mana = 500
    --         , _hitPoints = 50
    --         , _bossDamage = 9
    --         , _bossHitPoints = 51
    --         , _effects = []
    --         } 

    let initialState = GS
            { _mana = 250
            , _hitPoints = 10
            , _bossDamage = 8
            , _bossHitPoints = 13
            , _effects = []
            } 

    print $ play [Poison, MagicMissle] initialState