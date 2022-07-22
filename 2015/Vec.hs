module Vec (Vec2(Vec2)) where

newtype Vec2 a = Vec2 { getVec2 :: (a,a) }

instance Num a => Num (Vec2 a) where
    Vec2 (x1,y1) + Vec2 (x2,y2) = Vec2 (x1+x2,y1+y2)
    Vec2 (x1,y1) * Vec2 (x2,y2) = Vec2 (x1*x2,y1*y2)
    abs (Vec2 (x,y)) = Vec2 (abs x, abs y)
    signum (Vec2 (x,y)) = Vec2 (signum x, signum y)
    fromInteger x = Vec2 (fromInteger x, fromInteger x)
    negate (Vec2 (x,y)) = Vec2 (negate x, negate y)

instance Eq a => Eq (Vec2 a) where
    Vec2 v == Vec2 w = v == w

instance Ord a => Ord (Vec2 a) where
    Vec2 v <= Vec2 w = v <= w

instance Show a => Show (Vec2 a) where 
    show (Vec2 (x,y)) = show (x,y)