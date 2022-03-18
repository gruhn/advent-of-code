## Input

```
CardPK = 3469259
DoorPK = 13170438
```

## Solution:

(solve with wolframalpha.com)

```
7^CardLS `mod` 20201227 = CardPK 
    => CardLS = 13739269

7^DoorLS `mod` 20201227 = DoorPK
    => DoorLS = 11114599

CardPK^DoorLS `mod` 20201227 = 7269858
DoorPK^CardLS `mod` 20201227 = 7269858
```