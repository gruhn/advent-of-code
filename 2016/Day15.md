## Input 

- Disc #1 has 13 positions; at time=0, it is at position 11.
- Disc #2 has 5 positions; at time=0, it is at position 0.
- Disc #3 has 17 positions; at time=0, it is at position 11.
- Disc #4 has 3 positions; at time=0, it is at position 0.
- Disc #5 has 7 positions; at time=0, it is at position 2.
- Disc #6 has 19 positions; at time=0, it is at position 17.

## Solution

Solve for `x` using chinease remainder theorem (e.g. [with this tool](https://atozmath.com/ChineseRmThm.aspx?q=1)) in

```python
x = (-11 -1) (mod 13)
x = (  0 -2) (mod 5)
x = (-11 -3) (mod 17)
x = (  0 -4) (mod 3)
x = ( -2 -5) (mod 7)
x = (-17 -6) (mod 19)

# for part 2, also include:
x = (  0 -7) (mod 11)
```
