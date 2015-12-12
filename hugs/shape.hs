
type Pair a = (a,a)
type Size = Pair Integer
type Pos = Pair Integer
type Rectangle = Pair Pos
type Block = [Rectangle]
type Bundle = (Size, [Block])

origin = (0,0)
corner1 = (10,10)
corner2 = (20,20)
corner3 = (30,30)

rect1 = (origin, corner1)
rect2 = (corner2, corner3)

block1 = [rect1]
block2 = [rect2]

bundle1 = ((4,5) , [block1, block2])
