# 🥔🧩📜

### Coordinate System

coordinates or as follows
```
        +z
        ^   +y
        |  ^
        | /
        |/
 <------/-------> +x
       /
      /
     /
    v
```
It is only possible to use the +x +y +z octant
If looking at just x y plane where z = 0, then should be equivalent to Puzzle Script



### Rotations and Velocities WIP

base orientation is looking down +Y axis with up in the +Z axis

AR (facing) (up): absolute rotation
AR (euler): absolute rotation as relative rotation from AR (1,0,0) (0,0,1)
T (pos): relative translation
R (euler): relative rotation

Orientations: (facing) (up)
R_FORWARD: AR (0,1,0) (0,0,1)
R_UP: R_FORWARD
R_LEFT: AR (-1,0,0) (0,0,1)
R_RIGHT: AR (-1,0,0) (0,0,1)
R_BACKWARDS: AR (0,-1,0) (0,0,1)
R_DOWN: R_BACKWARDS


VELOCITIES:
RIGHT: T (1,0,0)
UP: T (0,1,0)
DOWN: T (0,-1,0)
LEFT: T (-1,0,0)
M_PLUSZ: T (0,0,1)
M_MINUSZ: T (0,0,-1)
M_RIGHT: RIGHT
M_UP: UP
M_DOWN: DOWN
M_LEFT: LEFT
TURN_LEFT: R (0,0,π/2)
TURN_RIGHT: R (0,0,-π/2)
TURN_PLUSZ: (π/2,0,0)
TURN_MINUSZ: (-π/2,0,0)




### Pattern Matching

{scope} [{vel} {rot} object {| ...}] -> [{vel} {rot} object {| ...}]

if scope is nil, then translation component of first bound velocity is used
if scope is SCOPE_PATTERN, then all directions matching SCOPE_PATTERN are tried for scope (i.e. duplicate rules for each scope matching SCOPE_PATTERN)

if {vel} is abs(x) then match object with velocity x and bind velocity to x
if {vel} is rel(x) then match object with velocity scope * x
if {vel} is nil then match object with velocity 0

if {rot} is abs(x) then match object with rotation x
if {rot} is rel(x) then match object with rotation rot(scope) * rot
if {rot} is nil, then match object with any rotation

| operator is relative to scope, i.e. A|B means B is matched with trans(A)*scope
