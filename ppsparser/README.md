coordinates or as follows

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

If looking at just x y plane where z = 0, then should be equivalent to Puzzle Script

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
PVELS:
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
RVELS:
TURN_LEFT: R (0,0,π/2)
TURN_RIGHT: R (0,0,-π/2)
TURN_PLUSZ: (π/2,0,0)
TURN_MINUSZ: (-π/2,0,0)





rule = command | <scope> patterns -> patterns | <scope> patterns -> rule | bexpr -> rule
lhs = <scope> patterns | bexpr
bexpr = TRUE | FALSE | bexpr bop bexpr | algexpr abop algexpr
algexpr = ...
scope = TR
patterns = pattern | pattern patterns
objExpr = rmod objExpr | objExpr bop objExpr | obj
rmod = 
vmod =
pattern = [subpattern]
subpattern = vmod objExpr | (vmod objExpr pop subpattern) | ...
pop = |




regular velocties

[V P A | B] -> [V P A | abs_V B]

no scope
  match A with rot(A) = P && vel(A) = V
    scope V (because no scope, matches PS behavior)
      match B with

DIR [A | P B] -> [A | > B]
scope DIR
  match A
    match B with pos(B) = pos(A) + scope*1 && rot(B) = P
      set vel B = scope


relative velocities
rel(V) only supported for >^<v, these are matched relative to the scope direction

[rel(V) P A | B] -> [rel(V) P A | rel(V) B]

no global
  scope rel_V (vel of matched A)
    match A with rot(A) = P && vel(A) = V
      match B with pos(B) = pos(A) + scope*1
        set vel(B) = scope
