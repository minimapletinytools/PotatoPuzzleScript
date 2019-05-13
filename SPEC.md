# 🥔🧩📜 Spec (WIP)

### Coordinate System

Coordinates or as follows
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
If restricted to just x y plane where z = 0, then should be mostly equivalent to Puzzle Script

## Execution Phases

1. input phase: wait until timer or new input
2. main execution phase: match rules and do replacement
3. movement update phase: update all moving objects by their velocity if possible (no collisions)
4. late phase: match all late rules. No movement is allowed in late rules

## Rule Execution

A `rule` is the one of the following:

### command
Execute the `command`.
<TODO list commands>

### pattern match with optional modifiers
If the LHS `patterns` is matched, *replace* with the RHS `patterns`. A LHS `patterns` in a `pattern match` may have an optional `directional modifier`. See Pattern Matching section for more details.

### boolean statement followed by a rule
A `boolean statement` is either a `pattern expression` or a `boolean expression` followed by a `rule`. If the pattern experssion is matched or the expression evaluates to true, execute the RHS `rule`. In this case we say the LHS evaluates to true. If it does not, we say the LHS evaluates to false.

A rule is *done* if
1. it is a `command` that executed once
2. it is a `pattern match` where the LHS `patterns` matches nothing
3. it is a `boolean statement` where the RHS `rule` is *done* or the LHS evaluates to *false*.

## Rule Group Execution

each `rule group` is made of one or more `rules` or `rule groups`

to execute an `rule group`, execute each contained `rule` or `rule group` in order and repeat until all rules are *done*

the currently executing rule group is the `K'th rule group`
the `rule group` that executed the `K'th rule group` is the `K-1'th rule group`

if the `CANCEL N` command is executed, rollback to the start of execution of the `K-N'th rule group` and continue execution skipping the *cancelled* rule group


## Pattern Matching

A `pattern expression` has the following structure:
```
{DIRECTIONAL MODIFIER} [pattern] {[pattern] ...}
```

### Directional Modifier

Each pattern match may have an optional `directional modifier`. If there is no directional modifier, the default 8 cardinal directions are used. The rule is evaluated for each direction in the directional modifier. This is referred to as the `direction` of the evaluation.

### Pattern

A `pattern` has the following structure:
```
[pattern_object {| pattern_object | ...}]
```
and a `pattern_object` has the following structure:
```
{VELOCITY} {ORIENTATION} object
```
if no `velocity` is specified the object is not moving
if no `orientation` is specified, than all orientations are matched.

To match a `pattern`,
1. find an object matching the first pattern_object relative to the direction
2. if found, set the marker to the position of the matched object else FAIL
3. if there are more pattern_objects, move the marker once by the direction else PASS
4. if the match next pattern_object matches the object where the marker is at, go to step 3 else FAIL

The pattern expression is matched if each pattern it contains is matched

### Pattern Replacement

TODO
```
{DIRECTIONAL MODIFIER} [pattern] {[pattern] ...} -> [pattern] {[pattern] ...}
```

If matches, the LHS `patterns` is replaced with the RHS `patterns`.

The RHS `patterns` must have the same *structure* as the LHS `patterns`.

<TODO more about pattern replacement>




<TODO>


### Orientation

Orientations are denoted using 2 vectors. The first one is the facing direction, and the second is the up direction.

FFORWARD: (0,1,0) (0,0,1)
FBACKWARD: (0,-1,0) (0,0,1)
FUP: FFORWARD
FDOWN: FBACKWARD
FRIGHT: (-1,0,0) (0,0,1)
FLEFT: (-1,0,0) (0,0,1)

All `orientations` are *relative* to `direction` of the rule. It can be made *absolute* by adding `Abs_` in front e.g.

```
Abs_FFORWARD
```

an `orientation` can also be made by combining existing orientations with the `or` operator to match more than one e.g.

```
FFORWARD || FBACKWARD
```

### Velocity

The following available `velocity` constants are denoted with two vectors. The first one is the translation, and the second is the rotation in euler angles. Some constants are denoted as other constants combined with or.

~`velocities` can be *absolute* or *relative*. They can be coerced to the other type by adding `Rel` or `Abs` in front of the respectively.~

<TODO allow using direction as velocity>

\>: Rel (1,0,0) id
<: Rel (-1,0,0) id
^: Rel (0,1,0) id
v: Rel (0,-1,0) id
^^: Rel (0,0,1) id
vv: Rel (0,0,-1) id

PLUSZ: Abs (0,0,1) id
MINUSZ: Abs (0,0,-1) id
RIGHT: Abs (1,0,0) id
LEFT: Abs (-1,0,0) id
UP: Abs (0,1,0) id
DOWN: Abs (0,-1,0) id
LEFT: Abs (-1,0,0) id

TURN_LEFT: Rel id (0,0,π/2)
TURN_RIGHT: Rel id (0,0,-π/2)
vertical: UP or DOWN
horizontal: LEFT or RIGHT
parallel: > or <
perpendicular: ^ or v

a `velocity` can also be made by combining existing velocities with the `or` operator e.g.

```
LEFT or RIGHT
```



### DIRECTION

The following available `direction` constants are denoted with a single vector describing the direction or multiple directions.

RIGHT: (1,0,0)
LEFT: (-1,0,0)
UP: (0,1,0)
DOWN: (0,-1,0)
PLUS_Z: (0,0,1)
MINUS_Z: (0,0,-1)

a `direction` can also be made by combining existing directions with the `or` operator e.g.

```
LEFT or RIGHT
```



OLD STUFF BELOW HERE
### Pattern


### Pattern Matching

a pattern match has the following syntax

```
{scope} [{vel} {rot} object {| ...}] -> [{vel} {rot} object {| ...}]
```

and has the following constraints on LHS and RHS of the expression:

TODO

if scope is nil, then translation component of first bound velocity is used
if scope is SCOPE_PATTERN, then all directions matching SCOPE_PATTERN are tried for scope (i.e. duplicate rules for each scope matching SCOPE_PATTERN)

if {vel} is abs(x) then match object with velocity x and bind velocity to x
if {vel} is rel(x) then match object with velocity scope * x
if {vel} is nil then match object with velocity 0

if {rot} is abs(x) then match object with rotation x
if {rot} is rel(x) then match object with rotation rot(scope) * rot
if {rot} is nil, then match object with any rotation

| operator is relative to scope, i.e. A|B means B is matched with trans(A)*scope
