[![Build Status](https://travis-ci.com/pdlla/PotatoPuzzleScript.svg?token=1wu7pMbHtbc6T6qruqqT&branch=master)](https://travis-ci.com/pdlla/PotatoPuzzleScript)

THIS IS A WIP

# 🥔PuzzleScript

🥔PuzzleScript is a markup language for building 3D puzzle games based on [PuzzleScript](https://github.com/increpare/PuzzleScript).

🥔PuzzleScript is done in Haskell.

It's called 🥔PuzzleScript because 🥔s are 3D.


## Current State

Parsers done for simplified rules
Parsers property tests WIP
Just started on interpreter
Added Vty terminal interface for testing interpreter

## Roadmap

- finish parser to generate AST (50%)
  - finish outstanding issues with velocity and orientation
  - add support for + operator and start/endLoop
  - finish commands
  - add support for boolean arithmetic
- finish AST interpreter (1%)
- full featured Haskell terminal interface (10%)
- C bindings
- Rust front end (separate repo)

## Features (planned)

- support full PuzzleScript markup language, with appropriate 🥔 modifications where it makes sense
- design and implement 🥔PuzzleScript language capable of expressing mechanics of [Stephen's Sausage Roll](https://www.increpare.com/2016/04/stephens-sausage-roll/) and [English Country Tune](https://www.increpare.com/2011/11/english-country-tune/)
- level editor


## Notable Changes from PuzzleScript

- 🥔🥔🥔🥔
- input is handled using rules rather than as a built in primitive. Prelude commands are available for default rules that match Puzzle Script functionality
- more generalized notion of rule groups and loops
- no "rigid" keyword. Instead, rigid bodies are implemented using loops and a scoped cancel command (details TBD)
