# sokoban

Simple sokoban solver using IDA-star algorithm. Heuristics used is simple manhatan distance of each box from the nearest spot.

## format of plan
List of strings - each string is a line where:

| \# | wall |
| x | spot for a box |
| s | sokoban |
| @ | box |
| o | empty |

Sample run:

```prolog
sokoban(["###",
    "#s#,
    "#@#",
    "#x#",
    "###"], Sol)
```

The solution is a list Sol containing instructions `l, r, u, d` for Sokoban to move.
