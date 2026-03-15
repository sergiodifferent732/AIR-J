# Hunt The Wumpus

This example is a complete AIR-J console game with AIR-J unit tests. It is split into:

- pure game logic in `wumpus_logic.airj`
- the console game in `hunt_the_wumpus.airj`
- reusable AIR-J test suite logic in `hunt_the_wumpus_test_suite.airj`
- a canonical text test root in `hunt_the_wumpus_tests_root.airj`
- a canonical JSON test root in `hunt_the_wumpus_tests_json.airj`

## Files

- `wumpus_logic.airj`: pure game logic helpers plus move/shot resolution
- `hunt_the_wumpus.airj`: the playable console game
- `hunt_the_wumpus_test_suite.airj`: reusable AIR-J test suite logic
- `hunt_the_wumpus_tests_root.airj`: canonical AIR-J text test root
- `hunt_the_wumpus_tests_json.airj`: canonical AIR-J JSON test root

The canonical AIR-J project test shape is:

- reusable suite modules export uniquely named suite functions
- explicit runnable test-root modules export:
- exported `tests : () -> (Seq TestOutcome)`
- exported `main : (StringSeq) -> Int`
- root `main` delegates to `airj/test-runner.run` or `airj/test-runner.run-json`
- this keeps test aggregation explicit instead of relying on discovery heuristics

The tests exercise:

- command parsing through canonical `Option` and `Result` values
- pure move and shot resolution
- state transitions for pits, bats, misses, and wins
- the console shell only as a thin wrapper around the pure logic

## Build A Runnable Jar

From the repo root:

```bash
clj -M -m airj.cli build --project-dir examples/HTW --jar /tmp/hunt-the-wumpus.jar example/hunt_the_wumpus
```

## Play

```bash
java -jar /tmp/hunt-the-wumpus.jar
```

Commands:

- `move <room>`
- `shoot <room>`
- `quit`

You can only move or shoot into an adjacent room.

## Scripted Smoke Test

This reproduces a known winning path in the example's default layout:

```bash
printf 'move 2\nmove 10\nshoot 11\n' | java -jar /tmp/hunt-the-wumpus.jar
```

It should end with:

```text
Your arrow strikes true. You win!
```

## Run The AIR-J Unit Tests

Build the AIR-J test jar:

```bash
clj -M -m airj.cli build --project-dir examples/HTW --jar /tmp/hunt-the-wumpus-tests.jar example/hunt_the_wumpus_tests_root
```

Run it:

```bash
java -jar /tmp/hunt-the-wumpus-tests.jar
```

The runner is AIR-J code, not Clojure. It prints one line per test plus a summary and exits with:

- `0` when all tests pass
- `1` when any test fails or errors

## Run The AIR-J Unit Tests As JSON

Build the JSON test jar:

```bash
clj -M -m airj.cli build --project-dir examples/HTW --jar /tmp/hunt-the-wumpus-tests-json.jar example/hunt_the_wumpus_tests_json
```

Run it:

```bash
java -jar /tmp/hunt-the-wumpus-tests-json.jar
```

The JSON artifact is the canonical machine-readable AIR-J test result shape:

- `module`
- `passed`
- `failed`
- `errored`
- `outcomes`
