# AIR-J Thermostat

This example is a contract-heavy AIR-J safety controller.

It demonstrates:

- invariants on bounded controller state
- preconditions on target updates and control steps
- postconditions that preserve exact state transitions
- AIR-J tests through explicit text and JSON test roots

## Files

- `thermostat.airj`: bounded controller logic
- `thermostat_test_suite.airj`: reusable AIR-J test suite
- `thermostat_tests_root.airj`: canonical text test root
- `thermostat_tests_json.airj`: canonical JSON test root

## Build The Text Test Jar

```bash
clj -M -m airj.cli build --project-dir examples/Thermostat --jar /tmp/thermostat-tests.jar example/thermostat_tests_root
```

## Run The Text Tests

```bash
java -jar /tmp/thermostat-tests.jar
```

## Build The JSON Test Jar

```bash
clj -M -m airj.cli build --project-dir examples/Thermostat --jar /tmp/thermostat-tests-json.jar example/thermostat_tests_json
```

## Run The JSON Tests

```bash
java -jar /tmp/thermostat-tests-json.jar
```

The suite covers both success paths and contract failures:

- creating a bounded controller
- updating targets within range
- heating and cooling one step at a time
- detecting stable and heating modes
- rejecting out-of-range targets
- rejecting control steps that violate preconditions
