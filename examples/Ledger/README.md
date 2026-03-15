# AIR-J Ledger

This example is a contract-heavy AIR-J domain model built around a bounded
bank-style ledger.

It demonstrates:

- record invariants on account state
- preconditions on deposits, withdrawals, and transfers
- postconditions that preserve exact balance changes
- AIR-J tests through explicit text and JSON test roots

## Files

- `ledger.airj`: contract-heavy account and transfer logic
- `ledger_test_suite.airj`: reusable AIR-J test suite
- `ledger_tests_root.airj`: canonical text test root
- `ledger_tests_json.airj`: canonical JSON test root

## Build The Text Test Jar

```bash
clj -M -m airj.cli build --project-dir examples/Ledger --jar /tmp/ledger-tests.jar example/ledger_tests_root
```

## Run The Text Tests

```bash
java -jar /tmp/ledger-tests.jar
```

## Build The JSON Test Jar

```bash
clj -M -m airj.cli build --project-dir examples/Ledger --jar /tmp/ledger-tests-json.jar example/ledger_tests_json
```

## Run The JSON Tests

```bash
java -jar /tmp/ledger-tests-json.jar
```

The suite covers both contract-preserving success paths and explicit contract failures:

- opening a valid account at zero
- positive deposits and bounded withdrawals
- transfer preserving total funds
- transfer updating both accounts exactly
- stable balance labels for reporting
- rejecting empty account ids
- rejecting overdrafts
