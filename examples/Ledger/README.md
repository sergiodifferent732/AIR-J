# AIR-J Ledger

This example is a bounded AIR-J domain model built around a bank-style ledger.

It demonstrates:

- pure logic organized so stronger contracts can be added later
- AIR-J tests that prove balance-preservation and transfer behavior today
- AIR-J tests through explicit text and JSON test roots

## Files

- `ledger.airj`: bounded account and transfer logic
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

The suite focuses on the domain rules that a fuller contract system should
eventually express directly:

- opening a valid account at zero
- positive deposits and bounded withdrawals
- transfer preserving total funds
- transfer updating both accounts exactly
- stable balance labels for reporting

Current AIR-J note:

- richer arithmetic `requires`/`ensures` clauses for this example are not yet
  accepted by the implementation, so those rules are enforced here by AIR-J
  tests rather than by compiled contracts
