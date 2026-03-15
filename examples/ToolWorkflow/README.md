# AIR-J Tool Workflow

This example describes the AIR-J tool-workflow proof that already exists in the test suite.

It does four things:
- reads JSON from a file
- runs a subprocess
- parses the subprocess output back into canonical AIR-J interchange
- writes JSON back to a file

The concrete end-to-end proof currently lives in:
- [`spec/airj/compiler_spec.clj`](/Users/unclebob/projects/AIR-J/spec/airj/compiler_spec.clj)

The relevant example:
- reads JSON from disk
- runs `/bin/cat`
- decodes the subprocess output from `Bytes`
- parses it as canonical `Interchange`
- writes JSON back out

That proof is currently exercised through the compiler spec rather than kept as a standalone checked-in program file.

## What It Demonstrates

- canonical `Result`-based file, JSON, and process boundaries
- canonical `Bytes` conversion through `airj/bytes`
- an end-to-end agent-style workflow already proven by the test suite
