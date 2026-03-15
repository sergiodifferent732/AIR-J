# AIR-J Tool Workflow

This example is a non-game AIR-J project that demonstrates:

- canonical file/process/json boundaries
- AIR-J unit tests with explicit text and JSON test roots
- AIR-J consumption of the canonical JSON test artifact

## Files

- `tool_logic.airj`: pure logic over canonical `Interchange`
- `tool_workflow.airj`: reads JSON, runs `/bin/cat`, transforms the payload, writes JSON
- `tool_workflow_test_suite.airj`: reusable AIR-J test suite logic
- `tool_workflow_tests_root.airj`: canonical AIR-J text test root
- `tool_workflow_tests_json.airj`: canonical AIR-J JSON test root
- `tool_workflow_test_report.airj`: AIR-J consumer for the JSON test artifact

## Build And Run The Workflow

Build the runnable jar:

```bash
clj -M -m airj.cli build --project-dir examples/ToolWorkflow --jar /tmp/tool-workflow.jar example/tool_workflow
```

Run it:

```bash
printf '{"tool":"wumpus"}' > /tmp/tool-input.json
java -jar /tmp/tool-workflow.jar /tmp/tool-input.json /tmp/tool-output.json
cat /tmp/tool-output.json
```

Expected output file:

```json
{"tool":"wumpus-checked","status":"ok"}
```

## Run The AIR-J Unit Tests

Build the text test jar:

```bash
clj -M -m airj.cli build --project-dir examples/ToolWorkflow --jar /tmp/tool-workflow-tests.jar example/tool_workflow_tests_root
```

Run it:

```bash
java -jar /tmp/tool-workflow-tests.jar
```

## Run The AIR-J Unit Tests As JSON

Build the JSON test jar:

```bash
clj -M -m airj.cli build --project-dir examples/ToolWorkflow --jar /tmp/tool-workflow-tests-json.jar example/tool_workflow_tests_json
```

Run it:

```bash
java -jar /tmp/tool-workflow-tests-json.jar
```

## Consume The JSON Test Artifact In AIR-J

Capture the JSON test output:

```bash
java -jar /tmp/tool-workflow-tests-json.jar > /tmp/tool-workflow-tests.json
```

Build the AIR-J report jar:

```bash
clj -M -m airj.cli build --project-dir examples/ToolWorkflow --jar /tmp/tool-workflow-test-report.jar example/tool_workflow_test_report
```

Run it:

```bash
java -jar /tmp/tool-workflow-test-report.jar /tmp/tool-workflow-tests.json
```

It prints the `passed` count from the canonical AIR-J test JSON artifact.
