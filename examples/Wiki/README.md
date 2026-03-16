# AIR-J Wiki

This example ports the core non-HTTP wiki behavior from the Java `wiki`
example under:

- `/Users/unclebob/Documents/Pearson Courses/AgenticSoftwareDev/examples/wiki`

It intentionally focuses on the pure acceptance slices that AIR-J can model
cleanly today:

- page CRUD and validation
- alphabetical page discovery
- text search
- revision history
- wiki links
- markdown-like body rendering

It now includes both:

- the pure in-memory wiki engine and AIR-J tests
- a real in-process AIR-J HTTP server

## Files

- `wiki.airj`: pure in-memory wiki engine
- `wiki_persistence.airj`: canonical JSON/file persistence for wiki state
- `wiki_app.airj`: runnable console wiki shell
- `wiki_server.airj`: runnable HTTP wiki server
- `wiki_test_suite.airj`: reusable AIR-J wiki acceptance-oriented tests
- `wiki_tests_root.airj`: canonical AIR-J text test root
- `wiki_tests_json.airj`: canonical AIR-J JSON test root

## Build The Console App Jar

```bash
clj -M -m airj.cli build --project-dir examples/Wiki --jar /tmp/wiki-app.jar example/wiki_app
```

## Run The Console App

```bash
java -jar /tmp/wiki-app.jar
```

## Build The HTTP Server Jar

```bash
clj -M -m airj.cli build --project-dir examples/Wiki --jar /tmp/wiki-server.jar example/wiki_server
```

## Run The HTTP Server

```bash
java -jar /tmp/wiki-server.jar
```

Default address:

- `http://127.0.0.1:8080`
- default state file: `/tmp/airj-wiki.json`

Optional port:

```bash
java -jar /tmp/wiki-server.jar 9090
```

Optional port and state file:

```bash
java -jar /tmp/wiki-server.jar 9090 /tmp/airj-wiki-state.json
```

Useful routes:

- `GET /`
- `GET /view/<title>`
- `POST /create`
- `POST /update`
- `POST /delete`
- `POST /search`
- `GET /api/wiki`
- `GET /api/pages`
- `GET /api/page/<title>`
- `GET /api/history/<title>`
- `GET /api/search/<query>`
- `POST /api/create`
- `POST /api/update`
- `POST /api/delete`

Quick smoke test:

```bash
curl -L http://127.0.0.1:8080/
```

Persistence smoke test:

```bash
curl -L -d 'title=Home&body=%23%20Persisted' http://127.0.0.1:8080/update
```

Then restart the server against the same state file and revisit `/view/Home`.

JSON API smoke test:

```bash
curl -L http://127.0.0.1:8080/api/wiki
curl -L -d 'title=Home&body=%23%20Updated' http://127.0.0.1:8080/api/update
curl -L http://127.0.0.1:8080/api/page/Home
```

Supported commands:

- `help`
- `list`
- `view <title>`
- `render <title>`
- `search <query>`
- `history <title>`
- `create <title>|<body>`
- `update <title>|<body>`
- `delete <title>`
- `quit`

Example session:

```bash
printf 'create Home|# Welcome\nrender Home\nlist\nquit\n' | java -jar /tmp/wiki-app.jar
```

## Build The Text Test Jar

```bash
clj -M -m airj.cli build --project-dir examples/Wiki --jar /tmp/wiki-tests.jar example/wiki_tests_root
```

## Run The Text Tests

```bash
java -jar /tmp/wiki-tests.jar
```

## Build The JSON Test Jar

```bash
clj -M -m airj.cli build --project-dir examples/Wiki --jar /tmp/wiki-tests-json.jar example/wiki_tests_json
```

## Run The JSON Tests

```bash
java -jar /tmp/wiki-tests-json.jar
```

The text and JSON roots both execute the same AIR-J suite. The JSON artifact
uses the canonical AIR-J test result shape:

- `module`
- `passed`
- `failed`
- `errored`
- `outcomes`

## Canonical Wiki JSON Schema

The persisted file and `GET /api/wiki` use the same canonical AIR-J shape:

```json
{
  "pages": [
    {
      "title": "Home",
      "body": "# Welcome",
      "revisions": [
        {"body": "# Welcome"}
      ]
    }
  ]
}
```

Additional JSON routes use these shapes:

- `GET /api/pages`
  - `{"titles":["Home","Install Guide"]}`
- `GET /api/page/<title>`
  - `{"title":"Home","body":"# Welcome","rendered":"<h1>Welcome</h1>","history":["# Welcome"]}`
- `GET /api/history/<title>`
  - `{"title":"Home","history":["# Welcome","# Updated"]}`
- `GET /api/search/<query>`
  - `{"query":"Guide","titles":["Install Guide"]}`
- error responses
  - `{"error":{"message":"...","detail":"..."}}`

## Acceptance Scope Mapped From The Java Wiki

The AIR-J suite covers the non-HTTP feature slices from the Java example:

- `page_crud.feature`
- `discovery.feature`
- `revision_history.feature`
- `wiki_links.feature`
- `wiki_markup.feature`

The intent is not a production web framework. The intent is a faithful AIR-J
application that proves the wiki domain behavior, canonical persistence, and a
real internal AIR-J page server.
