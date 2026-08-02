# Lamplight

Lamplight is a browser-based graph visualizer for F# codebases. The CLI reads a
project directory, analyzes its `.fs` and `.fsx` files, serves the generated
analysis together with the graph UI, and opens the local browser URL.

The graph has two layers:

- **Files:** source files are nodes; dashed teal edges show inferred file references.
- **Functions:** double-click a file to descend into a project-wide function graph; solid violet edges show inferred function calls.

The analyzer is intentionally a version-0 proof of concept. It recognizes
top-level `let`, `let rec`, `and`, and member declarations, then uses module,
`open`, qualified-name, and token references to infer edges. It does not use the
F# compiler service, so overloaded names, shadowing, comments, string literals,
local/nested declarations, and ambiguous symbols can produce approximate
results. It is intended for exploration rather than compiler-accurate
architecture analysis.

## Getting Started

### Prerequisites

- [.NET 7 SDK](https://dotnet.microsoft.com/download/dotnet/7.0) (Fable 4.24.0 is pinned to the .NET 7 SDK via `global.json`).
- [Node.js](https://nodejs.org/) with `npm`.

### Install dependencies

```bash
npm install
```

This restores the local .NET tools (Fable and Femto) defined in
`.config/dotnet-tools.json`.

## Run the visualizer

Point the CLI at the F# project or source directory you want to inspect:

```bash
npm run run -- /path/to/your/fsharp-project
```

For example, to inspect this checkout:

```bash
npm run run -- .
```

The command builds the browser bundle, recursively reads `.fs` and `.fsx`
files, excludes `obj`, `bin`, `node_modules`, and `fable_modules`, then starts
a local server. It prints the analyzed file/function counts and serves the UI
at `http://localhost:8080` (or the next available port through `8090`). It
also attempts to open that URL with `xdg-open`; if no desktop browser is
available, copy the printed URL into one manually. Stop the server with
`Ctrl+C`.

No browser file or folder picker is required: the CLI argument is the source
of the project path.

## Using the graph

1. Drag nodes to adjust the force-directed layout. Use box selection or
   shift/ctrl-click for multiple nodes, and click a node or edge to inspect it.
2. Double-click a file node to enter the function layer. Use **Back** or
   **Escape** to return to the file layer.
3. Use **Pause layout** when you want to inspect or arrange a settled graph.

## Build and validation

To build only the static browser bundle:

```bash
npm run build
```

The CLI project can be built independently with:

```bash
dotnet build cli/Lamplight.Cli.fsproj
```

The static bundle is written to `public/bundle.js`; the normal workflow is to
run it through the CLI so that `analysis.json` is available from the same
origin. `npm start` remains available for Fable/Webpack development, but it
does not provide CLI analysis by itself.
