# Grafanix

Visualize build and runtime dependencies of your Nix derivations!

## Building

```bash
nix-build --attr grafanix --option build-use-sandbox false
```

>Note: we disable Nix sandboxing to make it possible for Bower to fetch frontend dependencies.

## Running

```bash
./result/bin/grafanix --help # show available options
./result/bin/grafanix        # start Grafanix, use <nixpkgs>
```

After starting Grafanix, open `localhost:3000` in your browser:

![](grafanix.png)

## Hacking

I suggest using VSCode with the following plugins:

- Haskell IDE Engine
- HTML CSS Support
- PureScript IDE

Make sure you run it in a shell with all necessary tooling:

```bash
nix-shell --command "code"
```

### Backend

```bash
cd backend

cabal new-build                                   # Build it
cabal new-repl                                    # Start a REPL
cabal new-run grafanix --static-path="../static"  # Run it, serve static assets from project directory
```

### Frontend

```bash
cd frontend

bower install                              # Fetch dependencies
pulp --watch build --to ../static/main.js  # Rebuild on every change
```
