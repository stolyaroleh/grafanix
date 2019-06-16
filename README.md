# Grafanix

Visualize build and runtime dependencies of your Nix derivations!

## Building

```bash
nix-build --attr grafanix
```

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
- Elm Support

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

./scripts/watch.sh # Rebuild on every change
```
