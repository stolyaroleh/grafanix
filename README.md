# Grafanix

Visualize build and runtime dependencies of your Nix derivations!

## Building

```bash
nix-build --attr grafanix --option build-use-sandbox false
```

>Note: we disable Nix sandboxing to make it possible for Bower to fetch frontend dependencies.

## Running

Grafanix expects a config file `config.dhall` in the directory you start it from.
You can start it directly from the Nix store:

```bash
cd ./result
./grafanix  # will use <nixpkgs>
```

..or from a different location with a modified config:

```bash
cp ./result/config.dhall .
vim config.dhall  # edit default configuration
./result/grafanix
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

cabal new-build         # Build it
cabal new-repl          # Start a REPL
cabal new-run grafanix  # Run it
```

### Frontend

```bash
cd frontend

bower install
pulp build -O --to ../static/main.js  # Build it
```
