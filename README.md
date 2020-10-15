# ka

`ka` is in active research phase, and is highly volatile. But, if you are curious, you may run it on your notebook (such as [neuron] notes); `ka` works with a folder of Markdown files linked to one another (wiki-links are supported).

Project goals:

1. Fast build system
2. Extensible via plugins
3. UX optimized for navigating private notebooks

The first two goals in particular are relevant for preparing `ka` to eventually supplant rib and shake in neuron.

To run `ka` as a GTK+ app on your notebook:

## Development

This spins up a local server, rather than a GTK+ app (as the latter cannot be reloaded by ghcid):

```bash
# Setup reflex-frp nix cache first: 
# https://github.com/obsidiansystems/obelisk#installing-obelisk
bin/run /your/notes/dir
```

## Running as GTK+ app

```bash
# Run this from your notes directory
$(nix-build)/bin/ka
```

## Running as web app

```bash
nix-build --arg useWarp true
JSADDLE_WARP_PORT=8080 ./result/bin/ka /your/notes/dir
```

[neuron]: https://github.com/srid/neuron