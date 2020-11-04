# ka

`ka` is in active research phase, and is highly volatile. `ka` works with a folder of Markdown files linked to one another (wiki-links are supported). If you are curious, begin by running `ka` on the ./guide notebook, before trying it out on your own notebook.

Project goals:

1. Fast build system
2. Extensible via plugins
3. UX optimized for navigating private notebooks

The first two goals in particular are relevant for preparing `ka` to eventually supplant rib and shake in neuron.

See also https://ka.srid.ca/ which corresponds to the notes in ./guide folder.

## Development

This spins up a local server, rather than a GTK+ app (as the latter cannot be reloaded by ghcid):

```bash
bin/run ./guide
```

## Running as GTK+ app

```bash
$(nix-build)/bin/ka ./guide  # Or pass your notebook directory
```

To install the GTK+ app, run `nix-env -if .` from project root. Then run `ka /path/to/your/notebook` to launch `ka` on your notebook.

(If the GTK+ app fails to launch for any reason, try the following section.)

## Running as web app

```bash
JSADDLE_WARP_PORT=8080 $(nix-build --arg useWarp true)/bin/ka ./guide
```