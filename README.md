# ka

See <https://ka.srid.ca/> for project intro and details.

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

(If the GTK+ app fails to launch for any reason, try the next section.)

## Running as web app

```bash
JSADDLE_WARP_PORT=8080 $(nix-build --arg useWarp true)/bin/ka ./guide
```

Note: Firefox is unsupported.