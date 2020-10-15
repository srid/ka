# ka

`ka` is in active research phase, and is highly volatile. But, if you are curious, you may run it on your notebook (such as [neuron] notes); `ka` works with a folder of Markdown files linked to one another (wiki-links are supported).

Project goals:

1. Fast build system
2. Extensible via plugins
3. UX optimized for navigating private notebooks

The first two goals in particular are relevant for preparing `ka` to eventually supplant rib and shake in neuron.

To run `ka` as a GTK+ app on your notebook:

```bash
# Setup reflex-frp nix cache first: 
# https://github.com/obsidiansystems/obelisk#installing-obelisk
$(nix-build)/bin/ka /your/notes/dir
```

## Development

This spins up a local server, rather than a GTK+ app (as the latter cannot be reloaded by ghcid):

```
# TODO: WIP
bin/run /your/notes/dir
```

## Deployment as web app

Set the `useWarp` flag in `reflex-dom` if you want to build it as a web app, instead of as GTK+ app.

TODO: WIP

[neuron]: https://github.com/srid/neuron