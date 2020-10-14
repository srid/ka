# ka

`ka` is in research phase, and is highly volatile. But, if you are curious, you may run it on your notebook (such as [neuron] notes); `ka` works with a folder of Markdown files linked to one another (wiki-links are supported).

Project goals:

1. Fast build system
2. Extensible via plugins
3. UX optimized for navigating private notebooks

The first two goals in particular are relevant for preparing `ka` to eventually supplant rib and shake in neuron.

To run `ka` locally on your notebook:

```bash
# Setup reflex-frp nix cache first: 
# https://github.com/obsidiansystems/obelisk#installing-obelisk
bin/run /your/notes/dir
```

[neuron]: https://github.com/srid/neuron