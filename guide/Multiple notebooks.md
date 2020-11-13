We want to be able to merge two distinct notebooks whilst also allowing *cross linking* between them using predictable *linking strategy*. This is a feature in roadmap.

[[Personal need]] for this feature: to be able to use this notebook in my private Zettelkasten, and be able to link to it in everyday contexts. As a meta observation, this feature is necessary to connect tasks on this project to [[Personal need]]s specified *only* in my private Zettelkasten.

## Implementation whiteboard

- Piggy back on the "scope" work done in [[Browse filesystem hierarchy]], and extend the Thing type to contain a `namespace` attribute, such that full path is a function of scope and namespace.
- Use the distinction between scope and namespace to derive the default linking strategy; specifically, if two notebooks have same note file, then to disambituate `[[Foo]]`, use the note from the same namespace as that note containing this link (if the same notebook has ambiguously named files that's still an error)
- In the UI, report these otherwise conflicting note files anyway, in case they were created by accident using the [[VSCode memo extension]] (its `Ctrl+Click`)

Dev tasks to implement the above:

- [ ] Refactor `_app_doc`'s Map value type to be an ADT.
- [ ] 