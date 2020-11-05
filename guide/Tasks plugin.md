The tasks plugin creates a (virtual) note named `+Tasks`, and displays a list of all tasks *accumulated* across all notes in your notebook.

- [x] MVP of tasks plugin
- [x] Hide notes with only completed tasks
- [ ] Improve it enough to be useable by myself

## Brainstorming

- **Inline tags**:
  - For context
  - For dates/times? (eg: `#ctx/2020/01/23`)
  - For place? (eg: `#ctx/loc/cafe`)
  - Inline tags itself could be a plugin; then pass the inline tag plugin's *output* to the tasks plugin.
    - So Tasks plugin can get the output of inline tag plugin
    - Thinking about plugins depending on one another.
- **Kanban board**
  - For self-improvement from Artyom: https://freedom.brick.do/71637a02-84ba-45ce-92e6-e63c4b8f6f9e
  - Merge "contexts" with board "columns"?
- Use natura language to configure board