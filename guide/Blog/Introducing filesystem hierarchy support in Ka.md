[[Blog]] post on 2020-10-18 

I had new requirement for Ka when working with my private notebook (which has plenty of daily notes): **archive old notes** whilst still being able to access and link to them.

My initial idea was a complicated one: write a specialized archive plugin. However, because I now strongly believe that software should be simple with a simple but functional [UX],[^simplicity] I decided to take a more general approach: if ka supported reading notes from the *whole directory tree*, then I can move note files to *any* sub-directory, and write a small plugin that will treat any pre-defined directory as containing archived notes (the UI can then hide these notes by default from sidebar listing).

:::{.ui .small .message}
:::{.header}
Interlude
:::
A trick I used here to overcome the tendency to design complex solutions by habit is to [[Think long-term when designing plugins]]. What would it look like to archive notes, whilst also marking their temporal position (say, spanning 3 years)?
:::

Such a general solution to this problem also enables other interesting use cases. For example, I can now put notes under different "buckets" -- for eg., work related notes can be put under the ./Work directory. Consequently, there is now an opportunity to improve the sidebar UI so as to present a tree view for navigating notes.

Of course, our notebook is still interlinked [Zettelkasten]-style. Notes from any directory can link to notes from any other directory. To this end, linking to ./Foo/Bar/SomeNote.md, for example, is done via `[[SomeNote]]`--i.e; a note is identified by its filename alone, regardless of its position in the directory hierarchy. This allows one to freely move notes around.

[UX]: https://opensource.com/life/15/3/user-experience-open-source-future
[Zettelkasten]: https://neuron.zettel.page/zettelkasten.html
[^simplicity]: Staying *simple* from the start, is a core tenet of the Ka project. See also: [[Markdown notes can be extended without compromising simplicity]].