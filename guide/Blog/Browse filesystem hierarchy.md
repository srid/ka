[[Blog]] post on 2020-11-11

After [[Introducing filesystem hierarchy support in Ka]], a natural question arose: why not enable users to browse their notes like they browse files in a file manager? To that end, in Ka you can now browse your notes in hierarchical fashion.

Broken down, [this change][pr] effectively introduces two features:

1. **Scope breadcrumbs**: displays the full path to the note when viewing the note, or directory
2. **Directory view**: Displays the contents of a directory

Ka launches with the directory view for `/` as the default route (see screeshot below). 

## Implementation

Each note gets an associated "scope". A scope of a note is simply the directory it resides in. Top-level notes have empty scope. 

There is potential for utilizing note scopes towards new features. For example, scopes may be used in designing configurable *linking strategies*. One particular use case I have in mind is support for [[Multiple notebooks]] and cross-linking between them while predictably dealing with duplicate notes across notebooks. It is very common for two notebooks to have notes with the same [[Daily notes]] filenames. Ka should continue to work with such predictable conflicts, using a "local first" linking strategy, wherein the daily note from the current notebook would be used.

Finally, note that - as [[Ka is reactive]], as you move files around using your native file manager, Ka's view will automatically update without manual refresh.

## Screenshot

Here's a screenshot of Ka's default route page on my private Zettelkasten, displaying the contents of the root scope.

{.ui .centered .large .bordered .image}
![](https://user-images.githubusercontent.com/3998/98863200-07a4b200-2436-11eb-9c37-109abe67f2ae.png)

[pr]: https://github.com/SridCircle/ka/pull/4