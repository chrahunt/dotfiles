# org-download-z

Add an `org-download-z-paste` for pasting directly
from the clipboard and include some helpers for WSL.

## details

This covers 2 use cases for image management in emacs:

1. Initiate a screenshot from within emacs itself to be inserted at the
   current location.
2. Insert an image into emacs that is already in the clipboard.

There are 2 contexts I need these use cases to work:

1. emacs in WSL under Windows 10
2. emacs natively on Ubuntu Linux with KDE

By default, the Spacemacs `org` layer includes
[org-download](https://github.com/abo-abo/org-download), which has a few
helpers:

* `org-download-screenshot` (, i D s) - initiates a screen capture utility,
  attaches the resulting image, and inserts a link in the document
* `org-download-yank` (, i D y) - grabs a URL, attaches the resulting image,
  and inserts a link in the document

`org-download-screenshot` covers the first use case, and is customizable so we
can also use it for the second.

## testing

```
Given emacs running in GUI mode under WSL opened to an org file
When `org-download-screenshot` is invoked
Then SnippingTool should be activated
And after selecting a portion of the screen it should be inserted as an
  image at the cursor

Given emacs running in GUI mode under WSL opened to an org file
And an image has been copied to the clipboard
When `org-download-z-paste` is invoked
Then the image from the clipboard should be inserted as an image at the cursor

Given emacs running in GUI mode on Linux
And an image has been copied to the clipboard
When `org-download-z-paste` is invoked
Then the image from the clipboard should be inserted as an image at the cursor

Given emacs running in GUI mode on Linux
And a copy was done from emacs (emacs holds the X clipboard)
When `org-download-z-paste` is invoked
Then nothing should be inserted
And emacs should not freeze
```
