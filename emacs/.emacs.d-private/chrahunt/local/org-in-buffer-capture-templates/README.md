# org-in-buffer-capture-templates

For keeping capture templates in the org files they are related to.

## Usage

Create a src block with language `org-capture-template` with a capture
template specification, like

```org
#+begin_src org-capture-template
'(
  "d"
  "Diary entry"
  entry
  (file+olp "" "Diary")
  "* %^{Title}
:PROPERTIES:
:CREATED: %U
:END:

%?
"
  ; Prepend to the date tree
  :prepend t
)
#+end_src
```

Evaluate the src block (`C-c C-c` or `org-babel-execute-src-block`) and it
will start a capture with this template, bypassing the org-capture dispatcher.

Execute the function `org-capture-from-buffer` to get an org-capture
dispatcher populated with the capture templates from the current buffer.
The list of templates is re-populated every time the function is executed.
