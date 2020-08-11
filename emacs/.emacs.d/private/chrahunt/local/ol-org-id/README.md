# ol-org-id

Cleans up behavior related to handling of org-id-based links in
org mode.

Specifically:

1. In indirect buffers where `org-id-open-links-in-same-buffer` is set,
   navigating links will use the current indirect buffer instead of
   reverting to the base buffer.

Callback `set-org-id-open-links-in-same-buffer` is available to register
for hooks.

## Testing

```
Feature: Navigating a link

  Scenario: Using a non-indirect buffer

    This should work as-expected, navigating to the location in the same
    buffer.

    Example: Non-indirect buffer

      Given GUI emacs with an open buffer (A) in org mode
        And the cursor is on any part of an internal (id-based) link
      When `org-open-at-point` is executed
      Then emacs navigates to the link destination in A

    Example: Non-indirect buffer with variable set

      Given GUI emacs with an open buffer (A) in org mode
        And the cursor is on any part of an internal (id-based) link
        And `org-id-open-links-in-same-buffer` is `t`
      When `org-open-at-point` is executed
      Then emacs navigates to the link destination in A

  Scenario: With an interactively-created indirect buffer

    Indirect buffers can be created for a number of reasons. By scoping our
    changes to indirect buffers created interactively it is easier to ensure
    that it doesn't conflict with navigating links in other contexts.

    Scenario Outline: Wrapped interactive indirect buffer commands

      Given GUI emacs with an open buffer (A) in org mode
        And an indirect buffer (B) is made with `<command>` interactively
        And the cursor is on any part of an internal (id-based) link
      When `org-open-at-point` is executed
      Then emacs navigates to the link destination in B

      Examples:
        | command                                            |
        | clone-indirect-buffer                              |
        | clone-indirect-buffer-other-frame                  |
        | clone-indirect-buffer-other-window                 |
        | clone-indirect-buffer-other-window-without-purpose |

  Scenario: With a non-interactively-created indirect buffer

    Other kinds of indirect buffers (like org-capture buffers) should not have
    the new behavior.

      Given GUI emacs with an open buffer (A) in org mode
        And an indirect buffer (B) is made with `org-capture`
        And the cursor is on any part of an (id-based) link
       When `org-open-at-point` is executed
       Then emacs navigates to the link destination in the original buffer
```
