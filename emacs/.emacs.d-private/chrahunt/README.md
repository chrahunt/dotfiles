# Personal spacemacs layer

For more information see [LAYERS](https://develop.spacemacs.org/doc/LAYERS.html).

Separate local packages are for things that seem solid and mostly
generic. My personal layer is for configuring packages, where that
configuration is unlikely to change. `.spacemacs` is for configuration
I'm still figuring out.

## Testing

### Internal link handling

```
Feature: Internal link navigation stays in indirect buffers

  Scenario Outline: indirect buffer

    Given GUI emacs with an open buffer (A) visiting some file
      And an indirect buffer (B) created with `<command>` (`<binding>`)
      And the cursor is on an id-based link for a node in the same file
    When return is pressed
    Then the link is navigated in B

    Examples:
      | command                           | binding   |
      | clone-indirect-buffer             | SPC b N i |
      | clone-indirect-buffer-other-frame | SPC b N F |

  Scenario: Capture templates aren't impacted

    Not ideal, but better than navigating in the capture template buffer,
    I think.

    Given GUI emacs with an open buffer (A) visiting some file
      And an indirect buffer (B) created with `clone-indirect-buffer`
      And the cursor is in some node
      And an org capture template that captures a link is invoked (SPC C c t)
      And a new buffer (C) has popped up to complete the capture
      And the id-based link to the current node has been captured
      And the cursor has been moved in the capture buffer to be on the link
     When return is pressed
     Then the link is navigated in A

Feature: Internal link navigation in another window

  Given GUI emacs with an open buffer (A) visiting some file
    And the cursor is on an id-based link for a node in the same file
  When S-return is pressed
  Then a new indirect buffer (B) is created
   And the link is navigated in B
   And a new frame is created containing 1 window viewing B
```

Missing:

* S-RET in terminal (though it should work)
* Mouse link interaction (not used)
