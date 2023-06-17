# frame-prefix

Easily set a prefix on the current frame title.

Uses:

- Helps differentiate frames when using alt-tab
- Helps differentiate frames when hovering over emacs taskbar icon
- Easier to find target frame with GUI window search utilities:
  - Linux (with KDE): krunner (https://askubuntu.com/a/835045/677357)
  - Windows: PowerToys run (https://learn.microsoft.com/en-us/windows/powertoys/run)

## Testing

```
Feature: Setting frame title prefix

  Scenario: In a single frame

    Example: When no existing prefix

    Example: With existing prefix

    Example: Sticks when switching buffers

  Scenario: With multiple frames

    Example: Only sets current frame title

  Scenario: Opening a new frame

    Example: New frame doesn't have existing prefix

Feature: Resetting frame title prefix

  Scenario: In a single frame

    Example: Removes existing prefix

    Example: No-op if no existing prefix

  Scenario: With multiple frames

    Example: Only removes current frame title prefix
```
