# Introduction #

PianoKeys is a keymap generator for [VMPK](https://sourceforge.net/projects/vmpk) (Virtual MIDI Piano Keyboard). It allows you to write the keymap in a much simpler syntax. You simply write:
```
LShift~RShift B0
CapsLock~Enter B1
Tab~\ B2
`~BackSpace B3
```

Yes, it's really that simple! Run `pianokeys`, and choose the generated XML file as raw keyboard map in VMPK preferences window, then you are done! Now you can use the four rows of your computer keyboard as piano white keys, starting from four octaves, to play your favorite song!
# Additional notes #
PianoKeys supports only raw keyboard maps, which use keycodes and therefore independent of the actual keyboard layout used(although the keymap is specified in QWERTY layout, any layout can be used when playing). One downside is that the keycodes may vary between operating systems.
Keycodes are defined in plain text files, whose syntax are as simple. By default, only keycode file for Linux is provided.

There are also keyboard definition files, which specify which keys are next to each other. This is useful because the exact arrangement of keys(most notably the "\\" key, and cursor control keys in standard vs. compact keyboards) are not the same across different keyboards.

It's also possible to specify black keys using the same syntax as white keys:

`CapsLock~Enter A#1`

This is most useful when the row on the bottom is white keys. Not all whole notes have corresponding semi notes, and their keyboard keys will be automatically omitted.

# Why Haskell? #

In case you didn't know, Haskell is an advanced purely [funtional](https://en.wikipedia.org/wiki/Functional_programming) programming language. In contrast to traditional imperative languages, it operates on first-class(can be passed as normal data) functions that are merely
transformations from their arguments to their return value and do not modify existing data.

While I am not very familar with Haskell, I'm here to learn. Besides, I think the nature of this task makes Haskell a good fit, especically thanks to its parser combinators and list processing capabilities.
