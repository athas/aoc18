# Advent of Code 2018

Some of the inputs have been changed to fit
[Futhark](https://futhark-lang.org) syntax, for convenience.  Others
have a preprocessing script (generally in Python) that must be used in
a pipeline before feeding Futhark.

## Shame

Futhark is a bit of an odd functional language in that it does not
support recursion.  Some tasks may be too painful to realistically
solve via iteration and stack management.  For these, I will
shamefully use another language, probably Haskell.
