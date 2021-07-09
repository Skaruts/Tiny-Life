# Tiny Life
A toy Game Of Life for the TIC-80

I had a bunch of fun doing this, and also learned quite a lot. It became quite more than I initially intended, and took way more code than I expected.

The code is a bit cryptic because I kept hitting on the 64k token limit, and also having performance problems. So I had to shorten things dramatically and also resort to optimizations that make the code harder to read. There's also some indentation shenanigans going on in there that one may find strange. This is because my brain (and my mouse wheel) explodes when editing too much code in one file, so I had to find a way to stay organized. Separating the code into indented sections allows me to fold the code and make it more manageable.

#### Since TIC version 0.80 is slower than 0.70, I'm still developing this in 0.70, with its token limit. The issue has been fixed in 0.90, so when it comes out I'll try to make the code more readable.


## Changelog (didn't keep track of it until 1.3)

### version 1.3
 - improved performance (with border swapping), to a mostly stable 60 fps in 0.70, and about 45-50 in 0.80
 - overhauled/streamlined the UI (this gave me a lot more token budget)
 - added a rounded mode to brush tool
 - fixed brush drawing gaps (you can now draw continuous strokes)
 - added copy/cut/paste from/to the board
 - patterns and clipboard can now be rotated (Q/E) and flipped (A/Z)
 - rect/circle sizes are shown while drawing
 - the current state of the board can be saved and loaded. Reseting (pressing stop button) also loads the saved state.
 - backgrond and cell colors can be changed in the options (and are persistent in the non-web version)
 - added speed controls (Up/Down arrows, or Shift+Up/Down arrows)
 - improved the drawing tools (all tools now draw with LMB and erase with RMB)

 - fixed annoying gaps that plagued flat ellipses
 - fixed problems that plagued rect/circle proportional/centered modes
 - removed dead cells stuff, as they were mostly useless and redundant
