## Todo:

- [ ] fix rect/circle proportional mode (it's broken when the mouse pos < origin)
- [ ] add a cycle-zoom button
- [ ] change the debug key

- [ ] add speed functionality
- [ ] add a rounded mode to brush tool
- [ ] could the fill tool be made to not recompute every frame? (maybe give tools control over 'tl.clear')

- [ ] figure out Hashlife (eventually)

## Bugs:
- filled ellipses are completely screwed because of the gaps (or maybe also because of the choice in fill algo - does it go through diagonal pixels?)
- if a rect being drawn in proportional mode touches screen edges, it moves the excess size to the other side
- mouse cursor cell shouldn't be drawn when using pattern tool? (or it should change color)
- ellipses have gaps, especially when they're too narrow
- cannot draw at coords 0,0 with zoom 8 (1px cells)
- why do I keep losing ~10 fps in 1px cells, seemingly randomly (when most cells are dead?!), or after clearing the grid while paused, or when mouse is over the UI? Stopping/randomizing and pressing play seems to bring fps back up, at least for a little bit.


## Considering:
- find a way to compress patterns more?
  - problem: needs a parser and too many more tokens?
- show rect/circle sizes in info bar while drawing
- show pattern names in info bar while holding one
- cell color option?
- add way to preserve existing cells while changing zoom
  - solution 1: add a camera and scrolling, and use the same 240*136 grid of cells for all zoom levels
    - problem: this will drop performance under 60fps for all zooms
  - solution 2: add way to preserve existing cells when changing zoom (zooming out adds empty space, zooming in removes excess cells). But to what advantage?




