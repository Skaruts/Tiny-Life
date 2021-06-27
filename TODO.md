## Todo:

- [ ] replace TICuare for TICkle IMGUI, to save on character limits
- [ ] add a finite state machine to the loop
- [ ] remove draw_dead_cells (it's redundant) in favor of just having a bg color
- [ ] fix rect/circle proportional mode (it's broken when the mouse pos < origin)
- [x] add a cycle-zoom button
- [x] make zoom persistent
- [x] change the debug key (changed to `~` (or `\` in some keyboards I guess))
- [ ] add speed functionality
- [ ] add a rounded mode to brush tool
- [ ] could the fill tool be made to not recompute every frame? (maybe give tools control over 'tl.clear')

- [ ] figure out Hashlife (eventually)

## Bugs:
- if a rect being drawn in proportional mode touches screen edges, it moves the excess size to the other side
- mouse cursor cell shouldn't be drawn when using pattern tool? (or it should change color)
- ellipses have gaps, especially when they're too narrow
  - filled ellipses are screwed because of those gaps
- cannot draw at coords 0,0 with zoom 8 (1px cells)
- why do I keep losing \~10 fps in 1px cells, seemingly randomly (when most cells are dead?!), or after clearing the grid while paused, or when mouse is over the UI? Stopping/randomizing and pressing play seems to bring fps back up, at least for a little bit.


## Considering:
- find a way to compress patterns more?
  - problem: needs a parser and too many more tokens?
- show rect/circle sizes in info bar while drawing
- show pattern names in info bar while holding one
- cell color option?
- add way to preserve existing cells while changing zoom
  - solution 1: add a camera and scrolling, and use the same 240x136 grid of cells for all zoom levels
    - problem: this will drop performance under 60fps for all zooms
  - solution 2: add way to preserve existing cells when changing zoom (zooming out adds empty space, zooming in removes excess cells). But to what advantage?





