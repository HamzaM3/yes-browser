# Yes Browser

Yes, it's a browser

![Browser demo](https://github.com/HamzaM3/yes-browser/blob/b90fa67d74e96415c5f0e23f7537fad81dccc5da/img/smallDemo.gif)

## Purpose

This is an experimental browser to explore the challenges posed by browsers and what they should be constituted of.

Anyone is invited to contribute and add their own improvement. We can make it go towards + infintiy and it's just the start so you can do a lot of significant contributions.

## Install dependencies

- `sudo apt install build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 libftgl-dev libxi-dev libftgl-dev libxi-dev freeglut3-dev`
- `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`

## Fonts

I have acquired the `libftgl` if you want to make any font stuff. We can also make it go towards + infinity.

## Specifications

We have to read the specification and implement it properly.

It's not that hard to understand and not that hard to do in my opinion.

For example, the basic properties of CSS are actually very few: https://www.w3.org/TR/CSS2/propidx.html#q24.0

## Todo

_Directions:_ Generalization and Optimization

_Priority:_ implement some Javascript while the functionalities are small (if it's bigger it will be hard)

=> all the core components of the browser have to be implemented first. Then we'll generalize and optimize progressively all at the same time.

### `Display.hs`

- [ ] Replace `postRedisplay` with a more cheap update method
  - [ ] Improve update performance
- [ ] Think about how to implement interactivity
- [ ] Add the ability to specify own font
- [ ] Improve [FTGL](https://github.com/HamzaM3/ftgl) and [Haskell's wrapper of FTGL](https://github.com/HamzaM3/FTGL-haskell) by making it more efficient and better architectured
  - Check this [issue](https://github.com/frankheckenbach/ftgl/issues/10) that explains the defects
  - [ ] Fix the fact that you can't modify height of fonts and get its line height directly
    - How about getting that info without creating the whole font
- [ ] Minimize the creation of fonts and layouts
- [ ] Make the display dependent on the window size (make it fast though !)
- [ ] Fix the namespacing in the file (no points !)

### `BoxTree.hs`

- [ ] Create a default CSS (instead of the unique default style)
- [ ] Improve the data structures
  - Think about the incorporation of JS
  - Tags might be needed for JS manipulation
  - Deal with input and buttons (at some point)
- [ ] Make sure there is always a value for all properties
  - Remove as many `Maybe`s as possible
  - Deal with inheritable values and non-inheritable
- [ ] Improve text box height calculation
  - We have ftgl so modify it in order to do that
- [ ] Leaf => ShallowEmptyNode ?
- [ ] margin padding width height min-width min-height elementwise
  - how are they actually specified ?
- [ ] Add flex and grid
- [ ] Add relative units
- [ ] Add position and floats
- [ ] Improve the map from Selectors to ElementStyle
  - [ ] Add all selectors
- [ ] Create a giga function of type :: css file -> html file -> display tree

### `Parsers.ParserUtils.Parser.hs`

- [ ] Use better parsing methods
- [ ] Don't waste already parsed elements
- [ ] Add error messages
- [ ] Is there an AST evaluation framework

### `Parsers.ParserUtils.CheckParser.hs`

- [ ] Implement dependent parsing (i.e. parsing based on previous values)
  - I'm using Monads here (there must be better)

### `Parsers.ElementStyle.hs`

- [ ] Add more properties
- [ ] Deal with inheritance

### `Parsers.HTMLParser.hs`

- [ ] Add all tags
  - [ ] Tags with numbers (h1, h2...)
- [ ] Add attributes
  - [ ] Deal with ids
  - [ ] Deal with classes
  - [ ] Deal with head and body
- [ ] Implement forgiveness (not correct don't imply exception)
- [ ] Implement the whole spec

### `Parsers.CSSParser.hs`

- [ ] Implement selectors
- [ ] Add all properties
- [ ] Create default style sheet
- [ ] Implement data structure incorporating priority of selector
  - i.e. data structure has to be order lexicographically based on (spec priority, precedence)
- [ ] Implement forgiveness (not correct don't imply exception)
- [ ] Create value parsers and combine them
  - [ ] Create a map (property -> value types) and build parsers from that
- [ ] Last semi colon is OK
- [ ] Improve perf
- [ ] Deal with units

### `GlobalStates.GlobalStyle.hs`

- [ ] Is using a global variable OK ?
  - How about tabs ? Why not some context pattern ?

### `GlobalStates.GlobalScroll.hs`

- [ ] Is global variable for scroll state ok ?
  - why not context pattern ?
- [ ] What should the scroll speed ?
- [ ] Let's accept tablet ?
  - An Haskell mobile app ?

### Javascript

How to implement Javascript ? Let's [discuss](https://github.com/HamzaM3/yes-browser/tree/javascript) !
