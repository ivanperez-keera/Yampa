# Yampa

[![Build Status](https://travis-ci.org/ivanperez-keera/Yampa.svg?branch=master)](https://travis-ci.org/ivanperez-keera/Yampa)
[![Version on Hackage](https://img.shields.io/hackage/v/Yampa.svg)](https://hackage.haskell.org/package/Yampa)
[![Flattr this](http://api.flattr.com/button/flattr-badge-large.png "Flattr This!")](https://flattr.com/submit/auto?user_id=ivanperez-keera&url=https://github.com/ivanperez-keera/Yampa&title=Yampa&language=&tags=github&category=software)

Domain-specific language embedded in Haskell for programming hybrid (mixed
discrete-time and continuous-time) systems. Yampa is based on the concepts of
Functional Reactive Programming (FRP) and is structured using arrow
combinators.

## Installation

Yampa is available on hackage: http://hackage.haskell.org/package/Yampa.

```
$ cabal sandbox init         # Optional, but recommended
$ cabal update
$ cabal install Yampa
```

## Examples

There is a directory with examples, which includes two basic SDL examples and
one with using a Nintendo Wii Remote. You can install them with:

```
$ cabal sandbox init         # Optional, but recommended
$ cabal update
$ cabal install Yampa -fexamples
```

### Other examples

There are many programs written in Yampa. See the following examples:

* [Haskanoid](https://github.com/ivanperez-keera/haskanoid): a game that uses
  SDL multimedia, wiimote and kinect. It's cross platform and works in desktop,
  mobile, and [web](http://ivanperez-keera.github.io/haskanoid/haskanoid.jsexe/index.html)
  (compiled with [GHCJS](https://github.com/ghcjs/ghcjs)).
* [Space invaders](https://hackage.haskell.org/package/SpaceInvaders).
* [Frag](https://hackage.haskell.org/package/frag): a 3D first person shooting game.
* [Yampa-2048](https://github.com/ksaveljev/yampa-2048): an implementation of
  the game 2048 using Yampa and Gloss.
* [MandelbrotYampa](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/MandelbrotYampa):
  a "hello world" using SDL2, Yampa and OpenGL.
* [Haskelloids](https://github.com/keera-studios/Haskelloids): a reproduction of the Atari 1979 classic "Asteroids"

A more comprehensive list can be obtained using the reverse dependency finder
(http://packdeps.haskellers.com/reverse/Yampa), but only programs uploaded to
hackage are listed.

| <img src="https://raw.githubusercontent.com/ivanperez-keera/haskanoid/master/screenshots/android.gif?raw=true" width="200" alt="Haskanoid Video" style="max-width:200px;"> | <img src="https://raw.githubusercontent.com/ivanperez-keera/SpaceInvaders/develop/screenshots/gameplay.gif?raw=true" width="138" alt="Space Invaders" style="max-width: 138px;"> | <img src="http://ksaveljev.github.io/2048.gif" width="110" alt="Space Invaders" style="max-width: 110px;"> |
|-------------------------------------------|---------------|-------------------------|
| Haskanoid, SDL cross-platform arkanoid. | SpaceInvaders, GLUT arcade game | Yampa2048, a gloss board game |

## Use in production

* Keera Studios is using it for several Haskell games for Android and iOS
  including a game currently available on [iTunes](https://itunes.apple.com/us/app/magic-cookies/id1244709871) and [Google Play](https://play.google.com/store/apps/details?id=uk.co.keera.games.magiccookies). See:
  * http://facebook.com/keerastudios
  * https://itunes.apple.com/us/app/magic-cookies/id1244709871
  * https://play.google.com/store/apps/details?id=uk.co.keera.games.magiccookies

## Backends

Yampa is backend agnostic, you can ultimately connect it to any backend you
want. Existing backends include:
* SDL
* SDL2
* OpenGL / GLUT
* WX (see wxHaskell)
* HTML Canvas

## Testing

Yampa comes with a sophisticated testing library that allows you to use
QuickCheck to test your games, and use a time-travel debugger. These features
are described in the paper [Testing and Debugging Functional Reactive
Programming](http://dl.acm.org/authorize?N46564).

You can find the additional projects at:
* [yampa-test](https://github.com/ivanperez-keera/Yampa/tree/develop/extensions/testing)
* [Haskell Titan](https://github.com/keera-studios/haskell-titan)

## Documentation and tutorials

The distribution of Yampa comes with substantial haddock documentation, which you can
build using haddock or just [read online](https://hackage.haskell.org/package/Yampa).
To build a local copy, do:

```
$ cabal unpack Yampa ## Or git clone this-repo
$ cd Yampa-*
$ cabal init
$ cabal install --only-dependencies
$ cabal configure && cabal haddock --internal
```

Documentation is also available online: https://wiki.haskell.org/Yampa

## Papers and technical reports

* [Extensible and Robust Functional Reactive Programming](http://www.cs.nott.ac.uk/~psxip1/papers/2017-Perez-thesis-latest.pdf) (Ivan Perez; 2017)
* [Testing and Debugging Functional Reactive Programming](http://dl.acm.org/authorize?N46564) (Ivan Perez and Henrik Nilsson; 2017)
* [Functional Reactive Programming, Refactored](http://dl.acm.org/authorize?N34896) (Ivan Perez, Manuel Bärenz, and Henrik Nilsson; 2016)
* [Safe Functional Reactive Programming through Dependent Types](http://dl.acm.org/authorize?N08595) (Neil Sculthorpe and Henrik Nilsson; 2009)
* [Push-Pull Functional Reactive Programming](http://conal.net/papers/push-pull-frp/push-pull-frp.pdf) (Conal Elliott; 2009)
* [Switched-on Yampa: Declarative Programming of Modular Synthesizers](http://www.cs.nott.ac.uk/~psznhn/Publications/padl2008.pdf) (George Giorgidze and Henrik Nilsson; 2008)
* [Demo-outline: Switched-on Yampa: Programming Modular Synthesizers in Haskell](http://dl.acm.org/authorize?N08596) (George Giorgidze and Henrik Nilsson; 2007)
* [Dynamic Optimization for Functional Reactive Programming using Generalized Algebraic Data Types](http://dl.acm.org/authorize?N08598) (Henrik Nilsson; 2005)
* [The Yampa Arcade](http://dl.acm.org/authorize?N08599) (Antony Courtney, Henrik Nilsson, and John Peterson; 2003)
* [Arrows, Robots, and Functional Reactive Programming](http://www.cs.nott.ac.uk/~psznhn/Publications/afp2002.pdf) (Paul Hudak, Antony Courtney, Henrik Nilsson, and John Peterson; 2002)
* [Functional Reactive Programming, Continued](http://dl.acm.org/authorize?N08592) (Henrik Nilsson, Antony Courtney, and John Peterson; 2002)
* [Genuinely Functional User Interfaces](http://conal.net/papers/genuinely-functional-guis.pdf) (Antony Courtney and Conal Elliott; 2001)


* See also:
  * [Henrik Nilsson's publications](http://www.cs.nott.ac.uk/~psznhn/papers.html)
  * [Ivan Perez's publications ](http://www.cs.nott.ac.uk/~psxip1/)
  * [First Year PhD Report](http://www.cs.nott.ac.uk/~psxip1/papers/2014-Perez-1st-year-report.pdf) (Ivan Perez, 2014), chapter 3 includes a review of FRP and outlines some existing issues.

## Help and collaboration

You can collaborate at least in three ways:

* File an issue (https://github.com/ivanperez-keera/Yampa/issues).
* Write documentation (send a link and/or a pull request).
* Research: we are constantly trying to improve Yampa. We'd be glad to have
  collaborators.  If you are working on this, please, let us know.

  (_Interactivity and FRP is the main topic of my (ongoing) work and research,
   so I'll keep working on this for some time._ -- Ivan Perez)

## Authors

* Henrik Nilsson
* Antony Courtney

### Maintainer

* Ivan Perez
