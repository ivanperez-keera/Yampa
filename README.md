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

There is a directory with examples, which includes basic SDL examples and
and one example with a Wiimote controller. You can install them by default
with 

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
  
A more comprehensive list can be obtained using the reverse dependency finder
(http://packdeps.haskellers.com/reverse/Yampa), but only programs uploaded to
hackage are listed.

## Use in production

* Keera Studios is using it for several Haskell games for Android and iOS
  including a game currently available on iTunes and Google Play. See:
  * http://facebook.com/keerastudios
  * https://itunes.apple.com/us/app/magic-cookies/id1244709871
  * https://play.google.com/store/apps/details?id=uk.co.keera.games.magiccookies

## Backends

Yampa is backend agnostic, you can ultimately connect it to any backend you
want. Existing backends include:
* SDL
* OpenGL / GLUT
* WX (see wxHaskell)

## Documentation and tutorials

The distribution of Yampa comes with subtantial haddock documentation, which you can
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

For a list of Yampa-related papers, see:

* http://www.cs.nott.ac.uk/~nhn/papers.html

See also PhD technical report, chapter 3. http://www.cs.nott.ac.uk/~ixp/
which includes a review of FRP and outlines some existing issues.

## Help and collaboration

You can collaborate at least in three ways:

* File an issue (https://github.com/ivanperez-keera/Yampa/issues).
* Write documentation (send a link and/or a pull request).
* Research: we are constantly trying to improve Yampa. We'd be glad to have
  collaborators.  If you are working on this, please, let us know.

  (_Interactivity and FRP is the main topic of my (ongoing) PhD studies, so I'll
  keep working on this for some time._ -- Ivan Perez)

## Authors

* Henrik Nilsson
* Antony Courtney

### Maintainer

* Ivan Perez
