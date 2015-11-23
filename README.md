# Yampa

[![Build Status](https://travis-ci.org/ivanperez-keera/Yampa.svg)](https://travis-ci.org/ivanperez-keera/Yampa)
[![Version on Hackage](https://img.shields.io/hackage/v/Yampa.svg)](https://hackage.haskell.org/package/Yampa)
[![Flattr this](http://api.flattr.com/button/flattr-badge-large.png "Flattr This!")](https://flattr.com/submit/auto?user_id=ivanperez-keera&url=https://github.com/ivanperez-keera/Yampa&title=Yampa&language=&tags=github&category=software)

Domain-specific language embedded in Haskell for programming hybrid (mixed
discrete-time and continuous-time) systems. Yampa is based on the concepts of
Functional Reactive Programming (FRP) and is structured using arrow
combinators.

:star2: **New version!**

Although the old API is mainly respected, the code has been reorganised to
facilitate understanding and documentation. The API is not fully backwards
compatible, but most projects should work without changes or with minimal
adaptations. If that is not the case, please let us know. Also, if you are FRP
savvy, please take a look around and tell us whether there's anything you'd
change. The documentation is available at:
http://hackage.haskell.org/package/Yampa/docs/FRP-Yampa.html

## Installation

Yampa is available on hackage: http://hackage.haskell.org/package/Yampa.

```
$ cabal sandbox init         # Optional, but recommended
$ cabal update
$ cabal install Yampa
```

## Examples

There are many programs written in Yampa. See the following examples:

* Haskanoid: a game that uses SDL multimedia, wiimote and kinect:
  * Github: https://github.com/ivanperez-keera/haskanoid
  * Hackage: http://hackage.haskell.org/package/haskanoid
  * Running on a browser, compiled with [GHCJS](https://github.com/ghcjs/ghcjs) (beware: bugs ahead):
  http://ivanperez-keera.github.io/haskanoid/haskanoid.jsexe/index.html

* Space invaders: a demonstration game used for a paper.
  https://hackage.haskell.org/package/SpaceInvaders
* Frag: a 3D first person shooting game.
  https://hackage.haskell.org/package/frag
* Yampa-2048: an implementation of the game 2048 using Yampa and Gloss.
  https://github.com/ksaveljev/yampa-2048

A more comprehensive list can be obtained using the reverse dependency finder
(http://packdeps.haskellers.com/reverse/Yampa), but only programs uploaded to
hackage are listed.

## Use in production

* Keera Studios is using it for several Haskell games for Android
  including a game currently available on Google Play. See:
  * http://facebook.com/keerastudios
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
  keep working on this for quite some time._ -- Ivan Perez)

## Authors

* Henrik Nilsson
* Antony Courtney

### Maintainer

* Ivan Perez
