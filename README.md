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

Getting Yampa to run is trivial. FRP is about values that change over time. In
Yampa, a system is defined by a signal function (SF), which determines how the
varying input and the varying output relate. For example:

```haskell
{-# LANGUAGE Arrows #-}
import FRP.Yampa

signalFunction :: SF Double Double
signalFunction = proc x -> do
  y <- integral -< x
  t <- time     -< ()
  returnA -< y / t
```

This signal function says that the output signal is the integral `y` of the
input signal `x`, divided by the time `t`. The above syntax uses a Haskell
extension called Arrows. If you are unhappy using arrow syntax, you can also
write that code using applicative style and/or arrow combinators:

```haskell
signalFunction1 :: SF Double Double
signalFunction1 = (/) <$> integral <*> time

signalFunction2 :: SF Double Double
signalFunction2 = (integral &&& time) >>^ (/)
```

All three are equivalent, and it's a matter of which one you like best. To run
this example, we need to provide the inputs, the times, and consume the output:
```haskell
firstSample :: IO Double   -- sample at time zero
firstSample =
  return 1.0  -- time == 0, input == 1.0

nextSamples :: Bool -> IO (Double, Maybe Double)
nextSamples _ =
  return (0.1, Just 1.0) -- time delta == 0.1s, input == 1.0

output :: Bool -> Double -> IO Bool
output _ x = do
  print x     -- print the output
  return False -- just continue forever
```

This is a trivial example, since the integral of the constant function 1.0 over
time, divided by the time, is always 1.0! Nevertheless, we are now ready to
run!

```
ghci> reactimate firstSample nextSamples output signalFunction
1.0
1.0
1.0
1.0
1.0
1.0
...
```

There is a directory with examples, which includes two basic SDL examples and
one with using a Nintendo Wii Remote. You can install them with:

```
$ cabal sandbox init         # Optional, but recommended
$ cabal update
$ cabal install Yampa -fexamples
```

There are many programs written in Yampa:

* [Haskanoid](https://github.com/ivanperez-keera/haskanoid): a game that uses
  SDL multimedia, wiimote and kinect. It's cross platform and works in desktop,
  mobile, and [web](http://ivanperez-keera.github.io/haskanoid/haskanoid.jsexe/index.html)
  (compiled with [GHCJS](https://github.com/ghcjs/ghcjs)).
* [Space invaders](https://hackage.haskell.org/package/SpaceInvaders).
* [Frag](https://hackage.haskell.org/package/frag): a 3D first person shooting game.
* [Peoplemon](https://linearity.itch.io/peoplemon): a role playing game.
* [Yampa-2048](https://github.com/ksaveljev/yampa-2048): an implementation of
  the game 2048 using Yampa and Gloss.
* [MandelbrotYampa](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/MandelbrotYampa):
  a "hello world" using SDL2, Yampa and OpenGL.
* [Haskelloids](https://github.com/keera-studios/Haskelloids): a reproduction of the Atari 1979 classic "Asteroids"

A more comprehensive list can be obtained using the reverse dependency finder
(http://packdeps.haskellers.com/reverse/Yampa), but only programs uploaded to
hackage are listed.

| <img src="https://raw.githubusercontent.com/ivanperez-keera/haskanoid/master/screenshots/android.gif?raw=true" width="200" alt="Haskanoid Video" style="max-width:200px;"> | <img src="https://media.giphy.com/media/SsBOvldu1FnSyNZW1z/giphy.gif" width="200" alt="Peoplemon by Alex Stuart" style="max-width: 200px;"> | <img src="http://ksaveljev.github.io/2048.gif" width="110" alt="Space Invaders" style="max-width: 110px;"> |
|-------------------------------------------|---------------|-------------------------|
| Haskanoid, SDL cross-platform arkanoid. | Peoplemon, a role playing game | Yampa2048, a gloss board game |

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
* HTML Canvas via JS Dom (for an example, see [haskanoid's GHCJS branch](https://github.com/ivanperez-keera/haskanoid/blob/ghcjs/src/Display.hs))
* HTML5 Canvas via blank-canvas (see [yampa-canvas](https://github.com/ku-fpg/yampa-canvas))
* Gloss (see [yampa-gloss](https://github.com/ivanperez-keera/yampa-gloss))
* Diagrams (see [diagrams example](examples/Diagrams.hs))
* [Keera Hails](https://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-yampa) (reactive programming framework with GTK, WX, Qt, Android, iOS and HTML support).

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
  * [Collection of Yampa diagrams](doc/diagrams/Diagrams.md)
  * [Henrik Nilsson's publications](http://www.cs.nott.ac.uk/~psznhn/papers.html)
  * [Ivan Perez's publications ](http://www.cs.nott.ac.uk/~psxip1/)
  * [First Year PhD Report](http://www.cs.nott.ac.uk/~psxip1/papers/2014-Perez-1st-year-report.pdf) (Ivan Perez, 2014), chapter 3 includes a review of FRP and outlines some existing issues.

## Related projects

* [yampa-sdl2](https://github.com/Simre1/yampa-sdl2): Yampa and SDL2 made easy.
* [Haskell-OpenGL-Tutorial](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/MandelbrotYampa) same as [here](https://github.com/ubuntunux/LambdaEngine3D/tree/master/MandelbrotYampa): Visually attractive mandelbrot implementation.
* [graphui](https://github.com/sinelaw/graphui): Attempt to implement Graphui.
* [hamball](https://github.com/harley/hamball): 3D, LAN FPS game of hamster balls flying around and shooting lasers written in Haskell.
* [yampa-glfw](https://github.com/deepfire/yampa-glfw): GLFW backend.
* [Spaceinvaders](https://github.com/ivanperez-keera/SpaceInvaders): Re-write of the classic space invaders.
* [Haskanoid](https://github.com/ivanperez-keera/haskanoid): Arkanoid clone in SDL with wiimote and kinect support that works on windows, linux, Mac, Android, and Web.
* [Magic Cookies](http://haskell.games): iOS/Android haskell puzzle game.
* [Pang-a-lambda](https://github.com/keera-studios/games-pang-a-lambda): 2D arcade game inspired by the classic super-pang.
* [yampa-canvas](https://github.com/ku-fpg/yampa-canvas): Backend to blank-canvas / HTML5 Canvas.
* [yampa-gloss](https://github.com/ivanperez-keera/yampa-gloss): Gloss backend.
* [diagrams example](examples/Diagrams.hs): Demonstration of Yampa with Diagrams.
* [Keera Hails](https://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-yampa): Backend for reactive framework with GTK, WX, Qt, Android, iOS and HTML support.
* [YampaSynth](http://hackage.haskell.org/package/YampaSynth): Software synthesizer.
* [Frag](https://wiki.haskell.org/Frag): 3D first person shooter.
* [cuboid](http://hackage.haskell.org/package/cuboid): 3D puzzle game with GLUT.
* [Haskelloids](http://hackage.haskell.org/package/Haskelloids): Reproduction of the Atari 1979 classic "Asteroids".
* [YFrob](hackage.haskell.org/package/YFrob): Yampa-based library for programming robots.
* [YampaShooter](https://github.com/werk/YampaShooter): Top-down team based networked tank game.
* [Functional Reactive Virtual Reality](https://imve.informatik.uni-hamburg.de/projects/FRVR): a fork of Yampa with extensions for VR.
* [Dunai](https://github.com/ivanperez-keera/dunai): An FRP implementation inspired by Yampa that extends SFs with a monad.
* [Bearriver](https://github.com/ivanperez-keera/dunai/tree/develop/examples/bearriver): API-compatible Yampa replacement built on top of dunai using Monadic Stream Functions.
* [The Bearriver Arcade](https://github.com/walseb/The_Bearriver_Arcade): A couple of arcade games made using bearriver.

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
