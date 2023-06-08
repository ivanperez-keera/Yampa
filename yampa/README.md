<div align="center">

# Yampa

[![Build Status](https://api.travis-ci.com/ivanperez-keera/Yampa.svg?branch=master)](https://app.travis-ci.com/github/ivanperez-keera/Yampa)
[![Version on Hackage](https://img.shields.io/hackage/v/Yampa.svg)](https://hackage.haskell.org/package/Yampa)

Domain-specific language embedded in Haskell for programming hybrid (mixed
discrete-time and continuous-time) systems. Yampa is based on the concepts of
Functional Reactive Programming (FRP).

[Installation](#installation) •
[Examples](#examples) •
[Related projects](#related-projects) •
[Documentation](#documentation) •
[Contributions](#contributions) •
[History](#history)

</div>

## Features

- Implements Functional Reactive Programming.

- Allows for dynamic programs whose structure changes over time.

- Isolates of effect-free signal functions from effectful actions.

- Runs fast and is memory efficient.

- Has been industry tested.

- Provides a robust, elegant, stable interface.

- Has well-defined semantics.

- Supports applicative, functional and arrowized styles.

- Supports all platforms and enjoys multiple backends.

- Programs can be tested with QuickCheck and debugged using Haskell Titan.

## Table of Contents

- [Installation](#installation)
  - [Pre-requisites](#pre-requisites)
  - [Compilation](#compilation)
- [Examples](#examples)
- [Related projects](#related-projects)
  - [Games and applications](#games-and-applications)
  - [Use in industry](#use-in-industry)
  - [Backends](#backends)
  - [Testing](#testing)
  - [Other projects](#other-projects)
- [Documentation](#documentation)
  - [API documentation and tutorials](#api-documentation-and-tutorials)
  - [Publications](#publications)
- [Contributions](#contributions)
  - [Discussions, issues and pull requests](#discussions-issues-and-pull-requests)
  - [Structure and internals](#structure-and-internals)
  - [Style](#style)
  - [Version control](#version-control)
  - [Versioning model](#versioning-model)
- [History](#history)

# Installation
<sup>[(Back to top)](#table-of-contents)</sup>

## Pre-requisites
<sup>[(Back to top)](#table-of-contents)</sup>

To use Yampa, you must have a Haskell compiler installed (GHC). We currently
support all versions of GHC from 7.6.3 to modern versions (9.X series as of
this writing).

On Debian/Ubuntu, the Haskell toolchain can be installed with:

```sh
$ apt-get install ghc cabal-install
```

On Mac, they can be installed with:

```sh
$ brew install ghc cabal-install
```

## Compilation
<sup>[(Back to top)](#table-of-contents)</sup>

Once you have a working set of Haskell tools installed, install Yampa from
[hackage](http://hackage.haskell.org/package/Yampa) by executing:

```sh
$ cabal update
$ cabal install --lib Yampa
```

Running the following will print the word `Success` if installation has gone
well, or show an error message otherwise:

```
$ runhaskell <<< 'import FRP.Yampa; main = putStrLn "Success"'
```

# Examples
<sup>[(Back to top)](#table-of-contents)</sup>

Getting Yampa to run is trivial. FRP is about values that change over time. In
Yampa, a system is defined by a signal function (SF), which determines how the
varying input and the varying output relate.

Code can be written in multiple styles: _applicative style_, _arrowized style_,
and just plain _arrow combinators_. All three are compatible and
interchangeable.

For example, the following signal function takes a value, integrates it, and
then divides that value by the current time:

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
input signal `x`, divided by the time `t`. The elements between `<-` and `-<`
are always signal functions (in this case, `integral` and `time` are signal
functions used to define another signal function).

The example above is written in arrow syntax and uses a Haskell extension
called Arrows. If you are unhappy using arrow syntax, you can implement the
same behavior using applicative style and/or arrow combinators:

```haskell
-- Applicative style
signalFunction1 :: SF Double Double
signalFunction1 = (/) <$> integral <*> time

-- Functional style with arrow combinators
signalFunction2 :: SF Double Double
signalFunction2 = (integral &&& time) >>^ (/)
```

All three are equivalent, and it's a matter of which one you like best.

To run this example, we need to provide the inputs, the times, and consume the
output:
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
$ cabal update
$ cabal install Yampa -fexamples
```

# Related projects
<sup>[(Back to top)](#table-of-contents)</sup>

## Games and applications
<sup>[(Back to top)](#table-of-contents)</sup>

* [cuboid](http://hackage.haskell.org/package/cuboid): 3D puzzle game with GLUT.
* [Frag](https://hackage.haskell.org/package/frag): a 3D first person shooting game.
* [hamball](https://github.com/harley/hamball): 3D, LAN FPS game of hamster balls flying around and shooting lasers written in Haskell.
* [Haskanoid](https://github.com/ivanperez-keera/haskanoid): a game that uses SDL multimedia, wiimote and kinect. It's cross platform and works in desktop, mobile, and [web](http://ivanperez-keera.github.io/haskanoid/haskanoid.jsexe/index.html) (compiled with [GHCJS](https://github.com/ghcjs/ghcjs)).
* [Haskelloids](https://github.com/keera-studios/Haskelloids): a reproduction of the Atari 1979 classic "Asteroids"
* [LaneWars](https://github.com/Mattiemus/LaneWars): Top-down MOBA game with online multiplayer.
* [MandelbrotYampa](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/MandelbrotYampa): a "hello world" using SDL2, Yampa and OpenGL.
* [Pang-a-lambda](https://hackage.haskell.org/package/pang-a-lambda): 2D arcade game inspired by the classic super-pang.
* [Peoplemon](https://linearity.itch.io/peoplemon): a role playing game.
* [Spaceinvaders](https://github.com/ivanperez-keera/SpaceInvaders): Re-write of the classic space invaders.
* [The Bearriver Arcade](https://github.com/walseb/The_Bearriver_Arcade): A couple of arcade games made using bearriver, a library that
implements the Yampa API.
* [Yampa-2048](https://github.com/ksaveljev/yampa-2048): an implementation of the game 2048 using Yampa and Gloss.
* [Yampa - Game of Life](https://github.com/Poselsky/Yampa-Game-Of-Life): an implementation of Game of Life using SDL2 and OpenGL.
* [YampaShooter](https://github.com/werk/YampaShooter): Top-down team based networked tank game.
* [YampaSynth](http://hackage.haskell.org/package/YampaSynth): Software synthesizer.
* [YFrob](hackage.haskell.org/package/YFrob): Yampa-based library for programming robots.

A more comprehensive list can be obtained using the reverse dependency finder
(http://packdeps.haskellers.com/reverse/Yampa), but only programs uploaded to
hackage are listed.

| <img src="https://raw.githubusercontent.com/ivanperez-keera/haskanoid/master/screenshots/android.gif?raw=true" width="200" alt="Haskanoid Video" style="max-width:200px;"> | <img src="https://media.giphy.com/media/SsBOvldu1FnSyNZW1z/giphy.gif" width="200" alt="Peoplemon by Alex Stuart" style="max-width: 200px;"> | <img src="http://ksaveljev.github.io/2048.gif" width="110" alt="Space Invaders" style="max-width: 110px;"> |
|-------------------------------------------|---------------|-------------------------|
| Haskanoid, SDL cross-platform arkanoid. | Peoplemon, a role playing game | Yampa2048, a gloss board game |

## Use in industry
<sup>[(Back to top)](#table-of-contents)</sup>

[Keera Studios](https://keera.co.uk) uses Yampa to create Haskell games available on [Google Play for Android](https://play.google.com/store/apps/developer?id=Keera+Studios&hl=en_US&gl=US) and [iTunes for iOS](https://apps.apple.com/us/developer/keera-studios-ltd/id1244709870):

<table>
  <tr>
    <td>
    <p align="center">
      Magic Cookies!
      <br /><br />
      <a href="http://magiccookies.haskell.games">
        <img src="https://raw.githubusercontent.com/keera-studios/magic-cookies/master/screencap.gif" height="auto" alt="Magic Cookies! Video" style="max-height:175px;"></a>
      <img width="441" height="1">
      <br />
      <sup>Copyright © 2015 - 2020 Keera Studios Ltd. All Rights Reserved.</sup>
      <br />
      <a href="https://itunes.apple.com/us/app/magic-cookies/id1244709871">
        <img src="https://raw.githubusercontent.com/edent/SuperTinyIcons/master/images/svg/apple.svg" width="32" height="32"></a>
      &nbsp;&nbsp;
      <a href="https://play.google.com/store/apps/details?id=uk.co.keera.games.magiccookies">
        <img src="https://raw.githubusercontent.com/edent/SuperTinyIcons/master/images/svg/google_play.svg" width="32" height="32"></a>
    </p>
    </td>
    <td>
    <p align="center">
      Magic Cookies 2!
      <br /><br />
      <a href="http://magiccookies2.haskell.games">
        <img src="https://raw.githubusercontent.com/keera-studios/magic-cookies2/master/screencap.gif" height="auto" alt="Magic Cookies 2! Video" style="max-height:175px;"></a>
      <img width="441" height="1">
      <br />
      <sup>Copyright © 2015 - 2022 Keera Studios Ltd. All Rights Reserved.</sup>
      <br />
      <a href="https://apps.apple.com/us/app/magic-cookies-2/id1602081807">
        <img src="https://raw.githubusercontent.com/edent/SuperTinyIcons/master/images/svg/apple.svg" width="32" height="32"></a>
      &nbsp;&nbsp;
      <a href="https://play.google.com/store/apps/details?id=uk.co.keera.games.magiccookies2">
        <img src="https://raw.githubusercontent.com/edent/SuperTinyIcons/master/images/svg/google_play.svg" width="32" height="32"></a>
    </p>
    </td>
    <td>
    <p align="center">
      Enpuzzled
      <br />
      <br />
      <a href="http://enpuzzled.haskell.games">
        <img src="https://raw.githubusercontent.com/keera-studios/enpuzzled/master/enpuzzled.gif" height="auto" alt="Enpuzzled Video" style="max-height: 175px;"></a>
      <img width="441" height="1">
      <br />
      <sup>Copyright © - 2017 - Keera Studios Ltd - All Rights Reserved.</sup>
      <br />
      <a href="https://apps.apple.com/us/app/enpuzzled/id1460083994">
        <img src="https://raw.githubusercontent.com/edent/SuperTinyIcons/master/images/svg/apple.svg" width="32" height="32"></a>
      &nbsp;&nbsp;
      <a href="https://play.google.com/store/apps/details?id=uk.co.keera.games.enpuzzled&hl=en_US&gl=US">
        <img src="https://raw.githubusercontent.com/edent/SuperTinyIcons/master/images/svg/google_play.svg" width="32" height="32"></a>
    </p>
    </td>
    </tr>
    <tr>
    <td colspan="3">
      <sub>
        <sup>
          Keera, Keera Studios, Magic Cookies, Magic Cookies 2, the Magic
          Cookies logo, the Magic Cookies 2 logo, the Magic Cookies splash
          screen, the Magic Cookies 2 splash screen, Enpuzzled, the Enpuzzled
          splash screen, and the Enpuzzled logo are trademarks of Keera Studios
          Ltd. Google Play and the Google Play logo are trademarks of Google
          LLC. Apple, the Apple logo, iPhone, and iPad are trademarks of Apple
          Inc., registered in the U.S. and other countries and regions. App
          Store is a service mark of Apple Inc.
        </sup>
      </sub>
    </td>
  </tr>
</table>

## Backends
<sup>[(Back to top)](#table-of-contents)</sup>

Yampa is backend agnostic, you can ultimately connect it to any backend you
want. Existing backends include:
* SDL (see [Haskanoid](https://github.com/ivanperez-keera/haskanoid) for an example)
* SDL2 (see [yampa-sdl2](https://github.com/Simre1/yampa-sdl2))
* OpenGL / GLUT (see [Haskell-OpenGL-Tutorial](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/MandelbrotYampa)).
* GLFW (see [yampa-glfw](https://github.com/deepfire/yampa-glfw))
* WX (see wxHaskell)
* HTML Canvas via JS Dom (for an example, see [haskanoid's GHCJS branch](https://github.com/ivanperez-keera/haskanoid/blob/ghcjs/src/Display.hs))
* HTML5 Canvas via blank-canvas (see [yampa-canvas](https://github.com/ku-fpg/yampa-canvas))
* Gloss (see [yampa-gloss](https://github.com/ivanperez-keera/yampa-gloss))
* Diagrams (see [diagrams example](https://github.com/ivanperez-keera/Yampa/blob/develop/yampa/examples/Diagrams.hs))
* [Keera Hails](https://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-yampa) (reactive programming framework with GTK, WX, Qt, Android, iOS and HTML support).

## Testing
<sup>[(Back to top)](#table-of-contents)</sup>

Yampa comes with a sophisticated testing library that allows you to use
QuickCheck to test your games, and use a time-travel debugger. These features
are described in the paper [Testing and Debugging Functional Reactive
Programming](http://dl.acm.org/authorize?N46564).

You can find the additional projects at:
* [yampa-test](https://github.com/ivanperez-keera/Yampa/tree/develop/yampa-test)
* [Haskell Titan](https://github.com/keera-studios/haskell-titan)

## Other projects
<sup>[(Back to top)](#table-of-contents)</sup>

* [Bearriver](https://github.com/ivanperez-keera/dunai/tree/develop/dunai-frp-bearriver): API-compatible Yampa replacement built on top of dunai using Monadic Stream Functions.
* [Dunai](https://github.com/ivanperez-keera/dunai): An FRP implementation inspired by Yampa that extends SFs with a monad.
* [Functional Reactive Virtual Reality](https://imve.informatik.uni-hamburg.de/projects/FRVR): a fork of Yampa with extensions for VR.
* [graphui](https://github.com/sinelaw/graphui): Attempt to implement Graphui.
* [Haskell-OpenGL-Tutorial](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/MandelbrotYampa) same as [here](https://github.com/ubuntunux/LambdaEngine3D/tree/master/MandelbrotYampa): Visually attractive mandelbrot implementation.
* [Keera Hails](https://github.com/keera-studios/keera-hails/tree/master/keera-hails-reactive-yampa): Backend for reactive framework with GTK, WX, Qt, Android, iOS and HTML support, with a library to connect Yampa signal functions.

# Documentation
<sup>[(Back to top)](#table-of-contents)</sup>

## API documentation and tutorials
<sup>[(Back to top)](#table-of-contents)</sup>

The API of Yampa is thoroughly documented
[on hackage](https://hackage.haskell.org/package/Yampa).
Documentation is also available in the
[Haskell wiki page for Yampa](https://wiki.haskell.org/Yampa).

## Publications
<sup>[(Back to top)](#table-of-contents)</sup>

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
  * [Collection of Yampa diagrams](https://github.com/ivanperez-keera/Yampa/tree/develop/doc/diagrams/Diagrams.md)
  * [Henrik Nilsson's publications](http://www.cs.nott.ac.uk/~psznhn/papers.html)
  * [Ivan Perez's publications](https://ivanperez.io)
  * [First Year PhD Report](http://www.cs.nott.ac.uk/~psxip1/papers/2014-Perez-1st-year-report.pdf) (Ivan Perez, 2014), chapter 3 includes a review of FRP and outlines some existing issues.

# Contributions
<sup>[(Back to top)](#table-of-contents)</sup>

If this library helps you, you may want to consider
[buying the maintainer a cup of coffee](https://flattr.com/submit/auto?user_id=ivanperez-keera&url=https://github.com/ivanperez-keera/Yampa&title=Yampa&language=&tags=github&category=software).

## Discussions, issues and pull requests
<sup>[(Back to top)](#table-of-contents)</sup>

**Discussions**

If you have any comments, questions, ideas, or other topics that you think will
be of interest to the Yampa community, start a new discussion
[here](https://github.com/ivanperez-keera/Yampa/discussions). Examples include:

- You've created a new game or application that uses Yampa.
- You've written or found a library that helps use Yampa in a particular
  domain, or apply it to a specific platform.
- You've written or found a paper that mentions Yampa.
- You have an idea for an extension that will enable writing programs that are
  not currently possible or convenient to capture.
- You think you've found a bug.
- You want to propose an improvement (e.g., make the code faster or smaller).
- You have a question.
- Something in the documentation, a tutorial or a Yampa / FRP paper is unclear.
- You like the project and want to introduce yourself.

**Issues**

If a specific change is being proposed (either a new feature or a bug fix), you
can *open an issue* documenting the proposed change
[here](https://github.com/ivanperez-keera/Yampa/issues).

If you are unsure about whether your submission should be filed as an issue or
as a discussion, file it as a discussion. We can always move it later.

**Pull requests**

Once we determine that an issue will be addressed, we'll decide who does it and
when the change will be added to Yampa. Even if you implement the solution,
someone will walk you through the steps to ensure that your submission conforms
with our version control process, style guide, etc. More information on our
process is included below.

Please, do not just send a PR unless there is an issue for it and someone from
the Yampa team has confirmed that you should address it. The PR is *very*
likely to be rejected, and we really want to accept your contributions, so it
will make us very sad. Open a discussion / issue first and let us guide you
through the process.

## Structure and internals
<sup>[(Back to top)](#table-of-contents)</sup>

This project is split in two:

- _Yampa_: FRP library.
- _Yampa-test_: a testing layer for Yampa.

Yampa's unit tests are mostly implemented as tests inside the yampa-test
library. The module hierarchy of `yampa-test/tests/Test` mirrors that of Yampa.

A directory `yampa/examples` contains a number of examples of Yampa programs,
some of which can be installed with the flag `-fexamples`.

## Style
<sup>[(Back to top)](#table-of-contents)</sup>

We follow [this style
guide](https://keera.co.uk/wp-content/uploads/2021/11/haskellguide-v1.3.0.pdf)
for the Haskell code and [this style
guide](https://keera.co.uk/wp-content/uploads/2021/11/cabalguide-v1.3.0.pdf)
for Cabal files.

## Version control
<sup>[(Back to top)](#table-of-contents)</sup>

We follow [git flow](http://nvie.com/posts/a-successful-git-branching-model/).
In addition:

- Please document your commits clearly and separately.
- Always refer to the issue you are fixing in the commit summary line with the
  text `Refs #<issue_number>` at the end.
- If there is no issue for your change, then open an issue first and document
  what you are trying to achieve/improve/fix.
- Do not address more than one issue per commit or per PR. If two changes are
  not directly related to one another, they belong in different PRs, issues and
  commits.
- Document what you did in the respective CHANGELOGs in a separate commit
  before you send a PR.
- Make sure your changes conform to the [coding
  style](https://keera.co.uk/wp-content/uploads/2021/11/haskellguide-v1.3.0.pdf).

See the recent repo history for examples of this process. Using a visual repo
inspection tool like `gitk` may help.

## Versioning model
<sup>[(Back to top)](#table-of-contents)</sup>

The versioning model we use is the standard in Haskell packages. Versions have
the format `<PUB>.<MAJOR>.<MINOR>(.<PATCH>)?` where:

- `<PUB>` is just a way to signal important milestones or used for promotional
  reasons (to indicate a major advancement). A zero on this position has no
  special meaning.

- `<MAJOR>` increases on incompatible API changes.

- `<MINOR>` increases on backwards-compatible changes.

- `<PATCH>` (optional) increases on small changes that do not affect behavior
  (e.g., documentation).

# History
<sup>[(Back to top)](#table-of-contents)</sup>

This library was created by Henrik Nilsson and Antony Courtney in 2002, while
working at Yale's Haskell group, led by Paul Hudak. The design and
implementation benefited from frequent discussions with other members of the
group, including also John Peterson. From 2008 to 2012, it was maintained by
George Giorgidze. In 2014, maintenance was passed to Ivan Perez.

Yampa is the longest standing FRP implementation in Haskell still in use. It
has seen and continues to see abundant use in industry, research, and academia.
We invite you to be part of this incredible community project as a user, a
contributor, and overall supporter.

Yampa is named after the Yampa river in Colorado, USA.
