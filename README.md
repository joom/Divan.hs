# Divan.hs [![Build Status](https://secure.travis-ci.org/joom/Divan.hs.svg)](http://travis-ci.org/joom/Divan.hs)

A Haskell program to check the *vezin* (meter) of [Ottoman *Divan* poetry](http://en.wikipedia.org/wiki/Ottoman_poetry).

If you want to learn more about the meter of these poems, [this is a place to start](http://en.wikipedia.org/wiki/Arabic_prosody). This repository currently does not have much documentation about why things are the way they are, since it's mostly related to the grammatical rules of Turkish and the prosody rules of Divan poetry.

## What does this do?

Currently, all Divan.hs does is to check the vezin patterns of a verse.

Tests should explain what Divan.hs currently is able to do. View `tests/tests.hs` to read the tests or run `cabal test` on your terminal to see the test results.

`Divan.Tefile.detectTefile` function currently doesn't cover all *vezin* patterns, it needs `Divan.Tefile.tefileMap` to be expanded. But still, Divan.Tefile now **is** able to take a verse and return the tefile names, such as "*mefâilün mefâilün feûlün*". (*Tef'ile*'s are the vezin pattern names.)

In the long run, it should aim to generate simple Divan poems by using the *mazmun* (imagery) patterns for a given *vezin*. But there is no work on that yet.

## How do I use it?

In order to use or compile the program you need to have [Haskell](http://www.haskell.org/) installed.

You also need to install [Guguk](https://github.com/joom/Guguk.git), an unstable library NLP library for Turkish, which is why it is not on Hackage yet. You can install Guguk with these commands:

```
git clone https://github.com/joom/Guguk.git && cd Guguk && cabal install
```

### To install the library

```bash
cabal install
```

### REPL

```bash
cabal repl
```

### Running the tests

```bash
cabal test
```

### To build the executable

```bash
cabal build divan
```

This is the terminal command to run Divan.hs on a part of one of Fuzûlî's *ghazal*'s.

```bash
# assuming default build location
./dist/build/Divan/divan example/input.txt
```

And this is the output we got from the program, for that *ghazal* excerpt:

```
Öyle ser-mestem ki idrâk etmezem dünyâ nedir
öy - le - ser - mes - tem - ki - id - râ - ket - me - zem - dün - yâ - ne - dir
-.---.---.---.-
fâilâtün / fâilâtün / fâilâtün / fâilün

Ben kimem sâkî olan kimdir mey û sahbâ nedir
ben - ki - mem - sâ - kî - o - lan - kim - dir - me - yû - sah - bâ - ne - dir
-.---.---.---.-
fâilâtün / fâilâtün / fâilâtün / fâilün


Âh u feryâdın Fuzûlî incidibdir' âlemi
â - hu - fer - yâ - dın - fu - zû - lî - in - ci - dib - dir - â - le - mi
-.---.---.---.-
fâilâtün / fâilâtün / fâilâtün / fâilün

Ger belâ-yı ışk' ile hoşnûd' isen gavga nedir
ger - be - lâ - yı - ışk - i - le - hoş - nûd - i - sen - gav - ga - ne - dir
-.-.-..--..--..-
fâilün / müfâ'aletün / müfteilün / müfteilün
```

The first line is the verse itself. The second line is how the program syllablizes the verse. The third line is the symbol string for the *vezin* (list of Open and Closed). The fourth line is a *tef'ile* list suggestion.

The last verse of this excerpt is incorrect, because the program cannot yet detect the *aruz* flaws (*kusur*) that are made on purpose. However, it must be pretty much functional so far!

## Further Documentation

Haddock generated documentation can be read [from here](http://joom.github.io/Divan.hs).

## License
[MIT License](http://joom.mit-license.org/)
