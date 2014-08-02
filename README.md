# Divan.hs

A Haskell program to check the *vezin* (prosody) of [Ottoman *Divan* poetry](http://en.wikipedia.org/wiki/Ottoman_poetry).

If you want to learn more about the prosody of these poems, [this is a place to start](http://en.wikipedia.org/wiki/Arabic_prosody). This repository currently does not have much documentation about why things are the way they are, since it's mostly related to the grammatical rules of Turkish and the prosody rules of Divan poetry.

## What does this do?

Currently, all Divan.hs does is to check the prosody patterns of a verse.

Tests should explain what Divan.hs currently is able to do. View `Test.hs` to read the tests or run `runhaskell Test.hs` on your terminal to see the test results.

`Divan.Tefile.detectTefile` function currently doesn't cover all *vezin* patterns, it needs `Divan.Tefile.tefileMap` to be expanded. Also, Divan.Tefile should be able to take a verse and return the tefile names, such as "*mefâilün mefâilün feûlün*". (*Tefile*'s are the vezin pattern names.)

In the long run, it should aim to generate simple Divan poems by using the *mazmun* (imagery) patterns for a given *vezin*.

## License
[MIT License](http://joom.mit-license.org/)
