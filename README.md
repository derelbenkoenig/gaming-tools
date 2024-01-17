# gaming-tools
tools for computing things while gaming

## ARK
usage: 

```shell
derelbenkoenig@DESKTOP-KPN2FNI:~/github/gaming-tools/ark$ python
Python 2.7.16 (default, Oct 10 2019, 22:02:15)
[GCC 8.3.0] on linux2
Type "help", "copyright", "credits" or "license" for more information.
>>> from ark import berries2 as b
>>> b(1320, 200)
{'berries': 150.0, 'time': (7, 28)}
>>>

```

## Shadow the Hedgehog

This tool helps you calculate what series of missions you need to do to get a certain route
added to the in-game "Library". It contains definitions of each level, including what level you
reach by doing each possible mission on that level

There are 326 possible story runs, each of which has a name
associated with it. Use it in the REPL (GHCI) like this:

```haskell
*ShadowTheHedgehog Control.Monad Map Data.Foldable Data.List> routeFromNum 29
[ Westopolis, Dark mission => Digital Circuit, Dark mission => Cryptic Castle, Hero mission => Sky Troops, Dark mission => Air Fleet, Neutral mission => Black Comet, Dark mission ]

*ShadowTheHedgehog Control.Monad Map Data.Foldable Data.List> printRoute (routeFromNum 29)
Westopolis, Dark mission
Digital Circuit, Dark mission
Cryptic Castle, Hero mission
Sky Troops, Dark mission
Air Fleet, Neutral mission
Black Comet, Dark mission

*ShadowTheHedgehog Control.Monad Map Data.Foldable Data.List> printRouteAligned (routeFromNum 29)
Westopolis      => Dark mission
Digital Circuit => Dark mission
Cryptic Castle  => Hero mission
Sky Troops      => Dark mission
Air Fleet       => Neutral mission
Black Comet     => Dark mission

*ShadowTheHedgehog Control.Monad Map Data.Foldable Data.List> westopolis
Westopolis, Paths: [Dark -> Digital Circuit, Neutral -> Glyphic Canyon, Hero -> Lethal Highway]

*ShadowTheHedgehog Control.Monad Map Data.Foldable Data.List> cosmicFall
Cosmic Fall, Paths: [Dark -> Dark Ending, Hero -> Hero Ending]
```
