TicTacToe
=========

Write an API for the tic-tac-toe game. Do not use variables -- they are not permitted. This includes libraries that expose in-line updates. No exceptions (or non-termination) in exposed functions -- all functions return a consistent value for every element of their domain. The follow API methods should exist:

* `move`: takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board. This function can only be called on a board that is in-play. Calling move on a game board that is finished is a *compile-time type error*.

* `whoWon`: takes a tic-tac-toe board and returns the player that won the game (or a draw if neither). This function can only be called on a board that is finished. Calling move on a game board that is in-play is a *compile-time type error*.

* `takeBack`: takes either a finished board or a board in-play that has had at least one move and returns a board in-play. It is a compile-time type error to call this function on an empty board.

* `playerAt`: takes a tic-tac-toe board and position and returns the (possible) player at a given position. This function works on any type of board.

* Other API functions that you may see fit. These can be determined by also writing an interactive console application that uses the API -- other useful functions are likely to arise.

You should write automated tests for your API. For example, the following universally quantified property holds true:

`forall Board b. forall Position p. such that (not (positionIsOccupied
p b)). takeBack(move(p, b)) == b`

You should encode this property in an automated specification test. For Scala, use ScalaCheck. For Haskell, QuickCheck. For Java, consider [Functional Java](http://functionaljava.org/). For other languages such as C# or F#, you may need to search around.

Haskell-specific
----------------

If you choose to use Haskell, also take advantage of its superior tooling:

* Build with CABAL
* Include a `.ghci` file for convenience when developing
  * http://haskell.org/ghc/docs/6.12.2/html/users_guide/ghci-dot-files.html
* API documented using Haddock
  * [http://www.haskell.org/haddock/doc/html/index.html](http://haskell.org/ghc/docs/6.12.2/html/users_guide/ghci-dot-files.html)
* Code style examined using hlint
  * `cabal install hlint`
  * Produce a report (`--report`)
  * [http://community.haskell.org/~ndm/darcs/hlint/hlint.htm](http://community.haskell.org/~ndm/darcs/hlint/hlint.htm)
* Use hoogle and hayoo to find library functions
  * [http://haskell.org/hoogle/](http://haskell.org/hoogle/)
  * [http://holumbus.fh-wedel.de/hayoo/hayoo.html](http://holumbus.fh-wedel.de/hayoo/hayoo.html)


Extra-curricular
----------------
* Write an opponent that never loses
* Write an opponent with easy, medium, hard difficulty levels
