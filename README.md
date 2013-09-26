This is a chess engine written in portable Common Lisp.

Design/Approach
---------------

Coming in to this project, I was very inexperienced with Common Lisp. I do have some background in Scheme and Clojure (and I have done some research in computer chess), but I had never done anything with CL. I am still obviously inexperienced, but I have found that I can write non-trivial software in it (such as this program).

Initially, I was worried about approaching a project like this in a dynamically typed language. Like most chess engines, it makes significant use of bitwise operations, packs data into machine-sized words, and so on. It turns out CL is perfectly suited to this task. I used CL's type system extensively in this project, and I have found it to be significantly "stronger" than some other statically typed languages out there. With SBCL, these operations are also very fast.

Progress
--------

I started this project for the [Lisp in Summer Projects](http://lispinsummerprojects.org/welcome) contest, but I ended up being busy for most of the summer (I got an internship, I was working on a research project, and I'm preparing my grad school applications), so the vast majority of this code was written in the last few weeks of September. Nevertheless, it is a program that runs and is capable of playing chess reasonably intelligently, even if it is not as full-featured or well-tested as I had hoped.

Originally I was developing a bitboard-based board representation, but after I was delayed and resumed the project this month I switched to the easier-to-manage [0x88 board representation](http://chessprogramming.wikispaces.com/0x88). 

Additionally, the program is incomplete or buggy in several areas: checkmate/stalemate detection, under-promotions, and castle captures, for example.

Future Work
-----------

Top of my list for future work is definitely finishing the user interface, ideally with xboard/winboard integration. As it is, a game of chess can be played interactively in the REPL, but that's not very convenient. Beyond that, I plan to add typical features like search extensions and a transposition table, and work on a more extensive static evaluation function.

References
----------

I used several sources in writing this program.

 * [Micro-max](http://home.hccnet.nl/h.g.muller/max-src2.html) was a major help. I used it as a reference and an inspiration for the overall architecture of the program.

 * [ChessBin](http://www.chessbin.com/) was the source for the piece-square tables in the static evaluation function. It was the most useful Google result on the first page for "chess piece square tables." 