# Minesweeper functional

Description: The final project for CS4012 requires you to build an auto-playing implementation of the game minesweeper; use Scotty to build the user interface
Due Date: Nov 22, 2019
Priority: Medium
Status: In Progress
Type: Assingment

The final project for CS4012 requires you to build an auto-playing implementation of the game *minesweeper*; use Scotty to build the user interface (special: if you wish you may use something else, for example Threpenny GUI which we will discuss in a coming class) and provide as complete an automatic player as you can.

Rules:

Minesweeper is a single-player logic puzzle; the [basic rules](http://www.freeminesweeper.org/help/minehelpinstructions.html) are simple (wikipedia has a [good description](http://en.wikipedia.org/wiki/Minesweeper_%28video_game%29) as well).

Phase 1

- Model the game of Minesweeper in Haskell. With no UI or interaction model this part of the project is simple enough.
- Build a UI for the program from phase 1 using Scotty. You can specify the details of how the user interacts to play the game (i.e. what the game looks like and exactly what controls are used to select locations and clear or flag them). It's probably best to keep this simple.

For phase one the following are necessary:

- Distribute mines randomly around the grid
- Allow the user to uncover and flag mines (interactively)
- Detect the endgame condition

Phase 2

- Add a 'play move' button to the game from phase 1. Pressing this button will *attempt* to play a good, safe, move. I will note that [Minesweeper is known to be NP-complete](http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ordmsw.htm), which means that a well written and efficient solver is, well, hard! That link contains some very good commentaries and links on solving minesweeper. Check out his PDF presentation linked there.

For this part it is *necessary* that

- the program play a move automatically if there is an unambiguously safe move available

It is *desirable* that

- If a safe move is not available the program will play the least dangerous move available (see the links below for some discussion of the probabilities).

Grading

- The basic implementation (phases 1 & 2) are worth up to 50% of the final marks. Your program needs to be
    - Well designed and commented
    - Accompanied by suitable documentation (this might be in the form of literate source files, or it may feature additional documents).
- The auto-player is worth the second half of the marks. At a minimum your player should be able to make simple and obvious moves (i.e. spot a few of the basic patterns as described in the Minesweeper wiki). The more sophisticated the player the better! Remember that you don't need to solve the whole game here, but try to implement at least *one* advanced tactic.

## **Resources**

- [Richard Kaye](http://web.mat.bham.ac.uk/R.W.Kaye/minesw/) has the definitive discussion of NP-Completeness in minesweeper (of course).
- H[ere](http://luckytoilet.wordpress.com/2012/12/23/2125/) is a description (with a link to some Java sources) of one project (that got a [writeup at hackaday.com](http://hackaday.com/2012/12/24/how-to-write-your-own-minesweeper-solver/))
- Some good strategic discussion at the [Minesweeper Wiki](http://www.minesweeper.info/wiki/Strategy)
- [Simon Thompson](http://www.cs.kent.ac.uk/people/staff/sjt/craft2e/Games/) has some Haskell minesweeper code (reading someone elses design can make it hard to see your own creative solution, so I would counsel against reading too much of this at first)
- [Sean Barrett](http://nothings.org/games/minesweeper/) has a nice discussion of calculating probabilities in minesweeper.
- [Raphaël Collet](http://www.springerlink.com/content/l0cxhkuwv5edjpc0/) wrote a paper about solving Minesweeper with constraints (Raphaël's put a copy on the web [here](http://www.info.ucl.ac.be/~pvr/minesweeper.pdf), and a cached copy of the paper is linked from [citeseer](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.139.4635)).
- [Minesweeper](http://hackage.haskell.org/package/minesweeper) even has a package in the Haskell hackage!