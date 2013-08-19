CombLogic
=========

## This project is a [combinatory logic](http://en.wikipedia.org/wiki/Combinatory_logic) playground.

### Two features are supported now:
1. resolve abstraction, (for example **[x,y,z].xz(yz) -> S**),
2. weak reduction, (for example **(SKK)x -> x**).

### A current problems are:
1. Infinite recursion (for example **SII(SII)**).


## How to run
1. Start sbt. 
2. Type run. REPL prompt will be appeared. 
3. Type your expression then enter. 
4. Abstractions and weak reduction will be applied.
5. Type "q:" without quotas for exit.

## Abstraction syntax
There is an abstraction with multiple binds supported here. 
Abstraction may be in arbitatrary position of the term.
* [x].x
* SK([x].x)K
* S([x,y,z].zy)K
* ([x].x)([x].y)KL([z].K)K

