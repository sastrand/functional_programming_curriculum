# Functional Programming with Haskell
## A Lab Curriculum for Portland State University

This series of exercises is meant to provide a hands-on introduction to functional programming with Haskell completed in five modules.

Each module provides examples, lecture outline, and exercises for about six hours of class time that includes time for students to complete most or all of the exercises presented.

## Reference materials

#### Lab reference for students:

*Learn You a Haskell for Great Good!*  
Miran Lipovača, 2011  
Available for free from the writer [here](http://learnyouahaskell.com/)

#### Sources referenced in development of curriculum:

*Real World Haskell*  
Bryan O'Sullivan, John Goerzen, and Don Stewart  
O'Reilly Media, first edition, 2008  
Available for free from the publishers [here](http://book.realworldhaskell.org/read/)

*Programming in Haskell*  
Graham Hutton  
Cambridge University Press, second edition, 2016

*Parallel and Concurrent Programming in Haskell*  
Simon Marlow  
O'Reilly Media, 2013


### Learning Objectives
```
Students who successfully complete this lab will be able to:
    * Describe the key characteristics of the functional paradigm.
    * Define algebraic data types to model useful domains.
    * Identify and program:
        - Head recursion
        - Tail recursion
        - Mutual recursion
        - Multiple recursion
        - Infinite and halting recursion
    * Write small Haskell programs to manipulate lists and trees.
    * Use first-class functions as data values.
    * Use higher order functions, including maps, folds, and composition
    * Write a multithreaded map-reduce application over lists
```

### Lab Outline

```
-------------------------------------
-- Module 1: weeks 1 - 2
-- Why Haskell
-- Lists and Functions
-------------------------------------
 
  * Simon Peyton Jones, "Haskell is Useless"
    - What makes Haskell so safe?
    - Strong, static typing
    - Declarative perspective
 
  * Learn you a Haskell (chptrs 1 & 2)
    - Functions
    - Lists
    - Ranges
    - Comprehensions
    - Tuples

  * Some operations on lists
    See Learn You a Haskell chptr 6.
    - `zip`
    - `zipWith`
    - `map`
    - `filter`
    - `takeWhile` 

  * Assignments
    - Working with functions
    - Working with lists
    - List comprehensions
    - Control flow


-------------------------------------
-- Module 2: weeks 3 - 4
-- Recursion
-------------------------------------
 
  * Learn you a Haskell (chptr 4)
    Function syntax
    - Pattern matching
    - Guards
    - `where`
    - `let`
    - Case expressions
 
  * Learn you a Haskell (chptr 5)
    Recursion
    - Intro examples
    - Head recursion
    - Tail recursion
    - Mutual recursion

  * Assignments
    - Write some awesome recursive functions
    - Use tuples to create a key-value store
    - And functions to perform set operations on lists


-------------------------------------
-- Module 3: weeks 5 - 7
-- Recursive data structures
-------------------------------------
 
  * Learn you a Haskell (chptr 6)
    Higher Order Functions
    - Currying
    - Lambdas
    - Folds
    - Function application
    - Function composition

  * Assignments
    - Add functionality to a BST module
    - Use higher-order functions on lists


-------------------------------------
-- Module 4: weeks 8 - 10
-- Types in Haskell
-------------------------------------
 
  * Learn you a Haskell (chptr 3)
    Types and typeclasses
    - Types
    - Type variables
    - Typeclasses
 
  * Learn you a Haskell (chptr 8)
    Building our own Types and Typeclasses
    - Algebraic data types
    - `record` syntax
    - Type parameters
    - `Maybe`
    - The IO Type

  * Assignments
    - Define a Library type and supporting functions
    - Expand a stack ADT
    - Write a text-processing program using `do` and the IO Type


-------------------------------------
-- Future work
-------------------------------------
-- Module 5
-- Parallelism and Haskell
-------------------------------------
 
  * Parallelism
    Control.Parallel
    - No race conditions or deadlocks
    - Guaranteed deterministic
 
  * Assignment
    - Write a parallel map-reduce application to run on some large-ish data

```

