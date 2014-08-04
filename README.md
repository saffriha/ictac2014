<h1 align="center">Precise Interprocedural Side-Effect Analysis</h1>

<h5 align="center">Manuel Geffken, Hannes Saffrich, and Peter Thiemann<br>
University of Freiburg, Germany</h5>

<h6 align="center">ICTAC 2014</h6>


## Introduction

This project contains a prototype implementation of the interprocedural
side-effect analysis introduced in [1].

The implementation is written in Scala and supports the analysis of Java source
projects. After the analysis has finished, a Scala Swing window displays the
analysis results together with profiling information and other statistics.

<center><img src="http://users.zankapfel.org/~stereoid/example_results.png"/></center>


## Usage

The project includes an eclipse project file, which should work out of the box,
and provides a run configuration, which analyses the example Java project from
the samples sub directory.

Alternatively an otherwise compiled version of the analysis can be started from
the command line by giving two arguments describing the main class of a project
and the project's class path.

Example:
``` bash
scala gaganalysis.Main "pkg.subpkg.Main" -cp "project/src"
```
The file in "project/src/pkg/subpkg/Main.java" is assumed to contain the project's
main method.

The implementation has been tested with Scala 2.10.4 on a 64-bit linux machine.


## Overview

The project contains 3 subdirectories:
  - **lib** contains the 4 dependencies of the implementation for Scala 2.10:
    - **soot** is a program analysis and transformation framework written in
      Java.  Used for call graph generation and control flow graph traversals of
      the dataflow analysis. Soot translates the Java input to an 3-address code
      intermediate language called Jimple which we then further abstract with
      respect to our analysis.

    - **scalaz** contains functional programming patterns and a somewhat
      complete reimplementation of the Haskell library.

    - **graph-core** is a generic graph library from the scalax project.

    - **jung** provides graph visualization for Java Swing, which we use to
      visualize generalized access graphs.

  - **samples** contains example Java source files used by the sample eclipse
    runconfigurations.

  - **src** contains the sources of the implementation. The details of the sources
    are covered in the Source section of this readme.


## Compacting points-to pairs

To enhance performance in fix-point computations, the analysis optionally supports
compacting of the points-to set by combining some of the contained points-to
pairs during analysis.

There are 3 compacting modes which can be specified using a command line
parameter:

- **--pts-compacting=none** (default) does not compact points-to pairs at all.

- **--pts-compacting=precise** compacts two points-to pairs
(r1, o1) and (r2, o2) to (r2, o2) iff r1 ⊑ r2 and o1 ⊑ o2, and hence preserves
the analysis precision,

- **--pts-compacting=conservative** compacts two points-to pairs
(r1, o1) and (r2, o2) to (r2, o1 ⊔ o2) iff r1 ⊑ r2, and hence leads to a
conservative precision loss if o1 ⋢ o2.

We found that, in the example causing the performance impact, both
precise and conservative compacting lead to the same points-to sets, meaning that
for all pairs (r1, o1) and (r2, o2) r1 ⊑ r2 implied o1 ⊑ o2 in this example.
Compared to using no compacting at all, in the most extreme case compacting
reduced a points-to set from 1600 to 200 pairs and reduced the analysis duration
from 302 to 13 seconds.

The eclipse project includes 3 run configurations which run the example project
using one of the compacting modes respectively.


## Sources

Each *.scala* source file contains a short comment after the import declarations
describing the contained code. This might be helpful when navigating the source
files the first time.

A short introduction to the implementation's architecture will follow.


## References

[1]: Geffken, Saffrich, Thiemann. Precise Interprocedural Side-Effect Analysis.

