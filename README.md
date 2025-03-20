# CS360 Programming Languages Course Materials

This repository contains the lecture materials and examples for the CS360 Programming Languages course at Drexel University.

## Course Structure

The course covers various programming language concepts and paradigms through the following lectures:

- **Lecture 02**: Scheme Programming
- **Lecture 04**: Streams
- **Lecture 07**: Lazy Evaluation
- **Lecture 08**: Introduction to Haskell
- **Lecture 09**: Algebraic Data Types
- **Lecture 10**: Polymorphism
- **Lecture 11**: Programs as Data
- **Lecture 12**: Lambda Calculus
- **Lecture 13**: Testing in Haskell
- **Lecture 15**: Interactive Programming
- **Lecture 16**: Monads

## Project Setup

Each lecture directory contains its own Haskell or Scheme project with:
- Source code examples
- Exercises
- Test cases
- Stack configuration files (for Haskell projects)

## Building and Running

### Haskell Projects
```bash
cd <lecture-directory>
stack build
stack test  # for projects with tests
```

### Scheme Projects
Use DrRacket or your preferred Scheme interpreter to run the `.rkt` files.

## Dependencies

- GHC (Glasgow Haskell Compiler)
- Stack (Haskell build tool)
- DrRacket (for Scheme files)
- Various Haskell packages (hspec, QuickCheck, etc.)

## License

This is a course repository. Please refer to your course syllabus for usage terms and conditions.
