# destructuring-match
A small macro to destructure lists based on literal values and tests. Useful in creating more complex macros
and parsing command syntax, as in irc bots or other text interfaces.

## usage
`(destructuring-match expression style match-form &rest body)`

## dependencies and installation

This project requires quicklisp to run. It's been tested on sbcl, but should work on other CL implementations.
To install quicklisp, head over to [quicklisp's website](https://www.quicklisp.org/beta/) and follow 
the instructions there. Make sure you run `(ql:add-to-init-file)`, otherwise quicklisp won't be avaliable 
when you start your interpreter.

To use it, clone this repo into `~/quicklisp/local-projects`, and run `(ql:quickload 'destructuring-match)`.
