# destructuring-match
A small macro to destructure lists based on literal values and tests. Useful in creating more complex macros
and parsing command syntax, as in irc bots or other text interfaces.

## usage
`(destructuring-match (key mode single) expression match-form &rest body)`

### clauses
* `choice &rest forms` matches any number of forms, takes the first to work
* `single var` causes it to match only one element of the list
* `optional &rest forms` if it matches, take that out of the list, keep matching either way
* `test var function` makes the form match the condition described as well as the structure of the match
* `take var function` same as test, except the result of the function is bound to the var instead of the normal result

parsing out key arguments?
single-key arguments

## dependencies and installation

This project requires quicklisp to run. It's been tested on sbcl, but should work on other CL implementations.
To install quicklisp, head over to [quicklisp's website](https://www.quicklisp.org/beta/) and follow 
the instructions there. Make sure you run `(ql:add-to-init-file)`, otherwise quicklisp won't be avaliable 
when you start your interpreter.

To use it, clone this repo into `~/quicklisp/local-projects`, and run `(ql:quickload 'destructuring-match)`.
