# destructuring-match
A small macro to destructure lists based on literal values and tests. Useful in creating more complex macros
and parsing command syntax, as in irc bots or other text interfaces.

## usage
`(destructuring-match (key string-mode single fail) expression match-form (rest body))`

### options
`string-mode` changes how literal strings and list elements are compared.

* `t` is the default value, strings match other strings without case sensitivity (compared with `equalp`)
* `symbol` - literal strings (`"foo"`) match symbols of the same name (mixed-case symbols are not matched)
* `case-sensitive` should be pretty obvious
* `regex` - the value in the matching form is used as a regex

`single` determines if free variables by default match a single list element or multiple. Should be `t` or `nil`.

`fail` is evaluated and returned if the match fails. `nil` by default.

### clauses
* `choice &rest forms` matches any number of forms, takes the first to work
* `single var` matches only one element of the list
* `multiple var` matches a list of elements
* `optional &rest forms` optionally matches all forms, sequentially
* `test var function` makes the form match the condition described as well as the structure of the match
* `take var function` same as test, except the result of the function is bound to the var instead of the normal result
* `sublist &rest forms` starts matching a sublist

## dependencies and installation

This project requires quicklisp to run. It's been tested on sbcl, but should work on other CL implementations.
To install quicklisp, head over to [quicklisp's website](https://www.quicklisp.org/beta/) and follow 
the instructions there. Make sure you run `(ql:add-to-init-file)`, otherwise quicklisp won't be avaliable 
when you start your interpreter.

To use it, clone this repo into `~/quicklisp/local-projects`, and run `(ql:quickload 'destructuring-match)`.
