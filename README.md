# destructuring-match
A small macro to destructure lists based on literal values and tests. Useful in creating more complex macros
and parsing command syntax, as in irc bots or other text interfaces.

## usage
`(destructuring-match (key string-mode binding-mode on-failure) expression match-form body)`
`(destructuring-match-switch expression match-forms)`

### options
`string-mode` changes how literal strings and list elements are compared.

* `string` is the default value, strings match other strings without case sensitivity (as if compared with `equalp`)
* `symbol` - literal strings (`"foo"`) match symbols of the same name (mixed-case symbols are not matched)
* `case-sensitive`
* `regex`

`binding-mode` determines if free variables by default match a single list element or multiple.
* `single` - free variables match only a single element of the list, unless modified by `multiple`, as in `(multiple foo)`
* `multiple` - free variables match one or more elements of the list, and are always lists, unless the
variable is designated as `single`
* `mixed` - the default, both `single` and `multiple` modifiers may be used on free varibles, unmodified ones can match any number of
elements, but ones that only match one item of the list will not be lists themselves.

`on-failure` is evaluated and returned if the match fails. `nil` by default.

### clauses
* `choice &rest forms` matches the first form in the list that matches
* `single var` matches only one element of the list
* `multiple var` matches a list of elements
* `optional &rest forms` optionally matches all forms, sequentially
* `test var function` makes the form match the condition described as well as the structure of the match
* `take var function` same as test, except the result of the function is bound to the var instead of the normal result
* `sublist &rest forms` starts matching a sublist

### examples
```
(destructuring-match 
   (split " " "If You Give a Mouse a Cookie")
   ("if" "you" verb "a" noun "a" object)
   (list verb noun object)) => ("Give" "Mouse" "Cookie")
   
(defun cookie (str)
   (destructuring-match :on-failure "couldn't match list :(" (split " " str) 
      ("if" "you" verb "a" noun "a" object)
      (list verb noun object)))

(cookie "if you give a mouse a cookie")       => ("give" "mouse" "cookie")
(cookie "if you give a moose a muffin")       => ("give" "moose" "muffin")
(cookie "if you give a cat")                  => "couldn't match list :("
(cookie "random string")                      => "couldn't match list :("
(cookie "if you give a pig a purple pancake") => ("give" "pig" ("purple" "pancake"))
```

```
(defun cookie (str)
   (destructuring-match-switch (split " " str)
      (("if" "you" "give" "a" noun (switch "a" "an") object) (list "give" noun object))
      (("if" "you" "take" "a" noun "to" (optional "the") place) (list "take" noun place))))

(cookie "if you give a mouse a cookie")      => ("give" "mouse" "cookie")
(cookie "if you take a mouse to school")     => ("take" "mouse" "school")
(cookie "if you take a mouse the movies")    => ("take" "mouse" "movies")
```

```
(destructuring-match '(1 2 3 4) (x y rest) (list x y rest)) => (1 2 (3 4))
```

```
(destructuring-match '(a b c d e f) (x 'd y) (list x y)) => ((a b c) (e f))
```

```
(destructuring-match '(a b c (d e f g)) (x (y 'f z)) (list x y z)) => ((a b c) (d e) g)
```


### edge cases
You can use the same variable twice, or as many times as you want. The value after matching will be the binding evaluated last. This allows you to use `_` for values you don't care about.

Using already bound lexical variables is fine, they'll be shadowed by the new binding, as normal.

## dependencies and installation

This project requires quicklisp to run. It's been tested on sbcl, but should work on other CL implementations.
To install quicklisp, head over to [quicklisp's website](https://www.quicklisp.org/beta/) and follow 
the instructions there. Make sure you run `(ql:add-to-init-file)`, otherwise quicklisp won't be avaliable 
when you start your interpreter.

To use it, clone this repo into `~/quicklisp/local-projects`, and run `(ql:quickload 'destructuring-match)`.
