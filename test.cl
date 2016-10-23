(ql:quickload 'destructuring-match)
(use-package 'destructuring-match)

(destructuring-match '(a b c) (foo) foo)
