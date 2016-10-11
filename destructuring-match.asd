(asdf:defsystem "destructuring-match"
   :serial t
   :version "1.0"
   :author "Dylan Ball <arathnim@gmail.com>"
   :maintainer "Dylan Ball <arathnim@gmail.com>"
   :description "matching list destructuring"
   :depends-on (alexandria iterate anaphora cl-ppcre)
   :components ((:file "destr-match")))
