(asdf:defsystem "destr-match"
	:serial t
	:version "1.0"
	:author "Dylan Ball <arathnim@gmail.com>"
	:maintainer "Dylan Ball <arathnim@gmail.com>"
	:description "matching list destructuring"
	:depends-on (alexandria iterate anaphora)
	:components ((:file "destr-match")))
