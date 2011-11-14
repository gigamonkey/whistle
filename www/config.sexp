(:ports
 ;; Configure an acceptor to listen on 8080
 (:http 8080)

 ;; Not implemented yet but this is the idea.
 ;(:https :8081 :certificate "foo.cert" :private-key "something")
)

(:root-directory "www/")
(:log-directory "logs/")

(:include "passwords.sexp")

(:protections
 ;; User must be the named user or a member of the named group (or a
 ;; member of the :wheel group)
 ("^/for-peter-only/" "peter")
 ("^/for-xach-only/" "xach")
 ("^/foo/" :foo)
 ("/bar/" :bar))

(:redirects
 ("/foo/bar/quux.html" "/" 301)
 ("/foo/bar/boom.html" "/" 302))

(:in-package :whistle)

(:urls (".*" default-handler))
