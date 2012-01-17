;; Configure an acceptor to listen on 8080
(port :http 8080)

;; Not implemented yet but this is the idea.
;;(port :https 8081 :certificate "foo.cert" :private-key "something")

(logs "logs/")

(include "passwords.sexp")

(protections
 ("^/for-peter-only/" "peter")
 ("^/for-xach-only/" "xach")
 ("^/foo/" :foo)
 ("/bar/" :bar))

(redirects
 ("/foo/bar/quux.html" "/" 301)
 ("/foo/bar/boom.html" "/" 302))

(handlers
 (default toot:static-file-handler :root "content/")
 (numbers whistle::numeral-handler))

(urls
 ("^/spell-number/(\\d+)" numbers :number $1)
 (".*" default))
