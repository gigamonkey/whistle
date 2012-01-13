(:ports
 ;; Configure an acceptor to listen on 8080
 (:http 8080)

 ;; Not implemented yet but this is the idea.
 ;(:https :8081 :certificate "foo.cert" :private-key "something")
)

;; While reading the config file, *default-pathname-defaults* is bound
;; to the directory containing this file. File names in config
;; directives such as :log-directory and :include are thus relative to
;; that directory. Things that look like filenames such as the :root
;; argument to the static-file-handler handler definition can be
;; passed to MERGE-PATHNAME by the INITIALIZE-INSTANCE method on the
;; handler if that's appropriate.

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

;; Handlers are instantiated and associated with a name. The names of
;; handlers are used in the :urls clause to specify which handler
;; handles which patterns of URLs. Each handler is specified as a
;; class plus initargs.

(:handlers
 (default toot:static-file-handler :root "content/")
 ;;(spam spamminator :db "www/data/spam/")
 ;;(comments comments :data "www/data/comments/")
 (numbers numeral-handler))

;; The URLs clause specifies which handlers handle which patterns of
;; URLs. The first element of each clause is a cl-ppcre regexp which
;; if it matches the request path indicates the request should be
;; handled by the specified handler, named by the second element of
;; the clause. Any remaining elements of the clause are arguments to
;; the handlers GENERATE-RESPONSE method with the symbols $1, $2, etc.
;; refering to the strings matched by the regexp's capture groups.

(:urls
 ("^/spell-number/(\\d+)" numbers :number $1)
 (".*" default))
