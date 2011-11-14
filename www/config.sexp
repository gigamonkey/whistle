(:ports (:http 8080))

(:root-directory "www/")
(:log-directory "logs/")

(:passwords
 ("peter" "$2a$10$.LUYdhWFju8kCoIWTYqgFuAey7yMi7v9xNPxoPTf2ObIKnleXQEAq" :wheel)
 ("xach" "$2a$10$.Drt/61JpYZnxnqsPfNKFeDVpOJ4VvEKgf5.bpEqbo7utQYlrxIuG" :foo)
 ("H4ns" "$2a$10$.LDUthf2d7pSc03/VUG9p.GwN0xJGd6pGTjANBHYLd7p4zLSpei0m" :bar)
 ("Krystof" "$2a$10$..Tgtf88jyFux6wpVkNLQ.AksJKTzY3C6zulKZwu4AbTejpO.GZ.K" :foo :bar))

(:protections
 ("^/foo/" :foogroup)
 ("/bar/" :bargroup))

(:redirects
 ("/foo/bar/quux.html" "/" 301)
 ("/foo/bar/boom.html" "/" 302))

(:urls (".*" whistle::default-handler))
