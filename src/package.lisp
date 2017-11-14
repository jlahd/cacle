(cl:defpackage :cacle
  (:use #:cl)
  (:export
   #:cache #:cache-max-size #:cache-provider #:cache-cleanup #:cache-lifetime #:cache-policy
   #:cache-size #:cache-count
   #:make-cache #:cache-fetch #:cache-release #:with-cache-fetch #:cache-remove #:cache-flush
   #:cache-entry #:entry-key #:entry-valid-p #:entry-size #:entry-expiry
   #:linked-cache-entry #:entry-next #:entry-previous #:unlink #:link-before #:link-after
   #:indexed-cache-entry #:entry-index
   #:replacement-policy #:entry-added #:access-entry #:entry-removed #:evict-entry
   #:linked-list-replacement-policy #:linked-list-head
   #:fifo-replacement-policy #:lifo-replacement-policy
   #:lru-replacement-policy #:mru-replacement-policy
   #:random-replacement-policy
   #:lfu-replacement-policy #:lfuda-replacement-policy
   #:cacle-tests))
