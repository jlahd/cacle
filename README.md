# cacle - Extensible Cache services for Common Lisp

## 1. Introduction

cacle implements a generic cache management facility with configurable
and extensible cache replacement policies.  The actual cached data can
be stored anywhere, with cacle taking charge of keeping track of which
entry is to be discarded next when more space is needed for a new
entry.

cacle is built to be thread safe and thus ready to be used in
multithreaded environments, such as web services.  In case multiple
threads request the same piece of data simultaneously, the data is
only obtained from the data provider once, then distributed to all the
threads that requested it.

**Note!** While cacle itself is thread safe, the provider and cleanup
functions are *not* called in a locked context, as they may take a
long time to complete, during which fetches from the cache should be
possible.  It is on the user's responsibility to protect against
potential thread conflicts in the provider and cleanup functions.

## 2. Installation

cacle can be installed using Quicklisp:
```lisp
* (ql:quickload "cacle")
; Loading "cacle"
("cacle")
* (require "cacle")
"cacle"
NIL
```

## 3. Examples

## 3.1. Walkthrough

As cacle does not care about the nature of the data in the cache, nor
where it comes from, it needs a *provider* function that is able to
obtain a block of data, given the block's *key*.  What the *key* is is
up to the application; to cacle, it is just something that can
function as a key to a hash table.  Indeed, cacle stores the current
contents of the cache in a hash table; the table's *test* can be
specified when creating the cache.

Let's introduce a provider that maps key *k* into a block *k* units
long.  As the block's data content, a simple string is constructed.
Of course, the string is not of the length indicated by the provider,
but that is exactly the point: cacle does not care what the data is.
The string could, for example, be the name of the actual file that
holds the contents of the data block.

```lisp
* (defun test-provider (key)
    "A provider returns two values: The data for the element, and the element's size."
    (format t "Providing data for key ~a~%" key)
    ;; Fetching the data takes some time...
    (sleep 1)
    (values (format nil "value for ~a" key)
            key))
TEST-PROVIDER
```

Now we are ready to create a cache that manages these blocks of data.

```lisp
* (defparameter *my-cache* (cacle:make-cache 100 #'test-provider :policy :lru))
*MY-CACHE*
* (cacle:cache-max-size *my-cache*)
100
* (cacle:cache-size *my-cache*)
0
* (cacle:cache-count *my-cache*)
0
```

So, the cache is empty.  Let's fetch some data.

```lisp
* (cacle:cache-fetch *my-cache* 42)
Providing data for key 42
"value for 42"
NIL
```

Note the one-second delay in the function call.  The function returned
two values; meaning of the second value (here `NIL`) will be discussed
later in this document.

etching the same data again does not cause a call to the provider,
with the value returned immediately.

```lisp
* (cacle:cache-fetch *my-cache* 42)
"value for 42"
NIL
* (cacle:cache-fetch *my-cache* 42)
"value for 42"
NIL
```

Next, widen the scope of requested items:

```lisp
* (cacle:cache-fetch *my-cache* 17)
Providing data for key 17
"value for 17"
NIL
* (cacle:cache-fetch *my-cache* 33)
Providing data for key 33
"value for 33"
NIL
* (cacle:cache-fetch *my-cache* 42)
"value for 42"
NIL
* (cacle:cache-size *my-cache*)
92
* (cacle:cache-count *my-cache*)
3
```

The cache is already quite full, with 92 units out of 100 used.  What
happens if we now request data with a fresh key?

```lisp
* (cacle:cache-fetch *my-cache* 24)
Providing data for key 24
"value for 24"
* (cacle:cache-size *my-cache*)
99
* (cacle:cache-count *my-cache*)
3
```

One of the stored blocks of data needs to go.  Since of the three keys
in the cache, both 42 and 33 have been referenced after 17, 17 has been
discarded:

```lisp
* (mapcar #'(lambda (key)
              (cacle:cache-fetch *my-cache* key :only-if-cached t))
          '(42 17 33 24))
("value for 42" NIL "value for 33" "value for 24")
```

Setting the *:only-if-cached* option prevents the calling of the
provider if the queried data is not found in the cache, just returning
*NIL* instead.

If multiple threads request the same data, the provider is only called
once, with all threads eventually getting the same result data:

```lisp
* (loop with out = *standard-output*
        for i below 10
        collect (bt:make-thread #'(lambda ()
                                    (let ((*standard-output* out))
                                      (cacle:cache-fetch *my-cache* 72)))))
Providing data for key 72
(#<PROCESS Anonymous thread(37) [Active] #x...> #<PROCESS Anonymous thread(38) [semaphore wait] #x...> #<PROCESS Anonymous thread(39) [semaphore wait] #x...> #<PROCESS Anonymous thread(40) [semaphore wait] #x...> #<PROCESS Anonymous thread(41) [semaphore wait] #x...> #<PROCESS Anonymous thread(42) [semaphore wait] #x...> #<PROCESS Anonymous thread(43) [semaphore wait] #x...> #<PROCESS Anonymous thread(44) [semaphore wait] #x...> #<PROCESS Anonymous thread(45) [semaphore wait] #x...> #<PROCESS Anonymous thread(46) [semaphore wait] #x...>)
* (mapcar #'bt:join-thread *)
("value for 72" "value for 72" "value for 72" "value for 72" "value for 72" "value for 72" "value for 72" "value for 72" "value for 72" "value for 72")
* (loop with first = (first *)
        for i in *
        always (eq i first))
T
```

Finally, get rid of all the cached data:

```lisp
* (cacle:cache-flush *my-cache*)
NIL
* (cacle:cache-size *my-cache*)
0
* (cacle:cache-count *my-cache*)
0
```

To facilitate situations where data expiring from the cache needs some
cleaning up - such as in the abovementioned case of the cache being on
the disk - an optional cleanup function can be defined for the cache.
This function is called whenever a block of data is discarded from the
cache.

```lisp
* (defun test-cleanup (data)
    (format t "Cleaning up: ~a~%" data))
TEST-CLEANUP
* (setf *my-cache* (cacle:make-cache 100 #'test-provider :policy :lru :cleanup #'test-cleanup))
#<CACHE #x...>
```

As cacle is designed to be used on multiple threads, a situation may
arise where multiple threads request data from the cache
simultaneously and an entry is removed from the cache by another
thread before the thread that requested it can use the data.  To
prevent this situation, when a cleanup function has been defined for
the cache, each call to *cache-fetch* must be paired with a call to
*cache-release*.  The release function is given as an argument the
second value returned by *cache-fetch*.  The cleanup function will not
be called for the data if there are live references (fetches without
corresponding releases) for the data.

```lisp
* (defparameter *42* (multiple-value-list (cacle:cache-fetch *my-cache* 42)))
Providing data for key 42
*42*
* *42*
("value for 42" #<LINKED-CACHE-ENTRY key 42 #x...>)
```

The tag datum should be treated opaque by the caller and used only as
an argument to *cache-release*.

A utility macro, *with-cache-fetch*, is provided for ensuring the
pairing of *cache-fetch* and *cache-release*:

```lisp
* (cacle:with-cache-fetch item (*my-cache* 17)
    (format t "my data: ~a~%" item))
Providing data for key 17
my data: value for 17
NIL
* (cacle:with-cache-fetch item (*my-cache* 33)
    (format t "my data: ~a~%" item))
Providing data for key 33
my data: value for 33
NIL
* (cacle:with-cache-fetch item (*my-cache* 24)
    (format t "my data: ~a~%" item))
Providing data for key 24
my data: value for 24
NIL
* (cacle:with-cache-fetch item (*my-cache* 55)
    (format t "my data: ~a~%" item))
Providing data for key 55
Cleaning up: value for 33
Cleaning up: value for 17
my data: value for 55
NIL
```

Note that even before the last function call, the item for the key 42
has already expired from the cache, since the total would otherwise
exceed the cache's limit of 100.  However, it has not been cleaned up,
because it is still reserved by the very first call to *cache-fetch*
that has not been matched with the call to *cache-release* yet.

```lisp
* (cacle:cache-release *my-cache* (second *42*))
Cleaning up: value for 42
NIL
```

**Note!** This example also demonstrates a property in the design of cacle that
should be understood before using it: The maximum size defined for the
cache is the size of the live objects in the cache and does not
include items that have already been scheduled for removal, pending a
call to *cache-release*, or items that are being fetched to the cache.
That is, the total size of the cache may exceed its limit by the
combined size of the items currently being used by the application.

### 3.2. A simple CDN node

A simple node in a content distribution network could be built using
cacle as follows.  The content being distributed is fetched from a
content server, and the cache resides on the local disk.

*Warning: untested code - written as an example, not to be used as a
real world CDN*

```
(defparameter *content-server* "http://server.example.com/")
(defparameter *disk-space* #x1000000000) ;; 64 GB
(defparameter *cache-path* "/var/cache/%")

(defun fetch-content (uri)
  ;; This provider function retrieves data from the content server
  (let* (size
         (file (fad:with-output-to-temporary-file (out :template *cache-path* 
                                                       :element-type '(unsigned-byte 8))
                 (multiple-value-bind (in status)
                     (drakma:http-request (concatenate 'string *content-server* uri)
                                          :want-stream t)
                   ;; Copy the retrieved data into a file
                   (when (<= 200 status 299)
                     (fad:copy-stream in out)
                     (setf size (file-length out)))))))
    (if size
        (values file size) ; success
        (progn ; error
          (ignore-errors (delete-file file))
          (values nil 0)))))

(defun cleanup-content (file)
  ;; When content removed from the cache, delete the corresponding file
  (ignore-errors (delete-file file)))

;; Set object lifetime to 3600 seconds to force a refresh once per hour
(defparameter *cache* (cacle:make-cache *disk-space* #'fetch-content 
                                        :test 'equal
                                        :cleanup #'cleanup-content
                                        :policy :lfuda
                                        :lifetime 3600))

;; Function called by the web server to serve a certain file
(defun serve-file (uri)
  (cacle:with-cache-fetch file (*cache* uri)
    (if file
        (progn
          ;; Send back the data in file
          ...)
        (progn
          ;; Report a 404 not found
          ))))
```

That's it.  On an incoming request, *serve-file* will fetch the
corresponding content from a file in the cache.  If the content is not
cached, it is transparently fetched from the content server, stored in
the cache, and sent to the end user.

## 4. Cache replacement policies

A [cache replacement
policy](https://en.wikipedia.org/wiki/Cache_replacement_policies)
defines how existing entries are discarded from the cache to make room
for the data that is currently being loaded.  cacle implements a set
of simple replacement policies and provides means for the user to
build their own policy, if necessary.  Additionally, a lifetime can be
defined for the cache, after which cached data expires and a fresh
copy is obtained instead.

The following cache replacement policies are implemented:

* First In First Out (*:fifo*): Data that has been in the cache for the longest time is discarded
* Last In First Out (*:lifo*): Most recently added data is discarded
* Least Recently Used (*:lru*): Data that has gone unused for the longest time is discarded
* Most Recently Used (*:mru*): Most recently used data is discarded
* Random (*:random*): A randomly selected piece of data is discarded
* Least Frequently Used (*:lfu*): Data with the lowest number of fetches is discarded
* Least Frequently Used with Dynamic Aging (*:lfuda*): An aging variable is introduced to LFU to prefer discarding data that has been used a lot in the history but less often recently.

### 4.1. Creating your own replacement policy

Each cache replacement policy is responsible of keeping track of all
the entries currently in the cache.  A suitable daata structure should
be chosen so that the relevant operations are as fast as possible.

A policy is a class that is instantiated once per a cache managed by
the policy.  Policies should be built as the
*replacement-policy* class as the superclass. Additionally, a
number of derived classes, listed in the next section, are exported by
*cacle* and can be used as basis for a custom policy.

To be able to store entries in the policy's desired manner, each cache
entry must be able to hold certain policy specific data.  To
accommodate this, policies may define specializations of the cache
entry base class *cache-entry*.  Policies must treat the base
class opaque and access the base class's data only through the
exported readers (*entry-key*, *entry-size* and
*entry-expiry*).  Policies can add slots as necessary for
their own operation, and *change-class* of an entry-to-be-added in the
*entry-added* generic function.

For example, *linked-list-replacement-policy* stores the entries
in a circular doubly linked list to serve simple policies such as FIFO
or LRU.  In a list structure, insertion and removal of entries are
constant time operations, but lookups for entries other than the first
or last are costly.  The respective cache entry class,
*linked-cache-entry*, defines two additional slots that hold the
forward and backward pointers:

```lisp
(defclass linked-cache-entry (cache-entry)
  ((next)
   (prev)))
```

The replacement policy class itself holds the head of the list of
entries, and, when a new entry is added to cache, changes the entry's
class and pushes it at the head of the list.  The call to
*change-class* is done in a *:before* method so that classes derived
from *linked-list-replacement-policy* do not need to remember to
*(call-next-method)* in their *entry-added* method implementations.

```lisp
(defclass linked-list-replacement-policy (replacement-policy)
  ((head :initform (make-instance 'linked-cache-entry))))

(defmethod entry-added :before ((policy linked-list-replacement-policy) (entry cache-entry))
  (change-class entry 'linked-cache-entry))

(defmethod entry-added ((policy linked-list-replacement-policy) (entry cache-entry))
  (link-after entry (slot-value policy 'head)))
```

The following classes have been implemented for the bundled
replacement policies:

* *fifo-replacement-policy*
* *lifo-replacement-policy*
* *lru-replacement-policy*
* *mru-replacement-policy*
* *random-replacement-policy*
* *lfu-replacement-policy*
* *lfuda-replacement-policy*.

*lfu-replacement-policy* uses a heap structure to store entries in
their changing order of precedence.  *lfuda-replacement-policy* builds
on it.

## 5. Dictionary

### 5.1. The cache class

[Standard class] **cache**

*cache* is the main class of the system.  It contains
information abnout the data blocks currently stored in a certain
cache.  Note that while storing the information, *cache* leaves the
storage of the actual data to the user: The data could be, for
example, a vector of octets directly linked to the entry; a certain
file in the filesystem; or a bunch of bananas in the storage room of
a zoo.

While you can create an instance of *cache* directly with
*make-instance*, it is recommended to use the convenience function
*make-cache*.

---
[Generic reader] **cache-max-size** *cache* => *number*

[Generic writer] (setf (**cache-max-size** *cache*) *new-max-size*)

Retrieves or sets the maximum cache size.

---
[Generic reader] **cache-provider** *cache* => *function*

[Generic writer] (setf (**cache-provider** *cache*) *new-provider*)

Retrieves or sets the cache's provider function.

---
[Generic reader] **cache-cleanup** *cache* => *function*

[Generic writer] (setf (**cache-cleanup** *cache*) *new-cleanup*)

Retrieves or sets the cache's cleanup function.

---
[Generic reader] **cache-lifetime** *cache* => *number*

[Generic writer] (setf (**cache-lifetime** *cache*) *new-lifetime*)

Retrieves or sets the cache object lifetime in seconds.

---
[Generic reader] **cache-policy** *cache* => *replacement-policy*

Retrieves the cache's replacement policy.

---
[Generic function] **cache-size** *cache* => *number*

Returns the sum of the sizes of data items currently stored in the
cache.

---
[Generic function] **cache-count** *cache* => *integer*

Returns the number of data items currently stroed in the cache.

### 5.2. Cache functions

[Function] **make-cache** *max-size provider &key (test 'eql) (policy :fifo) lifetime cleanup* => *cache*

This function creates a new cache instance with the specified maximum
size and provider function.

*max-size* defines the cache's capacity in some units of the
application's choice - for example, bytes, kilograms, or bananas.

*provider* must be a function that takes a single argument, key of the
data to provide, and returns two values: the data and its size.

*test* is the same as *make-hashtable*'s *test* and affects the
cache's underlying hashtable for key equality comparisons.

*policy* defines the cache replacement policy.  It must be *NIL* if
and only if *max-size* is NIL as well.  To specify a policy from the
set offered by cacle, you can use a keyword (*:fifo*, *:lifo*, *:lru*,
*:mru*, *:random*, *:lfu* or *:lfuda*).  To use a custom removal
policy, pass an instance of the policy class.

*lifetime* defines an optional object lifetime in seconds.  If
*lifetime* is defined, *cache-fetch* will not return objects returned
by *provider* longer than this time ago.

*cleanup* defines an optional callback function to be called for data
that is being discarded from the cache.  The function receives a
single parameter - the data - and its return value is discarded.

---
[Generic function] **cache-fetch** *cache &key only-if-cached* => *(values datum tag)*

Fetches a datum from the cache for the given key.  If the datum is not
currently cached and the *only-if-cached* flag is not set, it is
retrieved from the provider function, added to the cache, and then
returned.  The *tag* return value must be specified in a corresponding
call to *cache-release* before it will be cleaned up.

---
[Generic function] **cache-release** *cache tag* => *NIL*

Releases a reference obtained by a call to *cache-fetch*.

---
[Macro] **with-cache-fetch** *var (cache key &key only-if-cached) &body body*

Wraps the given body between calls to *cache-fetch* and
*cache-release*, ensuring that the data fetched from the cache is
valid inside *body* and will be released afterwards.

---
[Generic function] **cache-remove** *cache key* => *(or T NIL)*

Removes the datum for the specified key from the cache.  Returns *T*
if the data was currently cached, *NIL* otherwise.

---
[Generic function] **cache-flush** *cache* => *NIL*

Removes all entries from the cache.

### 5.3. Cache removal policies

[Standard class] **cache-entry**

Base class for cache entries.  All entries are created as instances of
this class, but the active removal policy may change the entry's class
in the call to *entry-added*.

[Generic reader] **entry-key**

[Generic reader] **entry-valid-p**

[Generic reader] **entry-size**

[Generic reader] **entry-expiry**

Readers for the cache entry's basic information.  **entry-size** will
signal an *unbound-slot* condition when the entry is not valid (when
**entry-valid-p** returns *NIL*).

To ensure thread safety, these functions must not be used outside of
the cache removal policy callback functions *entry-added*,
*access-entry*, *entry-removed* and *evict-entry*.  The same
restriction applies to all functions related to cache entries.

---
[Standard class] **linked-cache-entry**

Cache entry that can be stored in a circular doubly linked list for
bookkeeping.

[Generic reader] **entry-next** *linked-cache-entry* => *linked-cache-entry*

[Generic reader] **entry-previous** *linked-cache-entry* => *linked-cache-entry*

[Generic function] **unlink** *linked-cache-entry* => *linked-cache-entry*

[Generic function] **link-before** *linked-cache-entry linked-cache-entry* => *linked-cache-entry*

[Generic function] **link-after** *linked-cache-entry linked-cache-entry* => *linked-cache-entry*

These functions traverse and modify the doubly linked list formed by
the entries.  Each *linked-cache-entry* starts as a one-entry-long
list of its own, liked to itself in both directions.

---
[Standard class] **indexed-cache-entry**

An indexed cache entry simply associates a free-form index with each cache entry.

[Generic reader] **entry-index** *indexed-cache-entry* => *value*

[Generic writer] (setf (**entry-index** *indexed-cache-entry*) *new-index*)

---
[Standard class] **replacement-policy**

Base class for all replacement policies.

[Generic function] **entry-added** *policy entry* => *anything*

The cache calls this function for each new cache entry.  The policy
should initialize whatever bookkeeping is necessary; usually, this
begins with changing the class of the entry to something able to hold
the bookkeeping information.

[Generic function] **access-entry** *policy entry* => *(or T NIL)*

When an entry is accessed in the cache, this function is called.  The
function should verify that the entry is still valid and update the
bookkeeping data related to entry accesses.  Returning *T* means that
the entry is still valid and should be returned to the caller;
returning *NIL* removes the entry and results in a new call to the
provider.

Note that *access-entry* is not called for the very first access (when
*entry-added* is called).

[Generic function] **entry-removed** *policy entry* => *anything*

This function notifies the policy that an entry has been removed from
the cache.  The policy should update its bookkeeping data to keep
track of the situation.

[Generic function] **evict-entry** *policy size-hint* => *cache-entry*

When the cache space runs out, this function is called for the policy.
The policy instance should decide which of the currently present
entries is to be discarded to make room for the new one, remove it
from its books, and return the entry.

The *size-hint* parameter tells how much space needs to be freed from
the cache in order to fit in the new entry; the policy may use this
information if it wishes.  If not enough space is freed by the
returned *cache-entry*, the cache simply calls *evict-entry* again
until enough space has been freed.

Note that *entry-removed* is not called for the entry returned by
*evict-entry*.

---
[Standard class] **linked-list-replacement-policy**

This policy stores entries in a linked list, pushing any new items
after the head.  *evict-entry* is not implemented, so the class cannot
be directly used as a replacement policy.

[Generic reader] **linked-list-head** *linked-list-replacement-policy* => *linked-cache-entry*

Returns the head node of the linked list.  The head node is a
sentinel; it is a *linked-cache-entry* without a key or data, simply
serving as a point for attaching the actual data nodes.

---
[Standard class] **fifo-replacement-policy**

[Standard class] **lifo-replacement-policy**

[Standard class] **lru-replacement-policy**

[Standard class] **mru-replacement-policy**

[Standard class] **random-replacement-policy**

[Standard class] **lfu-replacement-policy**

[Standard class] **lfuda-replacement-policy**

These classes implement their respective cache replacement policies.

## 6. Tests

Unit tests for cacle are written using
[FiveAM](https://common-lisp.net/project/fiveam/).  They are hidden
behind the *#+5am* read-time conditional; to enable the tests, load
FiveAM before compiling cacle.  After that, you can run the test suite
from the REPL:

```lisp
* (5am:run! 'cacle:cacle-tests)

Running test suite CACLE-TESTS
 Running test [...]
 [...]
 Did 619 checks.
    Pass: 619 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

NIL
```

## 7. License

[MIT](https://opensource.org/licenses/MIT)
