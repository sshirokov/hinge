Hinge. A synonym for node.

An evented framework for Common Lisp.
Driven by [libev](http://software.schmorp.de/pkg/libev.html), like the cool kids.

Check out the [wiki](https://github.com/sshirokov/hinge/wiki) for docs and writeups
and stop by the `#hinge` channel on [Freenode](http://freenode.net/irc_servers.shtml)
to bug me in real time.

## Requirements

### System level dependencies

* [libev](http://software.schmorp.de/pkg/libev.html)
* [ZeroMQ](http://www.zeromq.org/) (2.1.11+)
* [SBCL](http://www.sbcl.org/) (With thread support)
* [curl](http://curl.haxx.se/) (Required by `make develop`)

[CCL](http://ccl.clozure.com/) is also supported, but SBCL is the
primary development platform, and is favored in the makefiles.

### Dependencies built outside of quicklisp

These deps are fetched and built with `make develop`

* [cl-ev](https://github.com/sbryant/cl-ev)
* [CFFI](http://common-lisp.net/project/cffi/) (Built from git, to support lisp-zmq)
* [lisp-zmq](https://github.com/galdor/lisp-zmq)

## Quickstart

Clone and init the project

```sh
# Clone the repo
$ git clone https://github.com/sshirokov/hinge.git
$ cd hinge

# Init the enviornment
$ make develop
```

Once you run `make develop` you're free to use Hinge in other
Lisp systems as long as you don't relocate the project directory.

Fire up the REPL and evaluate the following.
It should boot up and keep running an HTTP server on port 4545

```common-lisp
(ql:quickload :hinge)
(defpackage :hinge-example
  (:use :cl :hinge :hinge.http))

(in-package :hinge-example)

(let ((server (make-instance 'http-server)))
  (add-listener server "request"
                (lambda (request response)
                  (declare (ignorable request))
                  (write-head response 200 '(("Content-Type" . "text/html")))
                  (end response "Hello world!")))

  (bind server 4545))

(run :default)
```

You should now be able to throw HTTP requests at localhost:4545 with something
like `curl` and get replies:

```sh
$ time curl -i localhost:4545
HTTP/1.1 200 OK
Content-Length: 12
Content-Type: text/html

Hello world!
real	0m0.006s
user	0m0.004s
sys	0m0.000s
```

You can also throw something like `ab` at it,
which is somewhat mandatory despite being a poor benchmark:

```sh
$ ab -c 10 -n 1000 http://localhost:4545/
This is ApacheBench, Version 2.3 <$Revision: 655654 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking localhost (be patient)
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests


Server Software:
Server Hostname:        localhost
Server Port:            4545

Document Path:          /
Document Length:        12 bytes

Concurrency Level:      10
Time taken for tests:   0.397 seconds
Complete requests:      1000
Failed requests:        0
Write errors:           0
Total transferred:      51000 bytes
HTML transferred:       12000 bytes
Requests per second:    2519.51 [#/sec] (mean)
Time per request:       3.969 [ms] (mean)
Time per request:       0.397 [ms] (mean, across all concurrent requests)
Transfer rate:          125.48 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    0   0.0      0       0
Processing:     2    3   9.7      3     214
Waiting:        1    2   9.6      2     213
Total:          2    3   9.7      3     214

Percentage of the requests served within a certain time (ms)
  50%      3
  66%      3
  75%      3
  80%      3
  90%      3
  95%      3
  98%      4
  99%      5
 100%    214 (longest request)
```