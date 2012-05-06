Hinge. A synonym for node.

An evented framework for Common Lisp.
Driven by [libev](http://software.schmorp.de/pkg/libev.html), like the cool kids.

## Requirements

### System level dependencies

* [libev](http://software.schmorp.de/pkg/libev.html)
* [ZeroMQ](http://www.zeromq.org/) (2.1.11+)
* [SBCL](http://www.sbcl.org/) (With thread support)

### Dependencies built outside of quicklisp

These deps are fetched and built with `make develop`

* [cl-ev](https://github.com/sbryant/cl-ev)
* [CFFI](http://common-lisp.net/project/cffi/) (Built from git, to support lisp-zmq)
* [lisp-zmq](https://github.com/galdor/lisp-zmq)

## Concepts and Principles

### Event emitters

As introduced and described in the Node.js docs. Event emitters
allow you to emit an event named by a string with some parameters
and have a chain of callbacks invoked that are expecting that
event with the parameters.

The events of hinge are sent and delivered asyncronously
so that a long chain of event subscribers will not block the event
machine execution.

### TCP Servers and Clients

Modeled after the Node.js net.* APIs, provides event emitting
classes that abstract network communication to a set of emitted
"data" events and a write scheduler that allows IO to be deferred
until the socket is ready.

### ZMQ Sockets

Wrapped around the implementations of the TCP Socket and Server classes
allow the deliver of ZeroMQ messages as emitted "data" events and allows
the handling of blocking ZeroMQ socket sends with a callback.

### Async parallel work pool(s) with calling-thread-local, event-friendly result callbacks

Allows the submission of work with bound callbacks for the successfull or
failed execution of a set of forms in a thead pool. The result or error
will be delivered asynchronously to the thread controlling the event loop
while the work will be evaluated outside of it.

## TODO

The ZeroMQ sockets currently require access to the `sock` slot to perform non-IO operations such
as setting socket options. This is not permanent.

There are some example codes in the [examples](https://github.com/sshirokov/hinge/tree/master/examples)
directory of the project root that have been used to develop the concepts they demonstrate with some
comments. They might serve to make some more sense out of how this project could be applied.

## Quickstart

Clone and init the project

```sh
# Clone the repo
$ git clone https://github.com/sshirokov/hinge.git
$ cd hinge

# Init the enviornment
$ make develop
```

Fire up the REPL and evaluate the following.
It should boot up and keep running an echo server on port 4545

```common-lisp
(ql:quickload :hinge)
(in-package :hinge)

(let ((server (make-instance 'server)))
  (add-listener server "listening"
                (lambda (server)
                  (format t "~A is listening!~%" server)))

  (add-listener server "connection"
                (lambda (peer)
                  (format t "New client: ~A~%" peer)

                  (add-listener peer "data"
                                (lambda (data)
                                  (format t "Echoing: ~S~&" (babel:octets-to-string data))
                                  (send peer data
                                        (lambda (sock)
                                          (format t "Data written to peer.~&")))))

                  (add-listener peer "close"
                                (lambda (sock)
                                  (format t "Peer: ~A left.~%" sock)))))

  (bind server 4545))

(progn
  (format t "Starting echo server at ~A~%" (get-universal-time))
  (run :default))
```

This should print some information stating the server is running and listening.
From another shell you should now be able to communicate with the echo server
by connecting to TCP port 4545 and sending data.

```sh
$ nc localhost 4545
Hello World.
Hello World. # Output
Echo
Echo # Output
```

You can watch the server react in the REPL. Somewhat like this:

```
Starting echo server at 3541190006
#<SERVER {1007B10003}> is listening!
New client: #<SOCKET {10053E2B03}>
Echoing: "Hello World.
"
Invoking callback: #<FUNCTION (LAMBDA (SOCK)) {1007E0DD2B}>
Data written to peer.
Socket drained: #<SOCKET {10053E2B03}>
Echoing: "Echo
"
Invoking callback: #<FUNCTION (LAMBDA (SOCK)) {1007E0DD2B}>
Data written to peer.
Socket drained: #<SOCKET {10053E2B03}>
Peer: #<SOCKET {10053E2B03}> left.
```