Hinge. A synonym for node.

Something resembling an evented framework for Common Lisp.

Driven by [libev](http://software.schmorp.de/pkg/libev.html), like the cool kids.

## Quickstart

Clone and init the project

```sh
# Clone the repo
$ git clone https://sshirokov@github.com/sshirokov/hinge.git
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
  (run))
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
Clearing: #<EV-TIMER {1007CB5FB3}>
#<SERVER {1007B10003}> is listening!
Clearing: #<EV-TIMER {10054BF3E3}>
New client: #<SOCKET {10053E2B03}>
Clearing: #<EV-TIMER {1005752B23}>
Echoing: "Hello World.
"
Clearing: #<EV-TIMER {1005A042D3}>
Invoking callback: #<FUNCTION (LAMBDA (SOCK)) {1007E0DD2B}>
Data written to peer.
Socket drained: #<SOCKET {10053E2B03}>
Clearing: #<EV-TIMER {1005A0B1A3}>
Echoing: "Echo
"
Clearing: #<EV-TIMER {1005A0D343}>
Invoking callback: #<FUNCTION (LAMBDA (SOCK)) {1007E0DD2B}>
Data written to peer.
Socket drained: #<SOCKET {10053E2B03}>
Clearing: #<EV-TIMER {1005A244C3}>
Clearing: #<EV-TIMER {1005A24603}>
Peer: #<SOCKET {10053E2B03}> left.
```