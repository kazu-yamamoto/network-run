# Revision history for network-run

## 0.7.1

* Sockets this library owns and has not handed to anyone are closed with
  `close(2)` rather than with `close`, which goes through the IO manager
  to tell a waiter that is not there.  Requires network 3.2.11.

## 0.7.0

* The accept loop is stopped by being told, through the new
  `settingsInstallShutdownHandler`, rather than by having the listening
  socket closed under it.  It accepts with `acceptWith`, which gives up
  on the accept itself, so nothing has to be closed or interrupted.  A
  closed socket still stops the loop, now by returning rather than by
  throwing.
* The UDP server's receive loop is stopped the same way, through
  `recvFromWith`, rather than by an asynchronous exception.
* Breaking change: `ServerSettings` gains `settingsInstallShutdownHandler`.
* Breaking change: the TCP servers return `IO ()` rather than `IO a`,
  since they can now return.
* `Listener` and `makeListener` are exported for servers with an accept
  loop of their own.
* Which errno `accept` may be retried on comes from network's
  `isTransientAcceptError` rather than from a list here, and this one
  was the shorter of the two: `ECONNABORTED` and `EINTR` only, where
  accept(2) names six more.
* Requires network 3.2.10 for `acceptWith`.

## 0.6.0

* Fixing a bug that the `[(SocketOption, Int)]` variants
  (`openClientSocketWithOptions`, `openServerSocketWithOptions` and
  `openTCPServerSocketWithOptions`) passed an `Int`, that is eight
  bytes, to `setsockopt`. BSD rejects this with `EINVAL`, so the
  documented `(IPv6Only, 0)` for a dual stack socket did not work
  there. The value is converted to `CInt` now.
* A test suite.
* Breaking change: Network.Run.TCP.Timeout no longer exports
  `openServerSocket`, `openServerSocketWithOptions` and
  `openServerSocketWithOpts`. They do not call `listen`, so
  `runTCPServerWithSocket` cannot accept on the resulting socket.
  Use `openTCPServerSocket` and friends instead.
* New API: `ServerSettings`, `defaultServerSettings` and the
  `runTCPServerWithSettings`, `runTCPServerWithSocketAndSettings` and
  `runUDPServerForkWithSettings` variants. `settingsOnException`
  receives exceptions which the library catches instead of propagating,
  `settingsGracefulCloseTimeout` controls `gracefulClose` and
  `settingsAcceptRetryDelay` controls the `accept` retry interval.
* New API: Network.Run.TCP.Timeout now exports `resolve`,
  `openTCPServerSocket`, `openTCPServerSocketWithOptions` and
  `openTCPServerSocketWithOpts`.
* `accept` no longer terminates the server on transient errors.
  `ECONNABORTED` and `EINTR` are retried immediately, and
  `EMFILE`/`ENFILE` are retried after a short delay and passed to
  `settingsOnException`.
* `runUDPServerFork` is now exception safe. A failure of `getAddrInfo`,
  `openServerSocket` or `connect` no longer leaks a socket nor kills
  the server; such a datagram is dropped and passed to
  `settingsOnException` instead. An unknown address family is dropped
  rather than calling `error`.
* Fixing a bug that `runUDPServerFork` labels every forked thread with
  the first host name.
* Documenting IPV6_V6ONLY and the single address family of
  `runTCPServer`.
* Network.Run.UDP exports `openServerSocket`,
  `openServerSocketWithOptions` and `openServerSocketWithOpts`.

## 0.5.0

* Fixing a bug that TimeoutServer is not killed.
* Breaking change: the signatures of Timeout.runTCPServer and
  Timeout.runTCPServerWithSocket are changed.

## 0.4.3

* Using time-manager >= 0.2.

## 0.4.2

* Using `withHandle` of time-manager.

## 0.4.1

* Make sure to cancel Handles.
  [#13](https://github.com/kazu-yamamoto/network-run/pull/13)
* New API: `openClientSocketWithOpts`, `openServerSocketWithOpts`
  and `openTCPServerSocketWithOpts`.
  [#12](https://github.com/kazu-yamamoto/network-run/pull/12)

## 0.4.0

* New API: `openTCPServerSocket`, `runTCPClientWithSettings`, etc.
* Breaking change: runTCPServerSocket takes a socket itself

## 0.3.2

* Add `openServerSocketWithOptions`, `openClientSocketWithOptions`,
  `runTCPServerWithSocketOptions`, `runTCPClientWithSocketOptions`.
  [#6](https://github.com/kazu-yamamoto/network-run/pull/6)

## 0.3.1

* Using close instead of gracefulClose for client
  [#5](https://github.com/kazu-yamamoto/network-run/pull/5)

## 0.3.0

* Specifying IPv6Only
  [#4](https://github.com/kazu-yamamoto/network-run/pull/4)

## 0.2.8

* runTCPClient specifies AI_ADDRCONFIG.

## 0.2.7

* Introduce `runTCPServerWithSocket`
  [#3](https://github.com/kazu-yamamoto/network-run/pull/3)

## 0.2.6

* Adding the Network.Run.TCP.Timeout module.

## 0.2.5

* Making accept breakable on windows
  [#2](https://github.com/kazu-yamamoto/network-run/pull/2)
