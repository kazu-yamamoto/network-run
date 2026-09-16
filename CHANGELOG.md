# Revision history for network-run

## 0.6.0

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
