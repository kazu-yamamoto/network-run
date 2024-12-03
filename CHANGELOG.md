# Revision history for network-run

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
