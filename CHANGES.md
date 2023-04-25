## Release v0.16.0

- Refactored the interface of the `Rpc` module to make less frequent use of
  function type aliases for sharing code. The result is that each function
  signature is larger, but easier to fully understand. Any users of the
  function type aliases will need to update their code, since those aliases
  were removed.

- Added the `Rpc.Transport` module. The interface for this module is similar to
  that of `Rpc`, except that callbacks are given a transport instead of a
  pre-constructed connection.
