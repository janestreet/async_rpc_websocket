open! Core
open! Async

type 'a t =
  | Web of 'a
  | Plain_tcp
[@@deriving sexp_of]
