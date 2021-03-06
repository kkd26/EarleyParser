module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type STATE = sig
  include MONAD

  type state

  val get : state t
  val put : state -> unit t
  val runState : 'a t -> init:state -> state * 'a
end

module State (S : sig
  type t
end) : STATE with type state = S.t = struct
  type state = S.t
  type 'a t = state -> state * 'a

  let return v s = (s, v)

  let ( >>= ) m k s =
    let s', a = m s in
    k a s'

  let get s = (s, s)
  let put s' _ = (s', ())
  let runState m ~init = m init
end
