(* @archlint.module interface
   @archlint.domain backend-registry *)

(** Lazy cache of backend instances keyed by [(backend_name, model)]. *)

type t

type kind =
  | Ephemeral of Llm_backend.t
  | Long_lived of Llm_backend_long_lived.t

val create :
  process_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  timeout:float ->
  setsid_exec:string option ->
  t
(** Build an empty registry that knows how to construct any of the supported
    backends. The Eio capabilities and per-session [timeout] are baked into the
    closure so callers don't re-thread them on every [get]. *)

val get : t -> backend:string -> model:string option -> kind
(** Return the cached backend for [(backend, model)], constructing it on first
    request. Raises [Invalid_argument] for unrecognised backend names — callers
    are expected to validate against the known list before asking. *)

val resolve_model : backend:string -> model:string option -> string option
(** Supply the concrete default required by [patch-agent]. Other backends keep
    [None], allowing their provider to choose its own default. *)
