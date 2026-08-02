open Base

module Patch_id : sig
  type t = private string
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  include Comparator.S with type t := t

  val of_string : string -> t
  val to_string : t -> string
end

module Pr_number : sig
  type t = private int
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  val of_int : int -> t
  val to_int : t -> int
end

module Session_id : sig
  type t = private string
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  val of_string : string -> t
  val to_string : t -> string
end

module Message_id : sig
  type t = private string
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  include Comparator.S with type t := t

  val of_string : string -> t
  val to_string : t -> string
end

module Effect_id : sig
  type t = private string
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  include Comparator.S with type t := t

  val of_string : string -> t
  val to_string : t -> string
end

module Branch : sig
  type t = private string
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  val of_string : string -> t
  val to_string : t -> string
end

module Operation_kind : sig
  type t =
    | Rebase
    | Human
    | Merge_conflict
    | Ci
    | Review_comments
    | Pr_body
    | Findings
        (** Review-service findings — deliveries from a backend in
            {!Review_backend} that mints its own finding store separate from
            GitHub review threads. The agent receives one [Findings_payload] per
            session and POSTs resolve verbs back to the originating backend
            after the session completes. *)
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  val to_label : t -> string
  (** Human-readable label for log messages (e.g. ["ci"], ["review-comments"]).
  *)
end

(** Wrapper for GitHub comment [databaseId]. Synthetic IDs are always negative;
    real GitHub IDs are always positive. When restoring persisted comments, call
    {!seed_synthetic_counter} before any call to {!next_synthetic} so newly
    minted IDs stay below the existing minimum. *)
module Comment_id : sig
  type t = private int
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  include Comparator.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
  val next_synthetic : unit -> t
  val seed_synthetic_counter : t list -> unit
end

module Comment : sig
  type t = {
    id : Comment_id.t;
    thread_id : string option;
    body : string;
    path : string option;
    line : int option;
    commit_sha : string option; [@yojson.default None]
    original_commit_sha : string option; [@yojson.default None]
    outdated : bool; [@yojson.default false]
    last_reply_author : string option; [@yojson.default None]
        (** Login of the last reply's author; [None] for opener-only threads.
            When it equals the viewer login, the thread's last word is onton's
            own posted reply — position disambiguates authorship even when a
            human co-reviews from the same account, because co-reviewers open
            threads and never correspond mid-thread. A re-delivered thread in
            that state only needs its resolve retried, not a duplicate reply. *)
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  include Comparator.S with type t := t
end

module Check : sig
  type t = {
    run : string;
        (** Repository-owned command whose zero exit status supplies evidence.
        *)
    proves : string;
        (** Observable claim established by a successful command. *)
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Patch : sig
  module Agent : sig
    type t = { backend : string; model : string }
    [@@deriving show, eq, sexp_of, compare, yojson]
  end

  type t = {
    id : Patch_id.t;
    goal : string;  (** Single observable outcome owned by this patch. *)
    branch : Branch.t;
    dependencies : Patch_id.t list;
    files : string list;
        (** Expected write surface. Enforcement belongs to the supervisor. *)
    checks : Check.t list;
        (** Commands that must pass before the patch can be considered done. *)
    agent : Agent.t option; [@yojson.default None]
        (** Optional complete per-patch backend/model selection. *)
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Ci_check : sig
  type t = {
    name : string;
    conclusion : string;
    details_url : string option;
    description : string option;
    started_at : string option;
    id : int option;
        (** GitHub CheckRun [databaseId] when available, [None] for legacy
            StatusContext entries (which expose no stable numeric ID). Used as
            the dedup key for CI feedback delivery so a single failing run is
            only delivered once. *)
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  val failure_conclusions : string list
  (** Conclusions that represent an actionable CI failure the agent can fix.
      Excludes ["cancelled"] — a cancelled check typically means the run was
      superseded by a newer commit or manually cancelled, not a real failure. *)

  val success_conclusions : string list
  (** Conclusions that represent a terminal successful outcome. *)

  val is_failure : t -> bool
  val is_success : t -> bool
  val merge_queue_failure : unit -> t
  val is_merge_queue_failure : t -> bool
end

module Pr_url : sig
  type t = private string
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  include Comparator.S with type t := t

  val of_string : string -> t
  val to_string : t -> string
end

module Stop_reason : sig
  type t =
    | End_turn
    | Tool_use
    | Max_tokens
    | Stop_sequence
    | Pause_turn
    | Refusal
    | Model_context_window_exceeded
  [@@deriving show, eq, sexp_of, compare]

  val of_string : string -> t option

  val to_display : t -> string
  (** Human-readable label for use in user-facing strings (e.g. activity log).
      Distinct from [show], which produces OCaml variant names. *)
end

(** Events from Claude Code's NDJSON stdout (--output-format stream-json), not
    the raw Anthropic streaming API. *)
module Stream_event : sig
  type t =
    | Turn_started
        (** Backend emitted an event indicating it accepted/started processing
            the current turn. This is stronger than session initialization:
            session IDs can be created or resumed without proving that the
            prompt was processed. *)
    | Text_delta of string
    | Tool_use of {
        name : string;
        input : string;
        status : string option;
            (** Tool-call lifecycle status from the backend stream, when
                exposed. [Some "completed"] means the tool returned a result;
                any other [Some _] (e.g. [Some "pending"], [Some "running"],
                backend-specific error states) indicates the tool was announced
                but did not finish normally. [None] for backends that do not
                surface per-tool state (all non-OpenCode backends today). *)
      }
    | Final_result of { text : string; stop_reason : Stop_reason.t }
    | Error of string
    | Session_init of {
        session_id : string;
        api_key_source : string option;
        model : string option;
        claude_code_version : string option;
        permission_mode : string option;
      }
  [@@deriving show, eq, sexp_of, compare]
end

module Gameplan : sig
  type t = {
    project_name : string;
    repo_owner : string;
    repo_name : string;
    patches : Patch.t list;
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  val slugify : string -> string
  (** Canonical project-name → branch-prefix slug used by the plan parser. *)
end
