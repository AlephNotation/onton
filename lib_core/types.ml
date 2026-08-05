open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Patch_id = struct
  module T = struct
    type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
  end

  include T
  include Comparator.Make (T)

  let of_string s = s
  let to_string t = t
end

module Pr_number = struct
  type t = int [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  let of_int n = n
  let to_int t = t
end

module Session_id = struct
  type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  let of_string s = s
  let to_string t = t
end

module Message_id = struct
  module T = struct
    type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
  end

  include T
  include Comparator.Make (T)

  let of_string s = s
  let to_string t = t
end

module Effect_id = struct
  module T = struct
    type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
  end

  include T
  include Comparator.Make (T)

  let of_string s = s
  let to_string t = t
end

module Branch = struct
  type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  let of_string s = s
  let to_string t = t
end

module Operation_kind = struct
  type t =
    | Rebase
    | Human
    | Merge_conflict
    | Ci
    | Review_comments
    | Pr_body
    | Findings
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  let to_label = function
    | Rebase -> "rebase"
    | Human -> "human"
    | Merge_conflict -> "merge-conflict"
    | Ci -> "ci"
    | Review_comments -> "review-comments"
    | Pr_body -> "pr-body"
    | Findings -> "findings"
end

module Comment_id = struct
  module T = struct
    type t = int [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
  end

  include T
  include Comparator.Make (T)

  let of_int n = n
  let to_int t = t
  let counter = Atomic.make 0

  let next_synthetic () =
    (* Atomically claim the next synthetic ID. fetch_and_add returns the old
       counter value and decrements it, so counter always equals the last issued
       ID after each call. Subtract 1 from the old value to get the new value;
       this ensures the first call returns -1 (counter: 0 → -1, result: 0-1=-1)
       and 0 is never issued. seed_synthetic_counter relies on this: seeding to
       min_id causes the next call to return min_id-1, safely below all existing
       IDs. *)
    Atomic.fetch_and_add counter (-1) - 1

  let seed_synthetic_counter ids =
    let min_id =
      List.fold ids ~init:0 ~f:(fun acc id -> Int.min acc (to_int id))
    in
    let rec try_seed () =
      let current = Atomic.get counter in
      if min_id < current then
        if not (Atomic.compare_and_set counter current min_id) then try_seed ()
    in
    try_seed ()
end

module Comment = struct
  module T = struct
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
              own posted reply (co-reviewers open threads, they don't correspond
              mid-thread), so a re-delivery only needs the resolve retried — not
              a duplicate reply. *)
    }
    [@@deriving show, eq, sexp_of, compare, yojson]
  end

  include T
  include Comparator.Make (T)
end

module Check = struct
  type t = { run : string; proves : string }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Expansion_policy = struct
  type t = { max_patches : int; files : string list; checks : Check.t list }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Patch = struct
  module Agent = struct
    type t = { backend : string; model : string }
    [@@deriving show, eq, sexp_of, compare, yojson]
  end

  type t = {
    id : Patch_id.t;
    goal : string;
    branch : Branch.t;
    dependencies : Patch_id.t list;
    files : string list;
    checks : Check.t list;
    agent : Agent.t option; [@yojson.default None]
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Ci_check = struct
  type t = {
    name : string;
    conclusion : string;
    details_url : string option;
    description : string option;
    started_at : string option;
    id : int option; [@yojson.default None]
        (** GitHub CheckRun [databaseId] when available, [None] for legacy
            StatusContext entries (which have no stable numeric ID). Used as the
            dedup key for CI feedback delivery so a single failing run is only
            delivered once even if [generation] bumps for other reasons. *)
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  (** Conclusions that represent an actionable CI failure the agent can fix.
      Notably excludes ["cancelled"] — a cancelled check typically means the run
      was superseded by a newer commit or manually cancelled, not that anything
      actually failed. *)
  let failure_conclusions =
    [ "failure"; "error"; "action_required"; "timed_out"; "startup_failure" ]

  (** Conclusions that represent a terminal successful outcome. *)
  let success_conclusions = [ "success"; "skipped"; "neutral" ]

  let is_failure (c : t) =
    List.mem failure_conclusions c.conclusion ~equal:String.equal

  let is_success (c : t) =
    List.mem success_conclusions c.conclusion ~equal:String.equal

  let merge_queue_failure_name = "GitHub merge queue"

  let merge_queue_failure () =
    {
      name = merge_queue_failure_name;
      conclusion = "failure";
      details_url = None;
      description =
        Some
          "GitHub reported a merge queue failure for this PR. It may have been \
           removed after queue checks failed, or marked unmergeable while \
           still in the queue.";
      started_at = None;
      id = None;
    }

  let is_merge_queue_failure (c : t) =
    String.equal c.name merge_queue_failure_name && is_failure c
end

module Pr_url = struct
  module T = struct
    type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
  end

  include T
  include Comparator.Make (T)

  let of_string s = s
  let to_string t = t
end

module Stop_reason = struct
  type t =
    | End_turn
    | Tool_use
    | Max_tokens
    | Stop_sequence
    | Pause_turn
    | Refusal
    | Model_context_window_exceeded
  [@@deriving show, eq, sexp_of, compare, yojson]

  let of_string = function
    | "end_turn" -> Some End_turn
    | "tool_use" -> Some Tool_use
    | "max_tokens" -> Some Max_tokens
    | "stop_sequence" -> Some Stop_sequence
    | "pause_turn" -> Some Pause_turn
    | "refusal" -> Some Refusal
    | "model_context_window_exceeded" -> Some Model_context_window_exceeded
    | _ -> None

  let to_display = function
    | End_turn -> "ended turn"
    | Tool_use -> "awaiting tool"
    | Max_tokens -> "max tokens"
    | Stop_sequence -> "stop sequence"
    | Pause_turn -> "paused"
    | Refusal -> "refused"
    | Model_context_window_exceeded -> "context window exceeded"
end

(* Models events from Claude Code's NDJSON stdout
   (--output-format stream-json), not the raw Anthropic streaming API. *)
module Stream_event = struct
  type t =
    | Turn_started
    | Text_delta of string
    | Tool_use of { name : string; input : string; status : string option }
    | Final_result of { text : string; stop_reason : Stop_reason.t }
    | Error of string
    | Session_init of {
        session_id : string;
        api_key_source : string option; [@yojson.default None]
        model : string option; [@yojson.default None]
        claude_code_version : string option; [@yojson.default None]
        permission_mode : string option; [@yojson.default None]
      }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Gameplan = struct
  type t = {
    project_name : string;
    repo_owner : string;
    repo_name : string;
    patches : Patch.t list;
  }
  [@@deriving show, eq, sexp_of, compare, yojson]

  (* Canonical project-name → branch-prefix slug used by the plan parser. *)
  let slugify name =
    String.lowercase name
    |> String.map ~f:(fun c ->
        if Char.is_alphanum c || Char.equal c '-' || Char.equal c '_' then c
        else '-')
    |> String.split ~on:'-'
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> String.concat ~sep:"-"
end
