(* @archlint.module test
   @archlint.domain worker-sandbox *)

open Base
open Onton_core

let require_ok = function
  | Ok value -> value
  | Error message -> failwith message

let make_policy () =
  Worker_sandbox_policy.create ~worktree:"/worktree"
    ~read_only_paths:[ "/context/plan.md" ] ~read_only_dirs:[ "/context/ci" ]
    ~writable_files:[ "/worktree/lib/owned.ml" ]
    ~writable_dirs:[ "/outputs/comments" ]
    ~creatable_dirs:[ "/worktree/lib/new" ]
    ~runtime_files:[ "/runtime/backend" ] ~runtime_roots:[ "/runtime/package" ]
    ~state_dir:"/state" ~network:Worker_sandbox_policy.Https_only
  |> require_ok

let env_value name env =
  Array.find_map env ~f:(fun entry ->
      match String.lsplit2 entry ~on:'=' with
      | Some (key, value) when String.equal key name -> Some value
      | Some _ | None -> None)

let () =
  let policy = make_policy () in
  let profile = Worker_sandbox_policy.macos_profile policy in
  assert (String.is_substring profile ~substring:"(deny default)");
  assert (
    String.is_substring profile ~substring:"(allow process-exec process-fork)");
  assert (not (String.is_substring profile ~substring:"process-signal"));
  assert (String.is_substring profile ~substring:"/worktree/lib/owned.ml");
  assert (String.is_substring profile ~substring:"/worktree/lib/new");
  assert (String.is_substring profile ~substring:"/runtime/backend");
  assert (String.is_substring profile ~substring:"/context/plan.md");
  assert (
    String.is_substring profile
      ~substring:"(literal \"/private/var/run/mDNSResponder\")");
  assert (String.is_substring profile ~substring:"/etc/ssl/openssl.cnf");
  assert (not (String.is_substring profile ~substring:"*:53"));
  assert (not (String.is_substring profile ~substring:"GITHUB_TOKEN"));
  let environment =
    Worker_sandbox_policy.environment
      ~allowed_provider_names:[ "OPENAI_API_KEY" ]
      ~base:
        [|
          "PATH=/usr/bin:/bin";
          "OPENAI_API_KEY=provider-secret";
          "ANTHROPIC_API_KEY=other-provider-secret";
          "GITHUB_TOKEN=forge-secret";
          "GH_TOKEN=forge-secret-two";
          "SSH_AUTH_SOCK=/private/tmp/agent.sock";
          "AWS_SECRET_ACCESS_KEY=cloud-secret";
          "ONTON_CONTROLLER_STATE=/controller";
          "CLAUDE_CONFIG_DIR=/ambient/claude";
        |]
      ~overrides:
        [
          ("PATH", "/usr/bin:/bin");
          ("HOME", "/state/home");
          ("TMPDIR", "/state/tmp");
          ("SSL_CERT_FILE", "/etc/ssl/cert.pem");
        ]
    |> require_ok
  in
  assert (
    Option.equal String.equal
      (env_value "PATH" environment)
      (Some "/usr/bin:/bin"));
  assert (
    Option.equal String.equal
      (env_value "OPENAI_API_KEY" environment)
      (Some "provider-secret"));
  assert (
    Option.equal String.equal
      (env_value "SSL_CERT_FILE" environment)
      (Some "/etc/ssl/cert.pem"));
  List.iter
    [
      "GITHUB_TOKEN";
      "GH_TOKEN";
      "SSH_AUTH_SOCK";
      "AWS_SECRET_ACCESS_KEY";
      "ONTON_CONTROLLER_STATE";
      "CLAUDE_CONFIG_DIR";
    ] ~f:(fun name -> assert (Option.is_none (env_value name environment)));
  assert (Option.is_none (env_value "ANTHROPIC_API_KEY" environment));
  assert (
    Result.is_error
      (Worker_sandbox_policy.environment ~allowed_provider_names:[] ~base:[||]
         ~overrides:[ ("GITHUB_TOKEN", "forbidden") ]));
  assert (
    Result.is_error
      (Worker_sandbox_policy.create ~worktree:"/safe/../escape"
         ~read_only_paths:[] ~read_only_dirs:[] ~writable_files:[]
         ~writable_dirs:[] ~creatable_dirs:[] ~runtime_files:[]
         ~runtime_roots:[] ~state_dir:"/state"
         ~network:Worker_sandbox_policy.Denied))
