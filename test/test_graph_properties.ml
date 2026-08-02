open Base
open Onton_core

let id index = Types.Patch_id.of_string (Printf.sprintf "p%d" index)

let patches_of_edges count edges =
  List.init count ~f:(fun index ->
      let dependencies =
        List.filter_map edges ~f:(fun (from_index, to_index) ->
            if from_index = index && to_index < index then Some (id to_index)
            else None)
      in
      {
        Types.Patch.id = id index;
        branch = Types.Branch.of_string (Printf.sprintf "onton/p%d" index);
        goal = Printf.sprintf "complete patch %d" index;
        dependencies;
        files = [ Printf.sprintf "lib/p%d.ml" index ];
        checks = [ { Types.Check.run = "dune build"; proves = "build passes" } ];
      })

let graph_case =
  QCheck2.Gen.(
    let* count = int_range 1 10 in
    let* edges =
      list_size (int_range 0 40)
        (pair (int_range 0 (count - 1)) (int_range 0 (count - 1)))
    in
    let patches = patches_of_edges count edges in
    return (patches, Graph.of_patches patches))

let ids_are_exactly_the_plan =
  QCheck2.Test.make ~name:"graph node set is exactly the plan" ~count:500
    graph_case (fun (patches, graph) ->
      List.equal Types.Patch_id.equal
        (List.map patches ~f:(fun (patch : Types.Patch.t) ->
             patch.Types.Patch.id)
        |> List.sort ~compare:Types.Patch_id.compare)
        (Graph.all_patch_ids graph |> List.sort ~compare:Types.Patch_id.compare))

let dependencies_are_deduplicated =
  QCheck2.Test.make ~name:"direct dependencies are deduplicated" ~count:500
    graph_case (fun (patches, graph) ->
      List.for_all patches ~f:(fun (patch : Types.Patch.t) ->
          let expected =
            List.dedup_and_sort patch.Types.Patch.dependencies
              ~compare:Types.Patch_id.compare
          in
          List.equal Types.Patch_id.equal expected
            (Graph.deps graph patch.Types.Patch.id)))

let depends_on_matches_direct_dependencies =
  QCheck2.Test.make ~name:"depends_on is direct dependency membership"
    ~count:500 graph_case (fun (_patches, graph) ->
      List.for_all (Graph.all_patch_ids graph) ~f:(fun patch_id ->
          List.for_all (Graph.all_patch_ids graph) ~f:(fun dependency ->
              Bool.equal
                (Graph.depends_on graph patch_id ~dep:dependency)
                (List.mem
                   (Graph.deps graph patch_id)
                   dependency ~equal:Types.Patch_id.equal))))

let reverse_edges_are_consistent =
  QCheck2.Test.make ~name:"dependents are the inverse of dependencies"
    ~count:500 graph_case (fun (_patches, graph) ->
      List.for_all (Graph.all_patch_ids graph) ~f:(fun patch_id ->
          List.for_all (Graph.deps graph patch_id) ~f:(fun dependency ->
              List.mem
                (Graph.dependents graph dependency)
                patch_id ~equal:Types.Patch_id.equal)))

let ancestors_are_transitive =
  QCheck2.Test.make ~name:"transitive ancestors contain every direct dependency"
    ~count:500 graph_case (fun (_patches, graph) ->
      List.for_all (Graph.all_patch_ids graph) ~f:(fun patch_id ->
          let ancestors = Graph.transitive_ancestors graph patch_id in
          (not (List.mem ancestors patch_id ~equal:Types.Patch_id.equal))
          && List.for_all (Graph.deps graph patch_id) ~f:(fun dependency ->
              List.mem ancestors dependency ~equal:Types.Patch_id.equal)))

let dependency_gate_matches_definition =
  QCheck2.Test.make ~name:"dependency gate allows at most one open dependency"
    ~count:500 graph_case (fun (_patches, graph) ->
      let has_merged patch_id =
        Int.rem (String.hash (Types.Patch_id.to_string patch_id)) 2 = 0
      in
      let has_pr patch_id =
        Int.rem (String.hash (Types.Patch_id.to_string patch_id)) 3 <> 0
      in
      List.for_all (Graph.all_patch_ids graph) ~f:(fun patch_id ->
          let dependencies = Graph.deps graph patch_id in
          let open_count =
            List.count dependencies ~f:(fun dependency ->
                not (has_merged dependency))
          in
          Bool.equal
            (Graph.deps_satisfied graph patch_id ~has_merged ~has_pr)
            (open_count <= 1
            && List.for_all dependencies ~f:(fun dependency ->
                has_merged dependency || has_pr dependency))))

let initial_base_tracks_the_only_open_dependency =
  QCheck2.Test.make
    ~name:"initial base is main or the sole open dependency branch" ~count:500
    graph_case (fun (_patches, graph) ->
      let main = Types.Branch.of_string "main" in
      let branch_of patch_id =
        Types.Branch.of_string (Types.Patch_id.to_string patch_id)
      in
      List.for_all (Graph.all_patch_ids graph) ~f:(fun patch_id ->
          let open_dependencies =
            Graph.open_pr_deps graph patch_id ~has_merged:(fun _ -> false)
          in
          match open_dependencies with
          | [] ->
              Types.Branch.equal
                (Graph.initial_base graph patch_id
                   ~has_merged:(fun _ -> false)
                   ~branch_of ~main)
                main
          | [ dependency ] ->
              Types.Patch_id.equal
                (Graph.sole_open_dep graph patch_id ~has_merged:(fun _ -> false))
                dependency
              && Types.Branch.equal
                   (Graph.initial_base graph patch_id
                      ~has_merged:(fun _ -> false)
                      ~branch_of ~main)
                   (branch_of dependency)
          | _ -> true))

let () =
  List.iter
    [
      ids_are_exactly_the_plan;
      dependencies_are_deduplicated;
      depends_on_matches_direct_dependencies;
      reverse_edges_are_consistent;
      ancestors_are_transitive;
      dependency_gate_matches_definition;
      initial_base_tracks_the_only_open_dependency;
    ] ~f:(fun property -> QCheck2.Test.check_exn property)
