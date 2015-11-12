#!/usr/bin/env opam-admin.top
#directory "+../opam-lib";;
(* Add the "build" dependency flag to all OPAM packages that
   heuristically seem to depend on ocamlbuild *)

open Opam_admin_top;;


let state =
  (* I need an OpamState.t value to pass to OpamAction.download_package;
     I scraped this code from the intimidating couverture.ml codebase,
     and I don't know what it is doing (where is the cache data
     stored?), but testing seems to say it works. *)
  let root = OpamStateConfig.opamroot () in
  OpamFormatConfig.init ();
  if not (OpamStateConfig.load_defaults root) then
    failwith "Opam root not found";
  OpamStd.Config.init ();
  OpamSolverConfig.init ();
  OpamStateConfig.init ();
  OpamState.load_state ~save_cache:true "add_ocamlbuild_dep"
    OpamStateConfig.(!r.current_switch)
;;

let detect_ocamlbuild_use package dir opam =
  let files =
    OpamFilename.rec_files dir
    |> List.map OpamFilename.basename
    |> List.map OpamFilename.Base.to_string
  in
  List.mem "_tag" files
  || List.mem "myocamlbuild.ml" files
;;

let () =
  iter_packages
    ~opam:(fun package opam ->
        print_newline ();
        let package_name =
          Printf.sprintf "%s.%s"
            (OpamPackage.name_to_string package)
            (OpamPackage.version_to_string package) in
        let download = OpamAction.download_package state package in
        match OpamProcess.Job.run download with
        | `Error err ->
          Printf.printf "failed to download %S: %s\n@!" package_name err;
          opam (* continue iterating *)
        | `Successful None ->
          Printf.printf "no archive for %S\n@!" package_name;
          opam
        | `Successful (Some archive) ->
          let ocamlbuild_use =
            OpamFilename.with_tmp_dir (fun dir ->
                let archive_dir = OpamFilename.Op.(dir / "archive") in
                OpamFilename.extract_generic_file archive archive_dir;
                detect_ocamlbuild_use package archive_dir opam) in
          Printf.printf "Package %S ocamlbuild usage: %B\n@!"
            package_name ocamlbuild_use;
          opam)
    ()
;;
