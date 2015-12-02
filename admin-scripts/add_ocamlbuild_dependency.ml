(* This program is compiled from opam/admin-scripts (where opam is
   a git clone of upstream opam with "make" done), by the command

    ocamlfind ocamlc -package re.glob,oasis,opam-lib,opam-lib.state -linkpkg -I ../src/tools opam_admin_top.cmo add_ocamlbuild_dependency.ml -o add_ocamlbuild_dependency

   It then needs to be run from a clone of opam-repository.
*)

(* Add the "build" dependency flag to all OPAM packages that
   heuristically seem to depend on ocamlbuild, that is:
   - they have a _tags or myocamlbuild.ml
   - they have an _oasis file with "BuildTools: ocamlbuild" inside
   - or they call "ocamlbuild" in the opam build command
*)

open Opam_admin_top

type action = [ `Download | `Detect | `Reformat | `Add_dep ]
(* The script may perform three different actions:

   `Download: download all package archives to test ocamlbuild usage;
      if disabled, only packages already marked in the archive cache
      will be traversed.

   `Detect: inspect the archives to tell which packages seem to depend
     on ocamlbuild, from the archive content alone (not opam metadata).

   `Reformat: before changing anything, it is useful to reformat the
     ./opam files of the packages we know we will change, to minimize
     the diff size of the actual change.

   `Add_dep: actually edit the ./opam files to add 'ocamlbuild' as
     a build dependency, whenever it is not already present as
     a dependency.
*)

(* edit this list to disable certain actions *)
let actions : action list = [
  `Download;
  `Detect;
  `Reformat;
  `Add_dep
]


(* Part 1: downloading the archives of all OPAM packages in parallel

   In theory this is wasteful of disk space: we could unpack the
   archive immediately after download, check ocamlbuild usage, store
   the boolean result in the cache, and delete the archive
   immediately -- working in constant disk space (assuming a fixed
   maximal bound on unpacked archive size).

   In practice, this would be very painful to use, because after each
   tweak of the script we would need to re-download the archives on
   the next run. Downloading all packages takes about an hour on my
   machine, so I don't want to do it each time. All archives take
   about 1.6Gio of disk space, which is fairly reasonable.

   The succesful download will be taken from cache on the next run,
   but failed download (or packages with a checksum mismatch) would be
   re-downloaded each time. To avoid this, we manually (de)serialize
   a cache in [archive_cache_file], which is a map of available
   archives. Packages not in the cache could not be downloaded.
*)

let in_blacklist package =
  (* I was unable to download these packages (acgtk),
     or their checksum did not match (acii85, planets),
     or their download takes a while and I know ocamlbuild
     won't be necessary (ocaml-src). *)
  let blacklist = ["acgtk"; "ascii85"; "ocaml-src"; "planets"] in
  List.mem (OpamPackage.name_to_string package) blacklist

(* cache the archive data in the current directory to skip the
   download-all-archives phase when it has been completed. *)
type 'a cache = { file : string; version : int }

let archive_cache : OpamTypes.generic_file OpamPackage.Map.t cache = {
  file = "archive_cache.data";
  version = 1;
}

let read_cache ({ file; version } : 'a cache) : 'a option =
  if not (Sys.file_exists file) then None
  else begin
    let input = open_in_bin file in
    let input_version = (input_value input : int) in
    let version_match = (input_version = version) in
    if not version_match then begin
      close_in input;
      Printf.eprintf
        "Cache %S is at version %d, but version %d was expected\n%!"
        file input_version version;
      None
    end else begin
      let cache = (input_value input : 'a) in
      close_in input;
      Some cache
    end
  end

let write_cache ({ file; version } : 'a cache) (cache : 'a) : unit =
  let output = open_out_bin file in
  output_value output (version : int);
  output_value output (cache : 'a);
  close_out output

let state =
  match Sys.getenv "OPAMROOT" with
  | exception Not_found ->
    prerr_endline
      "To run this script you must set the OPAMROOT variable \
       to explicitly indicate the OPAM root on which to operate.\n\
       Our recommendation is the following:\n\
         - clone a fresh copy of an OPAM repository, for example:\n\
         \t git@github.com:ocaml/opam-repository.git\n\
         - use this fresh copy as current directory, \
           and create a `local-root` subdirectory\n\
         - initialize `local-root` as an OPAM root \
           on top of this cloned repository, by running:\n\
         \t OPAMROOT=$(pwd)/local-root opam init $(pwd) \n\
         - run the script with OPAMROOT=$(pwd)/local-root set\n\
       ";
      failwith "OPAMROOT not set";
  | root ->
    OpamFormatConfig.init ();
    assert (OpamStateConfig.load_defaults (OpamFilename.Dir.of_string root));
    OpamStd.Config.init ();
    OpamStateConfig.init ();
    OpamState.load_state ~save_cache:true "add_ocamlbuild_dep"
      OpamSwitch.system

let archives =
  let archives = ref
      (match read_cache archive_cache with
       | Some archives -> archives
       | None -> OpamPackage.Map.empty) in
    if List.mem `Download actions then begin
      print_endline "DOWNLOAD";
      let open OpamProcess.Job.Op in
      let repo_index = OpamState.package_state state in
      OpamParallel.iter
        ~jobs:20 (* download are I/O driven, lots of job is fine *)
        ~command:(fun package ->
            if OpamPackage.Map.mem package !archives
            || in_blacklist package then Done ()
            else begin
              let package_str =
                Printf.sprintf "%s.%s"
                  (OpamPackage.name_to_string package)
                  (OpamPackage.version_to_string package) in
              OpamAction.download_package state package @@+ function
              | `Error err ->
                Printf.printf "failed to download %S: %s\n%!" package_str err;
                Done ()
              | `Successful None ->
                Printf.printf "no archive for %S\n%!" package_str;
                Done ()
              | `Successful (Some archive) ->
                archives := OpamPackage.Map.add package archive !archives;
                Done ()
            end)
        (OpamPackage.Map.keys repo_index);
      write_cache archive_cache !archives;
    end;
    !archives


(* Part 2: checking for ocamlbuild usage *)

let oasis_ocamlbuild_re =
  (* regexp to find "BuildTools: ocamlbuild" *)
  Re.(compile (seq [bol; rep space;
                    str "BuildTools"; rep space;
                    str ":"; rep space;
                    str "ocamlbuild"; rep space; eol]))

let ocamlbuild_in_oasis dir =
  (* is ocamlbuild mentioned in the _oasis file? *)
  let module OF = OpamFilename in
  let oasis_files =
    List.filter
      (fun file -> OF.Base.to_string (OF.basename file) = "_oasis")
      (OF.rec_files dir)
  in
  let uses_ocamlbuild oasis_file =
    let uses_ocamlbuild oasis_str =
      let uses = Re.execp oasis_ocamlbuild_re oasis_str in
      Printf.printf "\t%s: %B\n%!" (OF.prettify oasis_file) uses;
      uses
    in OpamFilename.with_contents uses_ocamlbuild oasis_file
  in
  List.exists uses_ocamlbuild oasis_files

let detect_ocamlbuild_use package dir =
  (* We test for _tags or myocamlbuild.ml or _oasis files even in
     depth ([rec_files] and not just [files]), so that it works below
     a src/ sub-directory for example. This means that we may detect
     an ocamlbuild dependency to projects that only need ocamlbuild in
     not-required-at-build-time sub-directories such as documentation
     or tests.*)
  let files =
    OpamFilename.rec_files dir
    |> List.map OpamFilename.basename
    |> List.map OpamFilename.Base.to_string
  in List.mem "_tags" files
  || List.mem "myocamlbuild.ml" files
  || List.mem "_oasis" files && ocamlbuild_in_oasis dir

let ocamlbuild_cache : bool OpamPackage.Map.t cache = {
  file = "ocamlbuild_cache.data";
  version = 1;
}

let ocamlbuild_map =
  let cached_map =
    match read_cache ocamlbuild_cache with
    | Some map -> map
    | None -> OpamPackage.Map.empty in
  if not (List.mem `Detect actions) then OpamPackage.Map.empty
  else begin
    print_endline "DETECT OCAMLBUILD";
    let map = OpamPackage.Map.mapi (fun package archive ->
        match OpamPackage.Map.find package cached_map with
        | result -> result
        | exception Not_found ->
          OpamFilename.with_tmp_dir (fun dir ->
              let archive_dir = OpamFilename.Op.(dir / "archive") in
              OpamFilename.extract_generic_file archive archive_dir;
              let ocamlbuild_usage =
                detect_ocamlbuild_use package archive_dir in
              let package_str =
                Printf.sprintf "%s.%s"
                  (OpamPackage.name_to_string package)
                  (OpamPackage.version_to_string package) in
              Printf.printf "Package %S ocamlbuild usage: %B\n%!"
                package_str ocamlbuild_usage;
              ocamlbuild_usage)
      ) archives in
    write_cache ocamlbuild_cache map;
    map
  end

(* Part 3: opam-repository metadata update

   The metadata-update functions provided by the OPAM library have
   a tendency to change the ordering of `opam` fields, or the
   line-wrapping choices made by the authors, or the amount of
   brackets used in the file. This means that diffs between the
   before- and after- opam files may be sensibly larger than
   necessary.

   To alleviate this issue, we provide two separate actions on
   ocamlbuild-detected files, a `Reformat action that only rewrites
   the file as pretty-printed by OPAM, and an `Add_dep action that
   actually adds the dependency. This allows to do the change in two
   step, with a large but semantics-preserving first patch, and a more
   readable diff for the actual dependency-introducing second patch.
*)

let ocamlbuild_in_opam_metadata opam =
  let ocamlbuild_in_command (command, _filters) =
    match command with
      | [] -> false
      | (head, _filters) :: _args ->
        begin match head with
          | OpamTypes.CString "ocamlbuild"
          | OpamTypes.CIdent "ocamlbuild" -> true
          | _ -> false
        end
  in List.exists ocamlbuild_in_command (OpamFile.OPAM.build opam)

let add_ocamlbuild_dep opam0 =
  let add_ocamlbuild deps =
    let ocamlbuild = OpamPackage.Name.of_string "ocamlbuild" in
    let open OpamFormula in
    let ocamlbuild_dep =
      Atom
        (ocamlbuild,
         (
           (* We only add a *build-time* dependency when ocamlbuild usage
              is detected.  Some people may depend on ocamlbuild at other
              time (to link the ocamlbuild library modules in their
              program, for example), they will have to extend the
              dependency scope manually. *)
           [OpamTypes.Depflag_Build],

           (* do not assume any upper bound on ocamlbuild version; note that
              the versions bundled with OCaml <= 4.02 are all considered
              "version 0".  *)
           Empty)) in
    let is_ocamlbuild (name, _versions) =
      OpamPackage.Name.compare name ocamlbuild = 0 in
    let has_ocamlbuild_dep deps =
      List.exists is_ocamlbuild
        (atoms (formula_of_extended (fun _ -> true) deps)) in
    (* do not add an ocamlbuild dependency if one already exists *)
    if has_ocamlbuild_dep deps then deps
    else And(deps, ocamlbuild_dep)
  in
  let open OpamFile.OPAM in
  let opam = with_depends opam0 (add_ocamlbuild (depends opam0)) in
  if opam = opam0 then opam0
  else with_opam_version opam (OpamVersion.of_string "1.2")

let () =
  if List.mem `Reformat actions || List.mem `Add_dep actions then begin
    print_endline "UPDATE REPOSITORY";
    iter_packages
      ~f:(fun package prefix opam ->
          match
            (OpamPackage.Map.find package ocamlbuild_map
            || ocamlbuild_in_opam_metadata opam)
          with
          | exception Not_found -> ()
          | false -> ()
          | true ->
            let updated_opam =
              OpamFile.OPAM.with_opam_version opam (OpamVersion.of_string "1.2")
            in
            let final_opam =
              if List.mem `Add_dep actions then add_ocamlbuild_dep updated_opam
              else if List.mem `Reformat actions then updated_opam
              else opam in
            let repo = Opam_admin_top.repo in
            let opam_file = OpamRepositoryPath.opam repo prefix package in
            if List.mem `Reformat actions then begin
              (* this very ugly hack avoids a shortcoming of the
                 formatting-preserving printer, which currently only
                 works for fields that have the exact same name in the
                 parsed and printed version. The field "author" is
                 a legacy name that is supported in parsing, but
                 reprinted as "authors", and its formatting would thus
                 not be preserved. *)
              let rename input output =
                let cmd =
                  Printf.sprintf
                    "sed \"s/%s/%s/\" %s --in-place"
                    input output (OpamFilename.to_string opam_file) in
                ignore (Sys.command cmd) in
              (* if "author:" is followed by more than two spaces,
                 this is a column-aligned version so we remove one space *)
              rename "^author:  " "authors: ";
              rename "^author:" "authors:";
            end;
            let printed_opam =
              OpamFile.OPAM.to_string_with_preserved_format opam_file final_opam in
            OpamFilename.write opam_file printed_opam)
      ()
  end
