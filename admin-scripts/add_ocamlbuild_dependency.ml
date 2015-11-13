(* This program is compiled from opam/admin-scripts (where opam is
   a git clone of upstream opam with "make" done), by the command

    ocamlfind ocamlc -package oasis,opam-lib,opam-lib.state -linkpkg -I ../src/tools opam_admin_top.cmo add_ocamlbuild_dependency.ml -o add_ocamlbuild_dependency

   It then needs to be run from a clone of opam-repository.
*)

(* Add the "build" dependency flag to all OPAM packages that
   heuristically seem to depend on ocamlbuild, that is:
   - they have a _tags or myocamlbuild.ml
   - they have an _oasis file with "BuildTools: ocamlbuild" inside
*)

open Opam_admin_top

(* Part 1: downloading the archives of all OPAM packages *)

let in_blacklist package =
  (* I was unable to download these packages (acgtk),
     or their checksum did not match (acii85, planets),
     or their download takes a while and I know ocamlbuild
     won't be necessary (ocaml-src). *)
  let blacklist = ["acgtk"; "ascii85"; "ocaml-src"; "planets"] in
  List.mem (OpamPackage.name_to_string package) blacklist

(* cache the archive data in the current directory to skip the
   download-all-archives phase when it has been completed. *)
let archive_cache_file = "archive_cache.data"
let archive_cache_version = 2

let archive_cache =
  if not (Sys.file_exists archive_cache_file) then None
  else begin
    let input = open_in_bin archive_cache_file in
    let table_version = (input_value input : int) in
    let version_match = (archive_cache_version = table_version) in
    if not version_match then begin
      close_in input;
      Printf.eprintf
        "Archive cache %S is at version %d,\
         while version %d was expected\n%!"
        archive_cache_file table_version archive_cache_version;
      None
    end else begin
      let table =
        (input_value input : OpamTypes.generic_file OpamPackage.Map.t) in
      close_in input;
      Some table
    end
  end

(* First, we download the archives of all packages in parallel.

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
let archives =
  match archive_cache with
  | Some archives -> archives
  | None ->
    let archives = ref OpamPackage.Map.empty in

    let state =
      (* I need an OpamState.t value to pass to
         OpamAction.download_package; I scraped this code from the
         intimidating couverture.ml codebase, and I don't know what it is
         doing (where is the cache data stored?), but testing seems to say
         it works. *)
      let root = OpamStateConfig.opamroot () in
      OpamFormatConfig.init ();
      if not (OpamStateConfig.load_defaults root) then
        failwith "Opam root not found";
      OpamStd.Config.init ();
      OpamSolverConfig.init ();
      OpamStateConfig.init ();
      OpamState.load_state ~save_cache:true "add_ocamlbuild_dep"
        OpamStateConfig.(!r.current_switch)
    in

    let () =
      print_endline "DOWNLOAD";
      let open OpamProcess.Job.Op in
      let repo_index = OpamState.package_state state in
      OpamParallel.iter
        ~jobs:20 (* download are I/O driven, lots of job is fine *)
        ~command:(fun package ->
            let package_str =
              Printf.sprintf "%s.%s"
                (OpamPackage.name_to_string package)
                (OpamPackage.version_to_string package) in
            if in_blacklist package then Done ()
            else begin
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
        (OpamPackage.Map.keys repo_index)
    in

    (* after the download are done, write the archive cache *)
    let output = open_out_bin archive_cache_file in
    output_value output archive_cache_version;
    output_value output !archives;
    close_out output;

    !archives

(* Part 2: checking for ocamlbuild usage and metadata update *)

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

let detect_ocamlbuild_use package dir opam =
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
  in List.mem "_tag" files
  || List.mem "myocamlbuild.ml" files
  || List.mem "_oasis" files && ocamlbuild_in_oasis dir

let ocamlbuild_dep : OpamTypes.ext_formula =
  OpamFormula.Atom
    (OpamPackage.Name.of_string "ocamlbuild",
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
      OpamFormula.Empty))

let add_ocamlbuild_dep opam0 =
  let open OpamFile.OPAM in
  let opam = with_depends opam0
      (OpamFormula.And(ocamlbuild_dep, depends opam0)) in
  if opam = opam0 then opam0
  else with_opam_version opam @@ OpamVersion.of_string "1.2"


(* Second, we update the opam-repository metadata.

   We iterate over all packages in the opam-repository rooted at the
   script invocation's working directory, extract the corresponding
   archive, checking for ocamlbuild usage, and updating the OPAM
   metadata if necessary.

   The metadata-update functions provided by the OPAM library have
   a tendency to change the ordering of `opam` fields, or the
   line-wrapping choices made by the authors, or the amount of
   brackets used in the file. This means that diffs between the
   before- and after- opam files may be sensibly larger than
   necessary.
*)
let () =
  print_endline "ARCHIVE INSPECTION";
  iter_packages
    ~opam:(fun package opam ->
        let package_str =
          Printf.sprintf "%s.%s"
            (OpamPackage.name_to_string package)
            (OpamPackage.version_to_string package) in
        match OpamPackage.Map.find package archives with
        | exception Not_found -> opam
        | archive ->
          let ocamlbuild_use =
            OpamFilename.with_tmp_dir (fun dir ->
                let archive_dir = OpamFilename.Op.(dir / "archive") in
                OpamFilename.extract_generic_file archive archive_dir;
                detect_ocamlbuild_use package archive_dir opam) in
          Printf.printf "Package %S ocamlbuild usage: %B\n%!"
            package_str ocamlbuild_use;
          if not ocamlbuild_use then opam
          else add_ocamlbuild_dep opam)
    ()
