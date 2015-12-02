(* Add the "build" dependency flag to all OPAM packages that
   heuristically seem to depend on ocamlbuild, that is:
   - they have a _tags or myocamlbuild.ml
   - they have an _oasis file with "BuildTools: ocamlbuild" inside
   - or they call "ocamlbuild" in the opam build command
*)

(* This program is compiled from opam/admin-scripts (where opam is
   a git clone of upstream opam with "make" done), by the command

    ocamlfind ocamlc -package re.glob,oasis,opam-lib,opam-lib.state -linkpkg -I ../src/tools opam_admin_top.cmx add_ocamlbuild_dependency.ml -o add_ocamlbuild_dependency

   It then needs to be run from a clone of opam-repository.


   The `Detect_full action is sensibly more CPU-hungry (match for
   regex in tons of source files), so it makes sense to compile it
   natively:

    ocamlfind ocamlc -package re.glob,oasis,opam-lib,opam-lib.state -linkpkg -I ../src/tools opam_admin_top.cmx add_ocamlbuild_dependency.ml -o add_ocamlbuild_dependency

   This, however, requires building opam_admin_top.cmx, which is not
   done by opam's build system (see oppam issue #2371), so for know
   I go to ../tools and manually run

     ocamlfind ocamlopt -package re -I ../client -I ../core -I ../format -I ../repository -I ../state -c opam_admin_top.ml
*)


open Opam_admin_top

type action = [ `Download | `Detect | `Detect_full | `Reformat | `Add_dep ]
(* The script may perform several different actions:

   `Download: download all package archives to test ocamlbuild usage;
      if disabled, only packages already marked in the archive cache
      will be traversed.

   `Detect: inspect the archives to tell which packages seem to depend
     on ocamlbuild, from the archive content alone (not opam metadata).

   `Detect_full: inspect the archives content harder to tell if the
     packages has a full dependency on ocamlbuild (it distributes
     ocamlbuild plugins or executables linked with
     ocamlbuild libraries), not just a build-time dependency (it uses
     ocamlbuild as build system). Sensibly slower check.

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
  `Detect_full;
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

type ocamlbuild_dep_kind =
  | Build_dep (* only a build-time dependency (most packages) *)
  | Full_dep  (* full dependency (ocamlbuild plugin libraries) *)

let merge_dep_kind d1 d2 = match d1, d2 with
  | Full_dep, _ | _, Full_dep -> Full_dep
  | Build_dep, Build_dep -> Build_dep

let merge_dep_kind_opt o1 o2 = match o1, o2 with
  | Some d1, Some d2 -> Some (merge_dep_kind d1 d2)
  | (Some _ as o), None | None, (Some _ as o) -> o
  | None, None -> None

let oasis_ocamlbuild_re =
  (* regexp to find "BuildTools: ocamlbuild" *)
  Re.(compile (seq [bol; rep space;
                    str "BuildTools"; rep space;
                    str ":"; rep space;
                    str "ocamlbuild"; rep space; eol]))


(* To tell whether an OCaml source file depends on ocamlbuild
   libraries, we check for the presence of the module names
   Ocamlbuild_plugin or Ocamlbuild_pack. This criterion was suggested
   by Gabriel Radanne.

   A common case of false positives for ocamlbuild-lib detection is
   programs that *generate* a myocamlbuild.ml, but do not depend on
   it. They will have lines of code like:

   {|
   libres3:
     ./detect.ml:    fprintf f "open Ocamlbuild_plugin\n\n";
   mirage:
     ./lib/mirage.ml:      "open Ocamlbuild_pack;;\n\
   (note: no closing quote ")
   mirari:
     ./lib/mirari.ml:      append oc "open Ocamlbuild_pack;;";
   |}

   Of course, trying to lex string literals using regexps only would
   be deeply wrong. We use oh-so-powerful heuristics instead, that happen
   to catch all cases we found in opam-repository.
 *)
let ocamlbuild_lib_re, ocamlbuild_lib_in_string_re =
  let open Re in
  let ocamlbuild_module = seq [
    bow;
    alt [
      str "Ocamlbuild_plugin";
      str "Ocamlbuild_pack"
    ];
  ] in
  let no_quotes = rep (compl [set "\""]) in
  let quote, backslash = str "\"", str "\\" in
  let escaped_newline = str "\\n" in
  let literal_string_marker =
   (* this regexp collects string whose presence
      indicate that we are inside a string literal *)
   alt [
    escaped_newline;
   ]
 in
  let literal_string_local_end =
    alt [
      quote;
      seq [backslash; eol];
    ] in
   let known_string_literal =
     seq [
       quote; rep space;
       str "open"; rep space;
       ocamlbuild_module; rep space;
       opt (str ";;"); rep space;
       quote;
     ] in
   let unknown_string_literal =
     (*
        We use the fact that the sequence "\n" only appears in string
        literals to detect that this occurence of Ocamlbuild_* in fact
        occurs inside a string literal, and discard it.
     *)
     seq [
       ocamlbuild_module;
       no_quotes;
       literal_string_marker;
       no_quotes;
       literal_string_local_end;
     ]
   in
   compile ocamlbuild_module,
   compile (alt [ known_string_literal; unknown_string_literal ])

let source_file_re =
  Re.(compile (str ".ml"))

let ocamlbuild_file_re =
  Re.(compile (seq [alt [str "myocamlbuild";
                         str "myocamlbuild_config"];
                    str ".ml"]))

let in_build_dir_re =
  Re.(compile (str Filename.(dir_sep ^ "build" ^ dir_sep)))

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

let ocamlbuild_lib_in_source dir =
  (* This criterion was suggested by Gabriel Radanne: a package may be
     distributing Ocamlbuild plugins (and thus needs ocamlbuild as
     a full dependency, rather than only a build-time dependency) if
     it uses the module Ocamlbuild_plugin (or OCamlbuild_package) in
     a file that is *not* myocamlbuild.ml.

     We implemented the following refinements to avoid false negatives:
     - also discard myocamlbuild.ml.in, myocamlbuild_config.ml
       (coccinelle, ocaml-efl; ocaml-gdb, heptagon)
     - only look for source-looking files, that is with extension *.ml*
       (argot, bisect have .html documentation show code snippets
          of how to use argot from their ocamlbuild plugins;
        async_stmp distributes a setup-dev.exe executable
          with myocamlbuild.ml stored inside)
     - discard files in a build/ subdirectory
       (batteries has some for-local-use only ocamlbuild plugins in
        a build/ subdirectory that are not distributed to users)
     - discard occurences of those module names that seem to appear in
       string literals (see the definition of
       ocamlbuild_lib_in_string_re)
       (libres3, mirage, mirage-types, mirari)

     Known positives we consider correct (they may not actually be
     use-time dependencies, but at least it is very reasonable that
     the script detects them as such) and we did not expect
     (js_of_ocaml, eliom etc. are known to provide
     ocamlbuild plugins).
     - bap: distributes a bapbuild executable on top of ocamlbuild
       (actually this could be a build-time dependency only)
     - cppo: provides an ocamlbuild plugin
     - libres3: embeds cppo sourcecode
       (besides the string literal thing we discard)
     - mezzo: the implementation uses the ocamlbuild standard library
     - ocamlnet: embeds cppo
       (actually this could be a build-time dependency only)
     - otags: distributes an util/filter_for_tags program
       that links with ocamlbuild
       (if this is distributed in binary form only, this could
        be a build-time dependency)
     - rml (ReactiveML): they have a weird forked version of
       ocamlbuild (this confuses the OCamlbuild_package heuristic as
       they reuse the same file names), but they also distribute
       a proper ocamlbuild plugin (so we were right after all)
  *)
  let module OF = OpamFilename in
  let good_file file =
    let basename = OF.Base.to_string (OF.basename file) in
    let is_ocamlbuild_file () = Re.execp ocamlbuild_file_re basename in
    let is_source_file () = Re.execp source_file_re basename in
    let is_in_build_dir () = Re.execp in_build_dir_re (OF.to_string file) in
    not (is_ocamlbuild_file ())
    && is_source_file ()
    && not (is_in_build_dir ())
  in
  let check_file_content file str =
   (* even if we detect a use of Ocamlbuild_lib in a source file,
      this may be a false alarm if it is included in a literal string;
      re-read the file more carefully, line per line, to verify that one
      line matches ocamlbuild_lib_re but *not* ocamlbuild_lib_re_in_string

      This elaborate hack avoids trying to lex string literals using
      regexps, which is bound to fail. Because we only do the re-read on
      files that pass the first test, it's not too performance-intensive.
  *)
   let uses_ocamlbuild_lib = Re.execp ocamlbuild_lib_re str in
   let line_really_uses_ocamlbuild_lib line =
     true
     && Re.execp ocamlbuild_lib_re line
     && not (Re.execp ocamlbuild_lib_in_string_re line)
   in
   true
   && uses_ocamlbuild_lib
   && List.exists line_really_uses_ocamlbuild_lib
        (OpamProcess.read_lines (OF.to_string file))
   in
  let check_file file =
    true
    && OF.exists file
    && good_file file
    && OF.with_contents (check_file_content file) file in
  List.exists check_file (OF.rec_files dir)

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
  in
  let build_time_use =
    List.mem "_tags" files
    || List.mem "myocamlbuild.ml" files
    || List.mem "_oasis" files && ocamlbuild_in_oasis dir in
  let lib_use =
    if not (List.mem `Detect_full actions) then false
    else ocamlbuild_lib_in_source dir in
  merge_dep_kind_opt
    (if build_time_use then Some Build_dep else None)
    (if lib_use then Some Full_dep else None)

let ocamlbuild_cache : ocamlbuild_dep_kind option OpamPackage.Map.t cache = {
  file = "ocamlbuild_cache.data";
  version = 2;
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
              Printf.printf "Package %S ocamlbuild usage: %s\n%!"
                package_str
                (match ocamlbuild_usage with
                 | None -> "no"
                 | Some Build_dep -> "build dep"
                 | Some Full_dep -> "full dep");
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

let add_ocamlbuild_dep ocamlbuild_dep_kind opam0 =
  let add_ocamlbuild (deps : 'atom OpamFormula.formula) =
    let ocamlbuild = OpamPackage.Name.of_string "ocamlbuild" in
    let open OpamFormula in
    let dep_flags = match ocamlbuild_dep_kind with
      | Build_dep ->
        [OpamTypes.Depflag_Build]
      | Full_dep ->
        []
    in
    let ocamlbuild_dep =
       (* do not assume any upper bound on ocamlbuild version; note that
          the versions bundled with OCaml <= 4.02 are all considered
          "version 0". *)
      Atom (ocamlbuild, (dep_flags, Empty)) in
    (* We traverse all atoms of the formula, possibly updating
       those that are ocamlbuild dependencies:
       - if their flags match the ones we are about to set, no change is necessary
       - if their flags are weaker or identical, we replace the atom
       - if their flags is other, we preserve it and add a separate dependency
    *)
    let dependency_updated = ref false in
    let update_atom (atom : 'atom) : 'atom OpamFormula.formula =
      let (name, (flags, version_constraint)) = atom in
      let no_change = Atom atom in
      (* not an ocamlbuild atom, or weird version constraint: don't touch it *)
      if OpamPackage.Name.compare name ocamlbuild <> 0
         || version_constraint <> Empty
      then no_change
      else begin
        let same_elts li1 li2 =
          let subset li1 li2 = List.for_all (fun elt -> List.mem elt li1) li2 in
          subset li1 li2 && subset li2 li1
        in
        if flags = [] then
          (* already present with no flag: no need to change *)
          (dependency_updated := true; no_change)
        else if dep_flags = [] then
          (* we are setting a total dep: we can replace the atom *)
          (dependency_updated := true; ocamlbuild_dep)
        else if same_elts flags dep_flags then
          (* we two atoms are in fact identical; doesn't matter *)
          (dependency_updated := true; ocamlbuild_dep)
        else
          (* this ocamlbuild atom has weird flags: don't touch it *)
          no_change
      end
    in
    let updated_formula = OpamFormula.map update_atom deps in
    if !dependency_updated then updated_formula
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
          let opam_dep =
            if ocamlbuild_in_opam_metadata opam
            then Some Build_dep
            else None in
          let package_dep =
            try OpamPackage.Map.find package ocamlbuild_map
            with Not_found -> None in
          match merge_dep_kind_opt opam_dep package_dep with
          | None -> ()
          | Some dep_kind ->
            let updated_opam =
              OpamFile.OPAM.with_opam_version opam (OpamVersion.of_string "1.2")
            in
            let final_opam =
              if List.mem `Add_dep actions then
                add_ocamlbuild_dep dep_kind updated_opam
              else if List.mem `Reformat actions then
                updated_opam
              else
                opam in
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
