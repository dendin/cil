module E = Errormsg
module D = Dynlink
module F = Findlib

(** Findlib magic *)

let init () = F.init ()

let findlib_lookup s =
  E.s (E.error "could not find plugin %s" s)

(** Find a plugin: file path, then relative to standard library, then using
 * findlib. Return a list of paths, including potential dependencies found by
 * findlib. *)

let stdlib_rel s = 
  if s.[0] = '+' then
    Printf.sprintf "%s/%s"
      (F.ocaml_stdlib ())
      (String.sub s 1 (String.length s - 1))
  else s

let find_plugin s =
  if s = "" then E.s (E.error "missing plugin name") else
  if Sys.file_exists s then [s] else
  if Sys.file_exists (stdlib_rel s) then [stdlib_rel s] else
  findlib_lookup s

(** List of plugins to load *)

let plugins = ref []

(** Add a single plugin, except if we have added it already *)
let add_plugin path =
  if not (List.mem path !plugins) then
  plugins := path :: !plugins

(** Look for plugin and depencies and add them *)
let add_plugin_deep s =
  let paths = find_plugin s in
  List.iter add_plugin paths


(** Parse only --load plugin, ignoring every error raised by other, unparsed
 * options. Return the list of plugins to load. *)
let parse argv =
  let spec = [
    "--load", Arg.String add_plugin_deep, "";
    (* ignore --help at this stage *)
    "--help", Arg.Unit ignore, ""; "-help", Arg.Unit ignore, "" ] in
  let idx = ref 0 in
  let rec aux () =
    try
      Arg.parse_argv ~current:idx argv spec ignore "";
      List.rev !plugins
    with Arg.Bad _ | Arg.Help _ -> incr idx; aux ()
  in aux ()

(** Dynamic linking *)

let load s =
  try
    D.allow_unsafe_modules true;
    D.loadfile s
  with D.Error e -> E.s (E.error "%s" (D.error_message e))

