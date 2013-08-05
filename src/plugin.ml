module E = Errormsg

let load s =
  try
    (* Dynlink.allow_unsafe_modules true; *)
    E.log "Loading plugin %s\n" s;
    Dynlink.loadfile (Dynlink.adapt_filename s)
  with Dynlink.Error e -> E.s (E.error "%s" (Dynlink.error_message e))

let plugins = ref []
let add_plugin s = plugins := s :: !plugins
let spec = [
  "--load", Arg.String add_plugin, "";
  (* ignore --help at this stage *)
  "--help", Arg.Unit ignore, ""; "-help", Arg.Unit ignore, ""
]

(** Parse only --load plugin, ignoring every error (raised by other, unparsed
 * options) *)
let parse argv =
  let idx = ref 0 in
  let rec aux () =
    try
      Arg.parse_argv ~current:idx argv spec ignore "";
    List.rev !plugins
  with Arg.Bad _ | Arg.Help _ -> incr idx; aux ()
  in aux()
