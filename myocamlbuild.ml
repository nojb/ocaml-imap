open Ocamlbuild_plugin
  
let _ =
  dispatch begin
    function
      After_rules ->
        Pathname.define_context "src/unix" ["src/imap"];
        Pathname.define_context "src/lwt" ["src/imap"];
        Pathname.define_context "tests" ["src/unix"; "src/lwt"; "src/imap"];
        Pathname.define_context "src/other" ["src/unix"; "src/lwt"; "src/imap"]
    | _ -> ()
  end
