open Ocamlbuild_plugin
  
let _ =
  dispatch begin
    function
      After_rules ->
        Pathname.define_context "src/unix" ["src/core"];
        Pathname.define_context "src/lwt" ["src/core"];
        Pathname.define_context "tests" ["src/unix"; "src/lwt"; "src/core"]
    | _ -> ()
  end
