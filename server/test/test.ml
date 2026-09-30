(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
   Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open QCheck
open Catala_utils
open Server_types

module PMap = Position_map.Make (struct
  include String

  let format = Format.pp_print_string
end)

let column_gen =
  let open Gen in
  let* l = 1 -- 80 in
  let* r = l -- (l + 50) in
  return (l, r)

let line_gen =
  let open Gen in
  let* l = 1 -- 800 in
  let* r = l -- (l + 2) in
  return (l, r)

let pos_gen =
  let open Gen in
  let open Pos in
  let* sl, el = line_gen in
  let* sc, ec = column_gen in
  return (from_info "dummy" sl sc el ec)

let data_gen =
  let open Gen in
  (* string_size (1 -- 10) ~gen:(oneof [char_range 'a' 'z'; char_range 'A'
     'Z']) *)
  string_small_of (oneof [char_range 'a' 'z'; char_range 'A' 'Z'])

let insert_all = List.fold_right (fun (p, v) -> PMap.add p v)

let list_gen =
  let open Gen in
  list_size (10 -- 15) (pair pos_gen data_gen)

module S = Set.Make (String)

let gen_hierarchy_arb =
  make ~shrink:Shrink.list_spine
    ~print:(fun l ->
      List.fold_right
        (fun (p, d) (m, s) ->
          let after = PMap.(add p d m) in
          ( after,
            Format.asprintf "tree: %a@\n@\n" PMap.format (PMap.finalize after)
            :: s ))
        l (PMap.empty_acc, [])
      |> fun (_, sl) -> String.concat "" (List.rev sl))
    list_gen

let pbt_hierarchy_test =
  let hierarchy_prop l =
    let t = insert_all l PMap.empty_acc |> PMap.finalize in
    let rec check (PMap.Tree.Node { itv = (li, i), (lj, j); children; _ }) :
        bool =
      let inner (PMap.Tree.Node { itv = (li', i'), (lj', j'); _ } as node) =
        (* child node's itv is subset of parent's itv *)
        let b =
          if li < li' && lj > lj' then true
          else if li = li' then i <= i' && (lj' < lj || (lj = lj' && j' <= j))
          else if li < li' && lj = lj' then j' <= j
          else false
        in
        b && check node
      in
      List.for_all inner children
    in
    Doc_id.Map.for_all (fun _ nodes -> List.for_all check nodes) t
  in
  QCheck.Test.make ~name:"hierarchy" ~long_factor:1000 ~count:100_000
    ~max_fail:0 gen_hierarchy_arb hierarchy_prop

let mk_pos ?lines sc ec =
  let el, ol = match lines with Some (a, b) -> a, b | None -> 1, 1 in
  Pos.from_info "dummy" el sc ol ec

let test_commute () =
  let map = PMap.empty_acc in
  let inner = PMap.add (mk_pos 1 2) "inner" in
  let outter = PMap.add (mk_pos 1 3) "outter" in
  let before = outter (inner map) in
  let after = inner (outter map) in
  assert (PMap.finalize before = PMap.finalize after)

(* The debugger evaluates a scope of an inline program, without stdlib *)
let debug_eval source scope =
  let options =
    Global.enforce_options
      ~input_src:(Contents (source, "test.catala_en"))
      ~language:(Some `En) ()
  in
  let prg, _ =
    Driver.Passes.dcalc options ~includes:[] ~stdlib:None ~optimize:false
      ~check_invariants:false ~autotest:false ~typed:Shared_ast.Expr.typed
  in
  let scope = Driver.Commands.get_scope_uid prg.decl_ctx scope in
  Lwt_main.run (Debug_interpret.interpret_with_env prg scope)

let test_json_literal () =
  let source =
    {|
```catala
declaration structure P:
  data n content integer
  data m content money

declaration scope S:
  output p content P

scope S:
  definition p equals #[json = "{\"n\": 9007199254740993, \"m\": 0.29}"] P
```
|}
  in
  let result =
    Format.asprintf "%a" Shared_ast.Print.UserFacing.expr
      (debug_eval source "S")
  in
  List.iter
    (fun expected ->
      if not (Re.execp (Re.compile (Re.str expected)) result) then
        Tezt.Test.fail "expected %s in %s" expected result)
    ["9,007,199,254,740,993"; "$0.29"]

let test_duration_overflow () =
  let source =
    {|
```catala
declaration scope S:
  output d content duration

scope S:
  definition d equals 4611686018427387903 day + 1 day
```
|}
  in
  match debug_eval source "S" with
  | _ -> Tezt.Test.fail "expected IntegerOverflow"
  | exception Catala_runtime.Error (Catala_runtime.IntegerOverflow, _, _) -> ()

let () =
  let open Tezt.Test in
  register ~__FILE__ ~title:"commutativity" ~tags:["unit"; "ordering"]
    (fun () -> Lwt.return @@ test_commute ());
  register ~__FILE__ ~title:"debugger: JSON literal of a structure"
    ~tags:["unit"; "debugger"] (fun () -> Lwt.return @@ test_json_literal ());
  register ~__FILE__ ~title:"debugger: duration sum overflow"
    ~tags:["unit"; "debugger"] (fun () ->
      Lwt.return @@ test_duration_overflow ());
  register ~__FILE__ ~title:"PBT: hierarchy property"
    ~tags:["pbt"; "qcheck"; "hierarchy"] (fun () ->
      let (Test cell) = pbt_hierarchy_test in
      Lwt.return @@ QCheck.Test.check_cell_exn cell)

let () = Tezt.Test.run ()
