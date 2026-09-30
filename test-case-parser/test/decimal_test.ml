(* Decimals cross the JSON boundary as exact strings and money as integer cents:
   nothing between a test file, the editor and the interpreter goes through a
   binary float. *)

module O = Catala_types_t
open Testcase_lib

let check_eq what pp expected got =
  if expected <> got then
    failwith
      (Printf.sprintf "%s: expected %s, got %s" what (pp expected) (pp got))

let quoted = Printf.sprintf "%S"

(* Spellings a reader may meet, and the one every reader produces *)
let canonical =
  [
    "0.1", "0.1";
    "1234567890.123456789", "1234567890.123456789";
    "3703703670.370370367", "3703703670.370370367";
    "1/3", "1/3";
    "-2/3", "-2/3";
    "2/4", "0.5";
    "3", "3.0";
    "13.", "13.0";
    "-0.50", "-0.5";
    "-0", "0.0";
    "0.000001", "0.000001";
    "1/1024", "0.0009765625";
    "22/7", "22/7";
    "123456789012345678901234567890.5", "123456789012345678901234567890.5";
  ]

let check_canonical () =
  List.iter
    (fun (s, expected) ->
      check_eq ("canonical " ^ s) quoted expected
        Model.(string_of_decimal (decimal_of_string s)))
    canonical

let check_rejects () =
  List.iter
    (fun s ->
      match Model.decimal_of_string s with
      | exception Failure _ -> ()
      | q ->
        failwith
          (Printf.sprintf "%S was accepted as the decimal %s" s (Q.to_string q)))
    [
      "";
      "-";
      ".";
      "abc";
      "1/0";
      "1.2/3";
      "1e5";
      "0x10";
      "+1";
      " 1";
      "1.2.3";
      "1/";
    ]

let check_money () =
  List.iter
    (fun (cents, expected) ->
      check_eq
        (Printf.sprintf "money %d" cents)
        quoted expected
        (Model.string_of_money_cents cents))
    [
      0, "0.00";
      5, "0.05";
      -5, "-0.05";
      -100, "-1.00";
      12345678901234, "123456789012.34";
      max_int, "46116860184273879.03";
      min_int, "-46116860184273879.04";
    ]

let print lang v =
  Format.asprintf "%a"
    (Model.print_catala_value ~typ:(Some O.TRat) ~lang)
    O.{ value = Decimal v; attrs = [] }

(* What the writer puts in a test file: Catala has no literal for 1/3 *)
let check_source_literals () =
  List.iter
    (fun (lang, v, expected) ->
      check_eq ("literal " ^ v) quoted expected (print lang v))
    [
      `En, "0.1", "0.1";
      `En, "1234567890.123456789", "1234567890.123456789";
      `En, "3.0", "3.0";
      `En, "-0.5", "-0.5";
      `En, "1/3", "(1.0 / 3.0)";
      `En, "-2/3", "(-2.0 / 3.0)";
      `Fr, "0.1", "0,1";
      `Fr, "-2/3", "(-2,0 / 3,0)";
      `Pl, "1/3", "(1.0 / 3.0)";
    ]

(* Any rational, finite decimal expansion or not, survives its spelling *)
let round_trip_property =
  let gen =
    QCheck.Gen.(
      map3
        (fun num den pow ->
          Q.make (Z.of_int64 num)
            (Z.mul (Z.of_int (1 + abs den)) (Z.pow (Z.of_int 10) pow)))
        int64 nat_small (int_bound 30))
  in
  QCheck.Test.make ~name:"decimal spelling round trip" ~count:10_000
    (QCheck.make ~print:Q.to_string gen) (fun q ->
      Q.equal q Model.(decimal_of_string (string_of_decimal q)))

let () =
  let open Tezt.Test in
  register ~__FILE__ ~title:"decimal: canonical spellings"
    ~tags:["unit"; "decimal"] (fun () -> Lwt.return @@ check_canonical ());
  register ~__FILE__ ~title:"decimal: malformed spellings are refused"
    ~tags:["unit"; "decimal"] (fun () -> Lwt.return @@ check_rejects ());
  register ~__FILE__ ~title:"decimal: money inputs from integer cents"
    ~tags:["unit"; "decimal"; "money"] (fun () -> Lwt.return @@ check_money ());
  register ~__FILE__ ~title:"decimal: exact Catala source literals"
    ~tags:["unit"; "decimal"] (fun () -> Lwt.return @@ check_source_literals ());
  register ~__FILE__ ~title:"PBT: decimal spelling round trip"
    ~tags:["pbt"; "qcheck"; "decimal"] (fun () ->
      let (Test cell : QCheck.Test.t) = round_trip_property in
      Lwt.return @@ QCheck.Test.check_cell_exn cell)

let () = Tezt.Test.run ()
