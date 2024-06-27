(* Inspired by Alcotest API
   https://github.com/mirage/alcotest
*)
open Format

module type TESTABLE = sig
  type t

  val pp : formatter -> t -> unit
  val equal : t -> t -> bool
end

type printer = formatter -> unit -> unit

let printer s fmt () = Format.fprintf fmt "%s" s

type 'a testable = (module TESTABLE with type t = 'a)
type test_result = (printer * printer, printer * printer) result

let check (type t) (module T : TESTABLE with type t = t) (v1 : t) (v2 : t) () =
  let s1 fmt () = T.pp fmt v1 in
  let s2 fmt () = T.pp fmt v2 in

  if T.equal v1 v2 then Ok (s1, s2) else Error (s1, s2)

module T = struct
  let string =
    (module struct
      include String

      let pp = pp_print_string
    end : TESTABLE
      with type t = string)

  let int =
    (module struct
      include Int

      let pp = pp_print_int
    end : TESTABLE
      with type t = int)

  let bool =
    (module struct
      include Bool

      let pp = pp_print_bool
    end : TESTABLE
      with type t = bool)
end

let do_tests l =
  let do_test test =
    match test () with
    | Ok (s1, s2) -> ("PASSED", s1, s2, true), 1
    | Error (s1, s2) -> ("FAILED", s1, s2, false), 0
    | exception e ->
      ("EXCEPTION", printer "ERROR", printer (Printexc.to_string e), false), 0
  in
  let do_serie name tests =
    let passed, total, lst =
      List.fold_left
        (fun (ap, at, al) test ->
           let v, i = do_test test in
           ap + i, at + 1, (at + 1, v) :: al)
        (0, 0, []) tests
    in
    (name, List.rev lst), (passed, total)
  in
  let passed, total, lst =
    List.fold_left
      (fun (ap, at, al) (name, tests) ->
         let ((_, (p, t)) as r) = do_serie name tests in
         (ap + if p = t then 1 else 0), at + 1, r :: al)
      (0, 0, []) l
  in
  List.rev lst, (passed, total)

type counter = {
  mutable tests : int;
  mutable series : int;
  mutable total_tests : int;
  mutable total_series : int;
  mutable failed_tests : string list;
}

let make_counter () =
  {
    tests = 0;
    series = 0;
    total_tests = 0;
    total_series = 0;
    failed_tests = [];
  }

let get_counter =
  let sessions = Hashtbl.create 16 in
  fun (id : string) ->
    try Hashtbl.find sessions id with
    | Not_found ->
      let counter = make_counter () in
      Hashtbl.replace sessions id counter;
      counter

let summary id =
  let counter = get_counter id in
  printf "@[SUMMARY:@[@\n";
  printf "@[Series: %d/%d@]@\n" counter.series counter.total_series;
  printf "@[Tests: %d/%d@]@\n" counter.tests counter.total_tests;
  begin
    match counter.failed_tests with
    | [] -> ()
    | _ ->
      printf "@[Failed tests:@[@\n";
      List.iter
        (fun s -> printf "- @[%s@]@\n" s)
        (List.rev counter.failed_tests);
      printf "@]@\n@\n"
  end;
  printf "@]@]@\n"

let run ?(id = "global") name l =
  let counter = get_counter id in
  let l, (passed, total) = do_tests l in
  counter.total_series <- counter.total_series + total;
  counter.series <- counter.series + passed;
  printf "@[Test: @['%s' (%d/%d)@\n" name passed total;
  List.iter
    (fun ((serie, tests), (passed, total)) ->
       counter.total_tests <- counter.total_tests + total;
       counter.tests <- counter.tests + passed;
       printf "@[Serie: @['%s.%s': (%d/%d)@\n@\n" name serie passed total;
       List.iter
         (fun (i, (msg, s1, s2, ok)) ->
            let id = sprintf "%s.%s.%d" name serie i in
            if not ok then counter.failed_tests <- id :: counter.failed_tests;
            printf "@['%s': %s@\n" id msg;
            printf "   @[ expected: @[%a@]@]@\n" s1 ();
            printf "   @[   result: @[%a@]@]@\n" s2 ();
            printf "@]")
         tests;
       printf "@]@\n@]@\n")
    l;
  printf "@]@]@\n"