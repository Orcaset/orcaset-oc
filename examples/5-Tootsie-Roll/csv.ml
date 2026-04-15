open Orcaset

let parse_date s =
  match String.split_on_char '-' s with
  | [ y; m; d ] -> Date.make (int_of_string y) (int_of_string m) (int_of_string d)
  | _ -> failwith ("invalid date: " ^ s)

let strip_bom s =
  if
    String.length s >= 3
    && Char.code s.[0] = 0xEF
    && Char.code s.[1] = 0xBB
    && Char.code s.[2] = 0xBF
  then String.sub s 3 (String.length s - 3)
  else s

let strip_cr s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

(** Split a CSV line respecting double-quoted fields. *)
let split_csv_line s =
  let s = strip_cr s in
  let len = String.length s in
  let buf = Buffer.create 64 in
  let rec go i in_quotes acc =
    if i >= len then List.rev (Buffer.contents buf :: acc)
    else if s.[i] = '"' then go (i + 1) (not in_quotes) acc
    else if s.[i] = ',' && not in_quotes then begin
      let field = Buffer.contents buf in
      Buffer.clear buf;
      go (i + 1) false (field :: acc)
    end
    else begin
      Buffer.add_char buf s.[i];
      go (i + 1) in_quotes acc
    end
  in
  go 0 false []

(* --- Period-data CSVs (two header rows: start dates, end dates) ---------- *)

type t = { periods : Period.t list; rows : (string * float list) list }

let of_file path =
  let ic = open_in path in
  let lines =
    let rec go acc =
      match input_line ic with
      | line -> go (line :: acc)
      | exception End_of_file ->
          close_in ic;
          List.rev acc
    in
    go []
  in
  let lines = match lines with first :: rest -> strip_bom first :: rest | [] -> [] in
  match lines with
  | starts_line :: ends_line :: data_lines ->
      let starts = List.tl (split_csv_line starts_line) |> List.map parse_date in
      let ends = List.tl (split_csv_line ends_line) |> List.map parse_date in
      let periods = List.map2 Period.make starts ends in
      let rows =
        List.map
          (fun line ->
            match split_csv_line line with
            | name :: values -> (name, List.map float_of_string values)
            | [] -> failwith "empty row")
          data_lines
      in
      { periods; rows }
  | _ -> failwith "CSV must have at least two header rows"

let periods t = t.periods
let find t name = List.assoc name t.rows

let cells t name =
  let values = find t name in
  List.map2
    (fun p v -> Period_cell.const p (fun () -> v) Period_cell.proportional_split)
    t.periods values

(* --- Quarterly-data CSVs (single header row with end-of-quarter dates) --- *)

type quarterly_csv = { qperiods : Period.t list; qrows : (string * float option list) list }

(* Build quarterly periods from a list of quarter-end dates.
   Periods are [prev_end, cur_end) to match Period.make_seq conventions.
   For the first quarter, we infer the previous quarter-end by shifting
   back 3 months with month_end snapping (e.g., 2023-03-31 -> 2022-12-31). *)
let periods_of_quarter_ends dates =
  match dates with
  | [] -> []
  | first :: _ ->
      let back_3m = Offset.make ~months:(-3) ~month_end:true () in
      let first_start = Date.shift back_3m first in
      let _, periods =
        List.fold_left
          (fun (prev_end, acc) end_date ->
            let p = Period.make prev_end end_date in
            (end_date, p :: acc))
          (first_start, []) dates
      in
      List.rev periods

let quarterly_of_file path =
  let ic = open_in path in
  let lines =
    let rec go acc =
      match input_line ic with
      | line -> go (line :: acc)
      | exception End_of_file ->
          close_in ic;
          List.rev acc
    in
    go []
  in
  let lines = match lines with first :: rest -> strip_bom first :: rest | [] -> [] in
  match lines with
  | date_line :: data_lines ->
      let dates = List.tl (split_csv_line date_line) |> List.map parse_date in
      let qperiods = periods_of_quarter_ends dates in
      let qrows =
        List.map
          (fun line ->
            match split_csv_line line with
            | name :: values ->
                let parsed =
                  List.map
                    (fun v ->
                      let v = String.trim v in
                      if v = "" then None else Some (float_of_string v))
                    values
                in
                (name, parsed)
            | [] -> failwith "empty row")
          data_lines
      in
      { qperiods; qrows }
  | _ -> failwith "CSV must have at least one header row"

let quarterly_periods t = t.qperiods
let quarterly_find t name = List.assoc name t.qrows

let quarterly_cells t name =
  let values = quarterly_find t name in
  List.map2
    (fun p v ->
      match v with
      | Some f -> Period_cell.const p (fun () -> f) Period_cell.proportional_split
      | None -> Period_cell.const p (fun () -> 0.0) Period_cell.proportional_split)
    t.qperiods values

(* --- Point-data CSVs ----------------------------------------------------- *)

type point_csv = { dates : Date.t list; point_rows : (string * float option list) list }

let point_of_file path =
  let ic = open_in path in
  let lines =
    let rec go acc =
      match input_line ic with
      | line -> go (line :: acc)
      | exception End_of_file ->
          close_in ic;
          List.rev acc
    in
    go []
  in
  let lines = match lines with first :: rest -> strip_bom first :: rest | [] -> [] in
  match lines with
  | date_line :: data_lines ->
      let dates = List.tl (split_csv_line date_line) |> List.map parse_date in
      let point_rows =
        List.map
          (fun line ->
            match split_csv_line line with
            | name :: values ->
                let parsed =
                  List.map
                    (fun v ->
                      let v = String.trim v in
                      if v = "" then None else Some (float_of_string v))
                    values
                in
                (name, parsed)
            | [] -> failwith "empty row")
          data_lines
      in
      { dates; point_rows }
  | _ -> failwith "CSV must have at least one header row"

let dates t = t.dates
let point_find t name = List.assoc name t.point_rows

(** [point_find_nth t name n] returns the values for the [n]-th (0-indexed) row named [name]. Useful
    when a CSV has duplicate row names. *)
let point_find_nth t name n =
  let rec go rows remaining =
    match rows with
    | [] -> failwith ("point_find_nth: not enough rows named " ^ name)
    | (k, v) :: rest ->
        if k = name then if remaining = 0 then v else go rest (remaining - 1) else go rest remaining
  in
  go t.point_rows n

let points t name =
  let values = point_find t name in
  List.filter_map
    (fun (d, v) -> match v with Some f -> Some (d, f) | None -> None)
    (List.combine t.dates values)

let points_nth t name n =
  let values = point_find_nth t name n in
  List.filter_map
    (fun (d, v) -> match v with Some f -> Some (d, f) | None -> None)
    (List.combine t.dates values)
