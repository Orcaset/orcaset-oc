(* Copyright (C) 2026 Orcaset Inc.
 * SPDX-License-Identifier: SSPL-1.0 *)

type evaluated =
  | Span_values of (Period.t * float option) list
  | Point_values of (Date.t * float option) list

type stmt =
  | Group of stmt list
  | Total of { total : Series.packed_series; children : stmt list }
  | Line of Series.packed_series
  | Span_family_lines : {
      family : ('key, 'tag Series.Spans.t) Series.Family.t;
      label : 'key -> string;
    }
      -> stmt

type resolved =
  | RGroup of resolved list
  | RTotal of { label : string option; values : evaluated option; children : resolved list }
  | RLine of { label : string option; values : evaluated option }

(* ----- Constructors ----- *)

let span_series series = Series.Series (Series.Span_series series)
let point_series series = Series.Series (Series.Point_series series)
let group children = Group children
let span_line series = Line (span_series series)
let span_lines series = List.map span_line series

let span_family_lines ?label family =
  let label = Option.value ~default:(Series.Family.key_to_string family) label in
  Span_family_lines { family; label }

let span_total total children = Total { total = span_series total; children }
let point_line series = Line (point_series series)
let point_lines series = List.map point_line series
let point_total total children = Total { total = point_series total; children }

(* ----- Evaluators ----- *)

let eval_series_periods cache periods (Series.Series series) =
  let values =
    match series with
    | Series.Span_series spans ->
        Span_values
          (List.map (fun period -> (period, Series.query_span cache spans ~period)) periods)
    | Series.Point_series points ->
        Point_values
          (Period.list_to_dates periods
          |> List.map (fun date -> (date, Series.query_point cache points ~date)))
  in
  (Series.label series, Some values)

let eval_span_values cache periods spans =
  Span_values (List.map (fun period -> (period, Series.query_span cache spans ~period)) periods)

let eval_series_dates cache dates (Series.Series series) =
  let values =
    match series with
    | Series.Point_series points ->
        Some
          (Point_values (List.map (fun date -> (date, Series.query_point cache points ~date)) dates))
    | Series.Span_series _ -> None
  in
  (Series.label series, values)

let eval_periods periods stmt =
  let cache = Series.make_cache () in
  let family_members (type key tag) (family : (key, tag Series.Spans.t) Series.Family.t) =
    let add acc (key, series) =
      if List.exists (fun (existing, _) -> Series.Family.key_equal family existing key) acc then acc
      else (key, series) :: acc
    in
    periods
    |> List.concat_map (Series.Family.members family)
    |> List.fold_left add []
    |> List.sort (fun (a, _) (b, _) -> Series.Family.key_compare family a b)
  in
  let rec go = function
    | Group children -> RGroup (List.map go children)
    | Total { total; children } ->
        let label, values = eval_series_periods cache periods total in
        RTotal { label; values; children = List.map go children }
    | Line series ->
        let label, values = eval_series_periods cache periods series in
        RLine { label; values }
    | Span_family_lines { family; label } ->
        family_members family
        |> List.map (fun (key, series) ->
            RLine
              { label = Some (label key); values = Some (eval_span_values cache periods series) })
        |> fun lines -> RGroup lines
  in
  go stmt

let eval_dates dates stmt =
  let cache = Series.make_cache () in
  let rec go = function
    | Group children -> RGroup (List.map go children)
    | Total { total; children } ->
        let label, values = eval_series_dates cache dates total in
        RTotal { label; values; children = List.map go children }
    | Line series ->
        let label, values = eval_series_dates cache dates series in
        RLine { label; values }
    | Span_family_lines _ -> RGroup []
  in
  go stmt

(* ----- Formatters ----- *)

type fixed_width_row =
  | Blank
  | Rule
  | Data of { depth : int; label : string option; cells : (Date.t * string option) list }

type markdown_row =
  | Markdown_blank
  | Markdown_data of {
      depth : int;
      label : string option;
      cells : (Date.t * string option) list;
      is_total : bool;
    }

let fixed_width_indent = 2
let fixed_width_separator = "  "
let fixed_width_min_value_width = 10
let fixed_width_unknown_label = "Unknown"

let resolved_label = function
  | Some label when String.trim label <> "" -> String.trim label
  | _ -> fixed_width_unknown_label

let fixed_width_cells = function
  | None -> []
  | Some (Span_values values) ->
      List.map
        (fun (period, value) -> (Period.end_ period, Option.map (Printf.sprintf "%.2f") value))
        values
  | Some (Point_values values) ->
      List.map (fun (date, value) -> (date, Option.map (Printf.sprintf "%.2f") value)) values

let fixed_width_rows resolved =
  let rec go depth = function
    | RLine { label; values } -> [ Data { depth; label; cells = fixed_width_cells values } ]
    | RTotal { label; values; children } ->
        List.concat_map (go (depth + 1)) children
        @ [ Rule; Data { depth; label; cells = fixed_width_cells values }; Blank ]
    | RGroup children -> (Blank :: List.concat_map (go depth) children) @ [ Blank ]
  in
  go 0 resolved

let markdown_rows resolved =
  let rec go depth = function
    | RLine { label; values } ->
        [ Markdown_data { depth; label; cells = fixed_width_cells values; is_total = false } ]
    | RTotal { label; values; children } ->
        List.concat_map (go (depth + 1)) children
        @ [
            Markdown_data { depth; label; cells = fixed_width_cells values; is_total = true };
            Markdown_blank;
          ]
    | RGroup children ->
        (Markdown_blank :: List.concat_map (go depth) children) @ [ Markdown_blank ]
  in
  go 0 resolved

let fixed_width_dates rows =
  let add_cell dates (date, _) =
    if List.exists (Date.equal date) dates then dates else date :: dates
  in
  let add_row dates = function
    | Blank | Rule -> dates
    | Data { cells; _ } -> List.fold_left add_cell dates cells
  in
  List.fold_left add_row [] rows |> List.sort Date.compare

let markdown_dates rows =
  let add_cell dates (date, _) =
    if List.exists (Date.equal date) dates then dates else date :: dates
  in
  let add_row dates = function
    | Markdown_blank -> dates
    | Markdown_data { cells; _ } -> List.fold_left add_cell dates cells
  in
  List.fold_left add_row [] rows |> List.sort Date.compare

let fixed_width_label depth label =
  String.make (depth * fixed_width_indent) ' ' ^ resolved_label label

let string_width = String.length
let max_or_zero values = List.fold_left max 0 values

let pad_left width value =
  let padding = width - string_width value in
  if padding <= 0 then value else String.make padding ' ' ^ value

let pad_right width value =
  let padding = width - string_width value in
  if padding <= 0 then value else value ^ String.make padding ' '

let fixed_width_label_width rows =
  rows
  |> List.filter_map (function
    | Blank | Rule -> None
    | Data { depth; label; _ } -> Some (fixed_width_label depth label |> string_width))
  |> max_or_zero

let fixed_width_value_width dates rows =
  let date_widths = List.map (fun date -> Date.to_string date |> string_width) dates in
  let cell_widths =
    rows
    |> List.concat_map (function
      | Blank | Rule -> []
      | Data { cells; _ } -> List.filter_map (fun (_, value) -> Option.map string_width value) cells)
  in
  max fixed_width_min_value_width (max_or_zero (date_widths @ cell_widths))

let fixed_width_cell cells date =
  match List.find_opt (fun (cell_date, _) -> Date.equal cell_date date) cells with
  | Some (_, Some value) -> value
  | Some (_, None) | None -> ""

let markdown_escape value =
  value |> String.split_on_char '\\' |> String.concat "\\\\" |> String.split_on_char '|'
  |> String.concat "\\|"

let markdown_indent depth =
  String.concat "" (List.init (depth * fixed_width_indent) (fun _ -> "&nbsp;"))

let markdown_label depth label = markdown_indent depth ^ (resolved_label label |> markdown_escape)
let markdown_cell cells date = fixed_width_cell cells date |> markdown_escape
let markdown_bold value = if value = "" then "" else "**" ^ value ^ "**"

let markdown_render_row dates = function
  | Markdown_blank ->
      "| " ^ String.concat " | " (List.init (List.length dates + 1) (fun _ -> "")) ^ " |"
  | Markdown_data { depth; label; cells; is_total } ->
      let label = markdown_label depth label in
      let values = List.map (markdown_cell cells) dates in
      let values = if is_total then List.map markdown_bold values else values in
      let label = if is_total then markdown_bold label else label in
      "| " ^ String.concat " | " (label :: values) ^ " |"

let markdown_header dates =
  let headers = "" :: List.map Date.to_string dates in
  let separators = List.map (fun _ -> "---") headers in
  String.concat "\n"
    [ "| " ^ String.concat " | " headers ^ " |"; "| " ^ String.concat " | " separators ^ " |" ]

let csv_escape value =
  if String.exists (function ',' | '"' | '\n' | '\r' -> true | _ -> false) value then
    "\"" ^ (value |> String.split_on_char '"' |> String.concat "\"\"") ^ "\""
  else value

let csv_render_row dates = function
  | Markdown_blank -> String.concat "," (List.init (List.length dates + 1) (fun _ -> ""))
  | Markdown_data { label; cells; _ } ->
      let label = resolved_label label |> csv_escape in
      let values = List.map (fun date -> fixed_width_cell cells date |> csv_escape) dates in
      String.concat "," (label :: values)

let csv_header dates = String.concat "," ("" :: List.map (fun date -> Date.to_string date) dates)

let fixed_width_render_row ~label_width ~value_width dates = function
  | Blank -> ""
  | Rule ->
      if dates = [] then ""
      else
        let cells = List.map (fun _ -> String.make value_width '-') dates in
        pad_right label_width "" ^ fixed_width_separator ^ String.concat fixed_width_separator cells
  | Data { depth; label; cells } ->
      let label = fixed_width_label depth label in
      if dates = [] then pad_right label_width label
      else
        let values =
          List.map (fun date -> fixed_width_cell cells date |> pad_left value_width) dates
        in
        pad_right label_width label ^ fixed_width_separator
        ^ String.concat fixed_width_separator values

let fixed_width_header ~label_width ~value_width dates =
  let headers = List.map (fun date -> Date.to_string date |> pad_left value_width) dates in
  pad_right label_width "" ^ fixed_width_separator ^ String.concat fixed_width_separator headers

let fixed_width resolved =
  let rows = fixed_width_rows resolved in
  let dates = fixed_width_dates rows in
  let label_width = fixed_width_label_width rows in
  let value_width = fixed_width_value_width dates rows in
  let rendered_rows = List.map (fixed_width_render_row ~label_width ~value_width dates) rows in
  (if dates = [] then rendered_rows
   else fixed_width_header ~label_width ~value_width dates :: rendered_rows)
  |> String.concat "\n"

let markdown resolved =
  let rows = markdown_rows resolved in
  let dates = markdown_dates rows in
  let rendered_rows = List.map (markdown_render_row dates) rows in
  markdown_header dates :: rendered_rows |> String.concat "\n"

let csv resolved =
  let rows = markdown_rows resolved in
  let dates = markdown_dates rows in
  let rendered_rows = List.map (csv_render_row dates) rows in
  csv_header dates :: rendered_rows |> String.concat "\n"
