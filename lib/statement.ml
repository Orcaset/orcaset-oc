type 'a item =
  | Line of { label : string; seq : 'a }
  | Group of { label : string; items : 'a item list; total : 'a option }

let line label seq = Line { label; seq }
let group ?total label items = Group { label; items; total }

let rec fold ~line_fn ~group_fn item =
  match item with
  | Line { label; seq } -> line_fn label seq
  | Group { label; items; total } ->
      let folded_items = List.map (fold ~line_fn ~group_fn) items in
      group_fn label folded_items total

let rec iter ~line_fn ~group_fn item =
  match item with
  | Line { label; seq } -> line_fn label seq
  | Group { label; items; total } ->
      group_fn label total `Enter;
      List.iter (iter ~line_fn ~group_fn) items;
      group_fn label total `Exit

let lines item =
  let acc = ref [] in
  let line_fn label seq = acc := (label, seq) :: !acc in
  let group_fn _ _ _ = () in
  iter ~line_fn ~group_fn item;
  List.rev !acc

let rec to_list item =
  match item with
  | Line { label; seq } -> [ (label, Some seq) ]
  | Group { label; items; total } ->
      let children = List.concat_map to_list items in
      let total_entry =
        match total with Some seq -> [ (label ^ " (Total)", Some seq) ] | None -> []
      in
      [ (label, None) ] @ children @ total_entry
