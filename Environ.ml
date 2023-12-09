type symbol_entry = {
  name: identifier;
  data_type: symbol_type;
  offset: int;
}

type scope_entry = {
  parent: scope option;
  symbols: symbol_entry list
}

and scope = Scope of scope_entry

let create_symbol name data_type offset = { name; data_type; offset }

let create_scope parent symbols = { parent; symbols }

let add_symbol scope symbol =
  match scope with
  | Scope entry ->
    let symbols = symbol :: entry.symbols in
    Scope { entry with symbols }

let rec lookup_symbol name scope =
  match scope with
  | Scope entry ->
    let matching_symbols = List.filter (fun s -> s.name = name) entry.symbols in
    match matching_symbols with
    | [symbol] -> Some symbol
    | _ -> (
        match entry.parent with
        | Some parent_scope -> lookup_symbol name parent_scope
        | None -> None
      )

