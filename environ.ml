#use "absyn"

type symbol_table_entry = {
        ident: identifier;
        typ: type_specifier;
        value: program;
        level: symbol_entry_level;
        parent: symbol_table_entry;
        children symbol_table_entry list;
}

and direction_graph_entry = {
        ident: identifier;
        deps: direction_graph_entry list;
        next: direction-graph_entry;
}

and flow_graph_ir = string

and ssa_graph_ir = string

and linear_ir = string

and symbol_entry_level =
   | Global
   | Scope of int
   | Local of int

and environment = {
        root_symtable: symbol_table_entry;
        control_flow: flow_graph_ir;
        ssa_graph: ssa_graph_ir;
        linear_repr: linear_ir;
        dependencies: direction_graph_entry;

}
