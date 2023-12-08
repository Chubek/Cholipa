#use "absyn"

type symbol_table_node = {
        name: identifier;
        tyy: type_specifier;
        tree: program;
        lexscope: symbol_lexical_scope;
        next: symbol_table_node option;
}

and symbol_lexical_scope =
   | GlobalScope
   | FunctionScope of int
   | LocalScope of int
;;

type symbol_table = symbol_table_node option

let symtab_insert ~sym_table name tyy tree lexscope : symbol_table = 
        match sym_table with
      | None -> {name;tyy;tree;lexscope;next=None;}
      | Some -> 
                      let {nm;ty;tr;lexs;next} = sym_table in
                      match next with
                    | None -> {nm;ty;tr;Some {name;tyy;tree;lexc;None}}
                    | Some -> symtab_insert ~sym_table:sym_table name tyy tree
;;
