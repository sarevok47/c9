// %run ./c9 -fdump-ast {}
/*
 %next | -typedef_decl_:         *
 %next   |-name: q
 %next   |-size: 0
 %next   |-type_name_:
 %next     |-size: 0
 %next     |-struct_decl_:
 %next       |-name:
 %next       |-size: 0
 %next       |-empty_node_:
 %next |-function_:
 %next   |-name: main
 %next   |-function_type_:
 %next     |-size: 0
 %next     |-type_name_:
 %next       |-size: 4
 %next       |-int_type_:
 %next         |-name: int
 %next         |-size: 4
 %next   |-compound_statement_:
 %next     |-variable_:
 %next       |-name: q
 %next       |-int_type_:
 %next         |-name: int
 %next         |-size: 4
 %next       |-int_cst_expression_:
 %next       |-ned in:
 %next         |-int_type_:
 %next           |-name: int
 %next           |-size: 4

*/
typedef struct X  q;

int main() { int q = 5; }
