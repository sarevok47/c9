// %run ./c9 -fdump-ast {}
/*
%next |-type_name_:
%next   |-size: 0
%next   |-struct_decl_:
%next     |-name:
%next     |-size: 0
%next     |-empty_node_:
%next |-typedef_decl_:
%next   |-name: q
%next   |-size: 0
%next   |-type_name_:
%next     |-size: 0
%next     |-struct_decl_:
%next       |-name:
%next       |-size: 0
%next       |-empty_node_:
*/
struct X;
typedef struct X q;
