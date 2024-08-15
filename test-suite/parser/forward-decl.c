// %run ./c9 -fdump-ast {}
/*
 %next > var-decl:
 %next ->type:
 %next --->struct:
 %next ----->empty-node
 %next ->definition:
 %next --->empty-node
 %next >typedef: q
 %next ->type:
 %next --->struct:
 %next ----->empty-node
*/
struct X;
typedef struct X q;
