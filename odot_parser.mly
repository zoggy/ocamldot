%{
open Odot_types

let compass_pt_of_id id =
  let s =
    match id with
      Simple_id s -> s
     | Double_quoted_id s -> s
     | Html_id s -> "<"^s^">"
  in
  let s = String.lowercase s in
  match s with
    "n" -> N
   | "ne" -> NE
   | "e" -> E
   | "se" -> SE
   | "s" ->  S
   | "sw" ->  SW
   | "w" ->  W
   | "nw" ->  NW
   | _ -> failwith "Not a compass_pt"
%}
%token LBRA
%token RBRA
%token STRICT
%token GRAPH DIGRAPH
%token Id Id_html Id_double_quoted
%type <string> Id Id_html Id_double_quoted
%token SEMICOLON
%token NODE
%token EDGE
%token LSBRA
%token RSBRA
%token COMMA
%token EQUALS
%token SUBGRAPH
%token COLON
%token PLUS
%token EDGEOP
%token EOF
%type <Odot_types.graph> graph
%start graph
%%
a_list:
|  {
  []
  }
| id a_list {
  ($1, None) :: $2
  }
| id COMMA a_list {
  ($1, None) :: $3
  }
| id EQUALS id a_list {
  ($1, Some $3) :: $4
  }
| id EQUALS id COMMA a_list {
  ($1, Some $3) :: $5
  }
;

attr_list:
|  {
  []
  }
| LSBRA a_list RSBRA attr_list {
  $2 @ $4
  }
;

node_id:
| id {
  ($1, None)
  }
| id COLON id {
  ($1, Some ($3, None))
  }
| id COLON id COLON id {
  try
  let compass_pt = compass_pt_of_id $5 in
  ($1, Some ($3, Some compass_pt))
with Failure _ ->
   failwith "Not a compass point."
  }
;

edge_point:
| node_id {
  Edge_node_id $1
  }
| subgraph {
  Edge_subgraph $1
  }
;

edgeRHS:
| EDGEOP edge_point {
  [$2]
  }
| EDGEOP edge_point edgeRHS {
  $2 :: $3
  }
;

edge_stmt:
| edge_point edgeRHS attr_list {
  ($1, $2, $3)
  }
;

attr_stmt:
| GRAPH attr_list {
  Attr_graph $2
  }
| NODE attr_list {
  Attr_node $2
  }
| EDGE attr_list {
  Attr_edge $2
  }
;

stmt:
| node_id attr_list {
  Stmt_node ($1, $2)
  }
| id EQUALS id {
  Stmt_equals ($1, $3)
  }
| edge_stmt {
  Stmt_edge $1
  }
| attr_stmt {
  Stmt_attr $1
  }
| subgraph {
  Stmt_subgraph $1
  }
;

stmt_list:
| stmt {
  [$1]
  }
| stmt SEMICOLON {
  [$1]
  }
| stmt SEMICOLON stmt_list {
  $1 :: $3
  }
| stmt stmt_list {
  $1 :: $2
  }
;

subgraph:
| SUBGRAPH id_opt LBRA stmt_list RBRA {
  { sub_id = $2 ;
   sub_stmt_list = $4;
}
  }
;

graph:
| strict graph_kind id_opt LBRA stmt_list RBRA {
  { strict = $1 ;
   kind = $2;
   id = $3;
   stmt_list = $5;
}
  }
;

id_opt:
|  {
  None
  }
| id {
  Some $1
  }
;

id:
| Id {
  Simple_id $1
  }
| id_dbl_q {
  Double_quoted_id $1
  }
| Id_html {
  Html_id $1
  }
;

id_dbl_q:
| Id_double_quoted {
  $1
  }
| Id_double_quoted PLUS id_dbl_q {
  $1 ^ $3
  }
;

graph_kind:
| GRAPH {
  Graph
  }
| DIGRAPH {
  Digraph
  }
;

strict:
|  {
  false
  }
| STRICT {
  true
  }
;
%%