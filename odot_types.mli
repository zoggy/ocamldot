(*********************************************************************************)
(*                OCamldot                                                       *)
(*                                                                               *)
(*    Copyright (C) 2005-2012 Institut National de Recherche en Informatique et  *)
(*    en Automatique. All rights reserved.                                       *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Lesser General Public License for more details.                        *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser Public License           *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*********************************************************************************)

(* $Id: odot_types.mli 120 2005-12-09 14:55:50Z zoggy $ *)

(** Types *)

type graph_kind = Graph | Digraph
type id =
    Simple_id of string
  | Html_id of string
  | Double_quoted_id of string

type attr = id * id option

type compass_pt = N | NE | E | SE | S | SW | W | NW

type port = id * compass_pt option

type node_id = id * port option

type edge_stmt_point =
    Edge_node_id of node_id
  | Edge_subgraph of subgraph

and edge_stmt = edge_stmt_point * edge_stmt_point list * attr list

and attr_stmt =
    Attr_graph of attr list
  | Attr_node of attr list
  | Attr_edge of attr list

and stmt =
  | Stmt_node of node_id * attr list
  | Stmt_equals of id * id
  | Stmt_edge of edge_stmt
  | Stmt_attr of attr_stmt
  | Stmt_subgraph of subgraph

and subgraph =
    { mutable sub_id : id option ;
      mutable sub_stmt_list : stmt list ;
    }

and graph =
    {
      mutable strict : bool ;
      mutable kind : graph_kind ;
      mutable id : id option;
      mutable stmt_list : stmt list ;
    }
