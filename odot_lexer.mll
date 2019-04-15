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

{
(* $Id: odot_lexer.mll 133 2005-12-16 10:03:56Z zoggy $ *)
open Odot_parser


let print_DEBUG s = () (*prerr_endline*)

let line = ref 0

let buf = Buffer.create 256
let init_buf () = Buffer.reset buf

let keywords = [
  "digraph", DIGRAPH ;
  "graph", GRAPH ;
  "strict", STRICT ;
  "node", NODE ;
  "edge", EDGE ;
  "subgraph", SUBGRAPH ;
]

let open_angles = ref 0

}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']

let id_string_start =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255']
let id_string_body =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9']
let id_number = ['-']?('.'['0'-'9']+ | ['0'-'9']+('.'['0'-'9']*)? )


let one_line_comment = "//" [^'\n']*'\n'
let c_line = "\n#" [^'\n']*'\n'
let escaped_newline = "\\\n"

rule main = parse
| one_line_comment { incr line; main lexbuf }
| c_line { incr line; incr line; main lexbuf }
| escaped_newline { incr line; main lexbuf }
| "--" { EDGEOP }
| "->" { EDGEOP }
| ';' { SEMICOLON }
| ':' { COLON }
| ',' { COMMA }
| '=' { EQUALS }
| '{' { LBRA }
| '}' { RBRA }
| '[' { LSBRA }
| ']' { RSBRA }
| '+' { PLUS }
| "/*" { comment lexbuf }
| '"' { print_DEBUG "entering double quoted string";
	init_buf () ;
	double_quoted lexbuf
      }
| id_string_start id_string_body *
    {
      let id = Lexing.lexeme lexbuf in
      try
	List.assoc (String.lowercase_ascii id) keywords
      with
	Not_found ->
	  print_DEBUG ("ID "^id);
	  Id id
    }
| id_number
    {
      let id = Lexing.lexeme lexbuf in
      print_DEBUG ("ID "^id);
      Id id
    }

| '<'
    {
      init_buf () ;
      open_angles := 1 ;
      html_id lexbuf
    }
| blank { main lexbuf }
| newline { incr line ; main lexbuf}
| eof { print_DEBUG "EOF"; EOF }
| _
    {print_DEBUG (Lexing.lexeme lexbuf);
      main lexbuf
    }

and double_quoted = parse
| "\\\"" { Buffer.add_string buf "\""; double_quoted lexbuf
	 }
| newline { incr line ;
	    Buffer.add_string buf (Lexing.lexeme lexbuf);
	    double_quoted lexbuf
	  }
| escaped_newline { incr line; double_quoted lexbuf }
| '"' { print_DEBUG "getting out of double quoted string";
	Id_double_quoted (Buffer.contents buf)
      }
| eof { failwith ("End of file in double quoted string, line "^(string_of_int !line)) }
| _ { Buffer.add_string buf (Lexing.lexeme lexbuf);
      double_quoted lexbuf
    }

and html_id = parse
| ">" { decr open_angles;
	if !open_angles <= 0 then
	   Id_html (Buffer.contents buf)
	else
	  (
	   Buffer.add_string buf ">";
	   html_id lexbuf
	  )
	 }
| "<" { incr open_angles ;
	Buffer.add_char buf '<';
	html_id lexbuf
      }

| newline { incr line ;
	    Buffer.add_string buf (Lexing.lexeme lexbuf);
	    html_id lexbuf
	  }
| escaped_newline { incr line; html_id lexbuf }
| eof { failwith ("End of file in html id, line "^(string_of_int !line)) }
| _ { Buffer.add_string buf (Lexing.lexeme lexbuf);
      html_id lexbuf
    }

and comment = parse
| "*/" { main lexbuf }
| newline { incr line ; comment lexbuf}
| escaped_newline { incr line; comment lexbuf }
| eof { failwith "Comment not terminated" }
| _ { comment lexbuf }
