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

(* $Id: test_gui.ml 600 2007-11-14 09:14:22Z zoggy $ *)

(** Testing the gui box to display graphs. *)

(*c==v=[Misc.md5sum_of_string]=1.0====*)
let md5sum_of_string s =
  let com = Printf.sprintf "echo %s | md5sum | cut -d\" \" -f 1"
      (Filename.quote s)
  in
  let ic = Unix.open_process_in com in
  let s = input_line ic in
  close_in ic;
  ignore (Unix.wait ());
  s
(*/c==v=[Misc.md5sum_of_string]=1.0====*)


class box file =
  object (self)
    inherit Odot_gtk.box ~dot_program: Odot_gtk.Fdp
	~tmp_hash: (Printf.sprintf "/tmp/%s" (md5sum_of_string file))
	()

    method refresh_data = ()

    method build_graph =
      try Odot.parse_file file
      with e ->
	let s = Printexc.to_string e in
	GToolbox.message_box "Error" s;
	{ Odot.id = None ;
	  Odot.strict = false ;
	  Odot.kind = Odot.Graph ;
	  Odot.stmt_list = [] ;
	}

    method on_button1_press ~x ~y = function
      None -> GToolbox.message_box "You clicked !" "no id under cursor"
    | Some s -> GToolbox.message_box "You clicked !" s
  end

let usage () =
  prerr_endline (Printf.sprintf "usage: %s <dot file>" Sys.argv.(0));
  exit 1

let main () =
  if Array.length Sys.argv < 2 then
    usage ();

  ignore (GMain.Main.init ());
  let f = Sys.argv.(1) in
  let w = GWindow.window ~width: 600 ~height: 400 ~title: f () in
  let b = new box f in
  w#add b#box#coerce;
  ignore (w#connect#destroy GMain.Main.quit);
  w#show ();
  GMain.Main.main ()


let _ = main ()
