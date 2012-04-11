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

(* $Id: odot_view.mli 662 2008-09-09 07:25:32Z zoggy $ *)

(** Displaying dot graphs in Lablgtk2. *)

(** To choose between the dot program to use to create images. *)
type dot_program = Dot | Fdp | Neato | Twopi | Circo


(** @param dot_program is [Dot] by default
   @param tmp_hash is the prefix to use for the temporary files.
*)
class virtual box :
    ?dot_program:dot_program ->
      tmp_hash: string -> unit ->
	object
	  val mutable current_zoom : float
	  val mutable dot_height : int
	  val mutable dot_width : int
	  val mutable ids : (float * float * float * float * string) list
	  method box : GPack.box
	  method virtual build_graph : Odot.graph
	  method clean_files : unit
	  method virtual on_button1_press : x: int -> y: int -> string option -> unit
	  method on_button3_press : int -> int -> unit
	  method refresh : unit -> unit
	  method virtual refresh_data : unit
	  method update_info : unit
	  method zoom : unit -> unit
	end


(*
type text = {
  text_item : GnoCanvas.text ;
  text_fontsize : int ;
  }

type node = {
  node_item : GnoCanvas.base_item ;
  node_id : string ;
  node_text : text option;
}
class virtual box :
  ?dot_program:dot_program ->
  tmp_hash:string ->
  unit ->
  object
    val mutable current_zoom : float
    val mutable edges : 'a list
    val mutable graph : Odot.graph option
    val mutable nodes : node list
    method box : GPack.box
    method virtual build_graph : Odot.graph
    method clean_files : unit
    method display : unit -> unit
    method input_zoom : unit -> unit
    method load_graph : string -> unit
    method virtual on_button1_press : x:int -> y:int -> string option -> unit
    method on_button3_press : x:int -> y:int -> string option -> unit
    method refresh : unit -> unit
    method virtual refresh_data : unit
    method refresh_dot : unit -> unit
    method resize_text_items : unit
  end
*)