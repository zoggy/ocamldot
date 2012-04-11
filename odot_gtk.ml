(*********************************************************************************)
(*                Odot                                                           *)
(*                                                                               *)
(*    Copyright (C) 2005 Institut National de Recherche en Informatique et       *)
(*    en Automatique. All rights reserved.                                       *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as published          *)
(*    by the Free Software Foundation; either version 2.1 of the License, or     *)
(*    any later version.                                                         *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Lesser General Public License for more details.                        *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*********************************************************************************)

(* $Id: odot_view.ml 662 2008-09-09 07:25:32Z zoggy $ *)

(** A Lablgtk2 box to view dot graphs.*)

let default_dot_ppi = 72.0

let p_dbg s = ()

(* let p_dbg = prerr_endline *)


type dot_program = Dot | Fdp | Neato | Twopi | Circo

let string_of_dot_program = function
  Dot -> "dot"
| Fdp -> "fdp"
| Circo -> "circo"
| Neato -> "neato"
| Twopi -> "twopi"

(*c==v=[String.split_string]=1.0====*)
let split_string s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" -> iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
    (*/c==v=[String.split_string]=1.0====*)

let show image file zoom_file zoom =
  let com = Printf.sprintf "convert -resize %d%% %s %s"
    zoom
      (Filename.quote file)
      (Filename.quote zoom_file)
  in
  match Sys.command com with
    0 -> image#set_file zoom_file
  | n -> failwith (Printf.sprintf "Exec error %d: %s" n com)

let get_graph_bounding_box stmt_list =
  let rec iter = function
    [] -> raise Not_found
  | (Odot.Stmt_attr (Odot.Attr_graph attr_list)) :: q ->
      begin
        match Odot.attr_value (Odot.Simple_id "bb") attr_list with
          Some (Odot.Simple_id v)
        | Some (Odot.Double_quoted_id v) ->
            begin
              match split_string v [','] with
                [x1;y1;x2;y2] ->
                  (
                   let (a,b,c,d) =
                     try (int_of_string x1, int_of_string y1,
                        int_of_string x2, int_of_string y2)
                     with | _ -> raise Not_found
                   in
                   match a,b,c,d with
                     0, _, _, 0 -> (0,0,c,b)
                   | _ as x -> x
                  )
              | _ -> raise Not_found
            end
        | _ -> iter q
      end
  | _ :: q -> iter q
  in
  iter stmt_list

let analyse_annot_dot_file f =
  try
    let graph = Odot.parse_file f in
    let (_,_,width,height) = get_graph_bounding_box graph.Odot.stmt_list in
    p_dbg (Printf.sprintf "width=%d,height=%d" width height);
    let rec iter acc = function
      [] -> acc
    |	stmt :: q ->
        match stmt with
          Odot.Stmt_node (node_id,attr_list) ->
            p_dbg "Stmt_node";
            begin
              try
                let w =
                  match Odot.attr_value (Odot.Simple_id "width") attr_list with
                  | Some (Odot.Simple_id v)
                  | Some (Odot.Double_quoted_id v) ->
                      (try float_of_string v
                       with _ -> raise Not_found)
                  | _ -> raise Not_found
                in
                let h =
                  match Odot.attr_value (Odot.Simple_id "height") attr_list with
                  | Some (Odot.Simple_id v)
                  | Some (Odot.Double_quoted_id v) ->
                      (try float_of_string v
                       with _ -> raise Not_found)
                  | _ -> raise Not_found
                in
                let (x,y) =
                  match Odot.attr_value (Odot.Simple_id "pos") attr_list with
                  | Some (Odot.Simple_id v)
                  | Some (Odot.Double_quoted_id v) ->
                      begin
                        match split_string v [','] with
                          [x;y] ->
                            (
                             try (int_of_string x, int_of_string y)
                             with | _ -> raise Not_found
                            )
                        | _ -> raise Not_found
                      end
                  | _ -> raise Not_found
                in
                let w = w *. default_dot_ppi in
                let h = h *. default_dot_ppi in
                let x1 = (float x) -. w /. 2.0 in
                let y1 = (float y) -. h /. 2.0 in
                let x2 = (float x) +. w /. 2.0 in
                let y2 = (float y) +. h /. 2.0 in
                let s_id = Odot.string_of_node_id node_id in
                p_dbg (Printf.sprintf "id %s: x1=%f y1=%f x2=%f y2=%f"
                 s_id x1 y1 x2 y2);
                iter ((x1,y1,x2,y2,s_id)::acc) q
              with
                Not_found ->
                  iter acc q
            end
        | Odot.Stmt_subgraph g ->
            iter acc (g.Odot.sub_stmt_list @ q)
        | Odot.Stmt_equals _
        | Odot.Stmt_edge _
        | Odot.Stmt_attr _ -> iter acc q
    in
    (width, height, iter [] graph.Odot.stmt_list)
  with
    e ->
      p_dbg (Printexc.to_string e);
      (1, 1, [])


class virtual box ?(dot_program=Dot) ~tmp_hash () =
  let dot_file = Printf.sprintf "%s.dot" tmp_hash in
  let annot_dot_file = Printf.sprintf "%s.dot_annot" tmp_hash in
  let png_file = Printf.sprintf "%s.png" tmp_hash in
  let vbox = GPack.vbox () in
  let hbox = GPack.hbox ~spacing:5 ~packing:(vbox#pack ~expand: false) () in
  let _ = GMisc.label ~text: "Zoom:" ~packing: (hbox#pack ~padding: 4 ~expand: false) () in
  let zooms =
    [ 10 ; 20 ; 30 ; 40 ; 50 ; 60 ; 70 ; 80 ; 90 ; 100 ; 120 ]
  in
  let wcombo = GEdit.combo
    ~popdown_strings: (List.map (fun s -> Printf.sprintf "%d%%" s) zooms)
      ~allow_empty:false
      ~enable_arrow_keys:true
      ~value_in_list:true
      ~packing: (hbox#pack ~expand: false)
      ()
  in
  let wb_refresh = GButton.button ~label: "Refresh"
    ~packing: (hbox#pack ~expand: false ~padding: 4) ()
  in
  let wscroll = GBin.scrolled_window
    ~vpolicy: `AUTOMATIC
      ~hpolicy: `AUTOMATIC
      ~packing: (vbox#pack ~expand: true)
      ()
  in
  let evt_box = GBin.event_box ~packing: wscroll#add_with_viewport () in
  let image = GMisc.image ~file: png_file ~packing:evt_box#add () in
  let _ = image#set_xalign 0.0 in
  let _ = image#set_yalign 0.0 in
  object(self)
    val mutable current_zoom = 100.0
    val mutable dot_width = 1
    val mutable dot_height = 1
    val mutable ids = []

    method virtual build_graph : Odot.graph
    method virtual refresh_data : unit
    method virtual on_button1_press : x: int -> y: int -> string option -> unit

    method private zoom_file_of_zoom zoom =
      Printf.sprintf "%s_%d%%.png" (Filename.chop_extension png_file) zoom

    method box = vbox

    method zoom () =
      let z =
        try Scanf.sscanf wcombo#entry#text "%d%%" (fun a -> Some a)
        with _ -> None
      in
      match z with
        None -> ()
      |	Some 100 ->
          current_zoom <- 100.0;
          image#set_file png_file
      |	Some z ->
          let f = self#zoom_file_of_zoom z in
          if Sys.file_exists f then
            image#set_file f
          else
            show image png_file f z;
          current_zoom <- float z

    method update_info =
      let (w,h,l) = analyse_annot_dot_file annot_dot_file in
      dot_width <- w;
      dot_height <- h;
      ids <- l

    method clean_files =
      List.iter (fun f -> try Sys.remove f with _ -> ())
        [ dot_file ; annot_dot_file ; png_file];
      List.iter (fun z -> try Sys.remove (self#zoom_file_of_zoom z) with _ -> ()) zooms;

    method refresh () =
      self#clean_files;
      self#refresh_data ;
      let g = self#build_graph in
      Odot.print_file dot_file g;
      let com = Printf.sprintf
        "%s -s%d -y %s > %s && %s -s%d -T png -o %s %s "
          (string_of_dot_program dot_program)
          (int_of_float default_dot_ppi)
          (Filename.quote dot_file)
          (Filename.quote annot_dot_file)
          (string_of_dot_program dot_program)
          (int_of_float default_dot_ppi)
          (Filename.quote png_file)
          (Filename.quote dot_file)
      in
      (
       match Sys.command com with
         0 ->
           self#update_info ;
           self#zoom ()
       | n -> GToolbox.message_box "Error"
           (Printf.sprintf "Exec error %d: %s" n com)
      );

    method private on_button1_press_cb x y =
      p_dbg (Printf.sprintf "Button 1 pressed ! x=%d y=%d" x y);
      let px = image#pixbuf in
      let dc =
        {
          Gobject.kind = `INT ;
          Gobject.proj = (function `INT n -> n | _ -> assert false) ;
          Gobject.inj = (fun n -> `INT n);
        }
      in
      let image_width = Gobject.Property.get px
        { Gobject.name = "width" ; Gobject.conv = dc }
      in
      let image_height = Gobject.Property.get px
        { Gobject.name = "height" ; Gobject.conv = dc }
      in
      let ratio_x = (float image_width) /. (float dot_width) in
      let ratio_y = (float image_height) /. (float dot_height) in
      p_dbg
        (Printf.sprintf "image width=%d height=%d ratio_x=%f ratio_y=%f"
         image_width image_height ratio_x ratio_y);
      let id_opt =
        p_dbg (Printf.sprintf "looking in %d ids" (List.length ids));
        let x = float x in
        let y = float y in
        try
          let (x1,y1,x2,y2,id) = List.find
            (fun (x1,y1,x2,y2,id) ->
               x1 *. ratio_x <= x && x <= x2 *. ratio_x &&
                 y1 *. ratio_y <= y && y <= y2 *. ratio_y
            )
              ids
          in
          p_dbg (Printf.sprintf
           "Id %s clicked pixels: x1=%f x2=%f y1=%f y2=%f ratio_x=%f ratio_y=%f"
             id
             (x1 *. ratio_x) (x2 *. ratio_x)
             (y1 *. ratio_y) (y2 *. ratio_y)
             ratio_x ratio_y
          );
          Some id
        with Not_found ->
          p_dbg "No id found";
          None
      in
      self#on_button1_press ~x ~y id_opt

    method on_button3_press x y =
      let entries = List.map
        (fun z ->
           let t = Printf.sprintf "%d%%" z in
           `I (t, fun () -> wcombo#entry#set_text t)
        )
          zooms
      in
      GToolbox.popup_menu ~entries ~button: 3 ~time: Int32.zero

    initializer
      ignore (vbox#connect#destroy (fun () -> self#clean_files));
      wcombo#entry#set_editable false;
      wcombo#entry#set_text "100%";
      ignore (wcombo#entry#connect#changed self#zoom );
      ignore (wb_refresh#connect#clicked self#refresh);
      ignore
        (evt_box#event#connect#button_press ~callback:
         (fun evt ->
            match GdkEvent.Button.button evt with
              1 ->
                GdkEvent.get_type evt = `BUTTON_PRESS &&
                  (
                   let x = int_of_float (GdkEvent.Button.x evt) in
                   let y = int_of_float (GdkEvent.Button.y evt) in
                   self#on_button1_press_cb x y;
                   true
                  )
            | 3 ->
                GdkEvent.get_type evt = `BUTTON_PRESS &&
                  (
                   let x = int_of_float (GdkEvent.Button.x evt) in
                   let y = int_of_float (GdkEvent.Button.y evt) in
                   self#on_button3_press x y;
                   true
                  )
            | n -> true
         )
        );
      if not (Sys.file_exists annot_dot_file) then
        self#refresh ()
      else
        (
         self#refresh_data;
         self#update_info
        )

  end

(*
module GCan = GnoCanvas;;

type text = {
  text_item : GCan.text ;
  text_fontsize : int ;
  }

type node = {
  node_item : GCan.base_item ;
  node_id : string ;
  node_text : text option;
}

let string_of_id = function
  Odot.Simple_id s -> s
| Odot.Double_quoted_id s -> s
| Odot.Html_id s -> s
;;

let attr_value name attr =
  try
    match Odot.attr_value (Odot.Simple_id name) attr with
      None -> raise Not_found
    | Some s -> string_of_id s
  with Not_found ->
      match Odot.attr_value (Odot.Double_quoted_id name) attr with
        None -> raise Not_found
      | Some s -> string_of_id s
;;

let attr_value_opt name attr =
  try Some (attr_value name attr)
  with Not_found -> None
;;

let attr_value_def name attr def =
  try attr_value name attr
  with Not_found -> def
;;

let map_opt f = function None -> None | Some x -> Some (f x);;

type props = {
  pr_shape : string ;
  pr_fillcolor : string option;
  pr_fontsize : int;
  }

let default_props = {
  pr_shape = "ellipse";
  pr_fillcolor = None ;
  pr_fontsize = 12;
  }

let get_label_position attrs =
  let f s =
    match split_string s [','] with
      [x;y] -> Some (float_of_string x, float_of_string y)
    | _ -> None
  in
  match attr_value_opt "lp" attrs with
    None -> None
  | Some s -> f s
;;

let node_of_odot_node f_click1 f_click3 group props id attrs =
  let id = Odot.string_of_id id in
  let w = float_of_string (attr_value "width" attrs) in
  let h = float_of_string (attr_value "height" attrs) in
  let (x,y) =
    match split_string (attr_value "pos" attrs) [','] with
      [x;y] ->
        (
         try (int_of_string x, - (int_of_string y))
         with | _ -> raise Not_found
        )
    | _ -> raise Not_found
  in
  let w = w *. default_dot_ppi in
  let h = h *. default_dot_ppi in
  let x1 = (float x) -. w /. 2.0 in
  let y1 = (float y) -. h /. 2.0 in

  let group = GnoCanvas.group ~x: x1 ~y: y1 group in
  let x1 = -. w /. 2. in
  let y1 = -. h /. 2. in
  let x2 = x1 +. w in
  let y2 = y1 +. h in
  let fun_of_shape = function
    "ellipse" -> GCan.ellipse
  | _ -> GCan.rect
  in
  let item =
    let f =
      match attr_value_opt "shape" attrs with
        Some s -> fun_of_shape s
      | None -> fun_of_shape props.pr_shape
    in
    let fill_color =
      match attr_value_opt "color" attrs with
        None -> props.pr_fillcolor
      | x -> x
    in
    f ~x1 ~y1 ~x2 ~y2 ?fill_color
      ~props: [`OUTLINE_COLOR "black"]
      group
  in
  let f_event = function
     `BUTTON_PRESS ev ->
      begin
        prerr_endline "click!";
        let x = int_of_float (GdkEvent.Button.x ev) in
        let y = int_of_float (GdkEvent.Button.y ev) in
        match GdkEvent.Button.button ev with
          1 -> f_click1 ~x ~y (Some id)
        | 3 -> f_click3 ~x ~y (Some id)
        | _ -> ()
      end;
      true
  | _ -> false
  in
  (* now the text *)
  let label =
    match attr_value_opt "label" attrs with
      Some s -> s
    | None -> id
  in
  let text =
    match label with
      "" -> None
    | text ->
        let g =
          match get_label_position attrs with
            None -> group
          | Some (x,y) ->
              GCan.group ~x ~y group
        in
        let item = GnoCanvas.text
          ~text ~font: "Times-Roman"
            ~props: [`SIZE_POINTS (float props.pr_fontsize)] g
        in
        let text = {
           text_item = item ;
           text_fontsize = props.pr_fontsize ;
          }
        in
        Some text
  in
  ignore(item#connect#event f_event);
  { node_item = (item :> GCan.base_item);
    node_id = id ;
    node_text = text ;
  }
;;

let props_of_graph_node_attr pr attrs =
  let props =
    [ "color", (fun p v -> { p with pr_fillcolor = Some v });
      "shape", (fun p v -> { p with pr_shape = v }) ;
      "fontsize", (fun p v -> { p with pr_fontsize = int_of_string v});
    ]
  in
  let f p (name, v) =
    try
      let f = List.assoc (string_of_id name) props in
      match map_opt string_of_id v with
        None -> p
      | Some v -> f p v
    with Not_found -> p
  in
  List.fold_left f pr attrs
;;


let get_graph_attrs stmt_list =
  let f acc = function
    Odot.Stmt_attr (Odot.Attr_graph l) -> l @ acc
  | _ -> acc
  in
  List.fold_left f [] stmt_list
;;

let create_subgraph f_click1 f_click3 group props g =
  let node =
    try
      let (x,y,w,h) = get_graph_bounding_box g.Odot.sub_stmt_list in
      let attrs = List.map
        (fun (s1,s2) -> (Odot.Simple_id s1, Some (Odot.Double_quoted_id s2)))
        [
          "height", string_of_int h;
          "width", string_of_int w;
          "pos", Printf.sprintf "%d,%d" x y ;
        ]
      in
      let id =
        match g.Odot.sub_id with None -> Odot.Simple_id "" | Some id -> id
      in
      node_of_odot_node f_click1 f_click3 group props id
          (attrs @ (get_graph_attrs g.Odot.sub_stmt_list))
    with _ ->
      let item = GCan.rect ~x1: 0.0 ~y1: 0. ~x2: 0. ~y2: 0. group in
        { node_item = (item :> GCan.base_item) ;
          node_id = "";
          node_text = None ;
        }
  in
  (node, group)
;;

let edges_of_odot_edges group props points attrs =
  []
;;

class virtual box  ?(dot_program=Dot) ~tmp_hash () =
  let dot_file = Printf.sprintf "%s.dot" tmp_hash in
  let annot_dot_file = Printf.sprintf "%s.dot_annot" tmp_hash in
  let vbox = GPack.vbox () in
  let hbox = GPack.hbox ~spacing:5 ~packing:(vbox#pack ~expand: false) () in
  let _ = GMisc.label ~text: "Zoom:" ~packing: (hbox#pack ~padding: 4 ~expand: false) () in
  let zooms =
    [ 10 ; 20 ; 30 ; 40 ; 50 ; 60 ; 70 ; 80 ; 90 ; 100 ; 120 ]
  in
  let wcombo = GEdit.combo
    ~popdown_strings: (List.map (fun s -> Printf.sprintf "%d%%" s) zooms)
      ~allow_empty:false
      ~enable_arrow_keys:true
      ~value_in_list:true
      ~packing: (hbox#pack ~expand: false)
      ()
  in
  let wb_refresh = GButton.button ~label: "Refresh"
    ~packing: (hbox#pack ~expand: false ~padding: 4) ()
  in
  let wscroll = GBin.scrolled_window
      ~packing: (vbox#pack ~expand: true)
      ~hpolicy: `AUTOMATIC
      ~vpolicy: `AUTOMATIC
      ()
  in
(*  let _evt_box = GBin.event_box ~packing: wscroll#add_with_viewport () in*)
  let canvas = GCan.canvas ~packing: wscroll#add () in
  let border = 10. in
  object(self)
    val mutable current_zoom = 1.0
    val mutable edges = []
    val mutable nodes = []
    val mutable graph = None

    method virtual build_graph : Odot.graph
    method virtual refresh_data : unit
    method virtual on_button1_press : x: int -> y: int -> string option -> unit

    method box = vbox

    method input_zoom () =
      let z =
        try Scanf.sscanf wcombo#entry#text "%d%%" (fun a -> Some a)
        with _ -> None
      in
      match z with
        None -> ()
      | Some n ->
          current_zoom <- (float n) /. 100.;
          canvas#set_pixels_per_unit current_zoom;
          self#resize_text_items

    method clean_files =
      List.iter (fun f -> try Sys.remove f with _ -> ())
        [ dot_file ; annot_dot_file ]

    method load_graph file =
      try
        graph <- Some (Odot.parse_file file)
      with Failure s -> GToolbox.message_box "Error" s

    method resize_text_items =
      let f_text t =
        let font_size = float t.text_fontsize *. current_zoom in
        if font_size <= 3.0 then
          t.text_item#hide ()
        else
          (
           t.text_item#set [`SIZE_POINTS font_size];
           t.text_item#show ();
          )
      in
      let f_node n =
        match n.node_text with
          None -> ()
        | Some t -> f_text t
      in
      List.iter f_node nodes

    method display () =
      List.iter
        (fun n ->
           n.node_item#destroy ();
           match n.node_text with Some t -> t.text_item#destroy () | None -> ()
        ) nodes;
      nodes <- [];
      match graph with
        None -> ()
      | Some g ->
          canvas#set_pixels_per_unit current_zoom;
          let (x1,y1,x2,y2) = get_graph_bounding_box g.Odot.stmt_list in
          let (y1,y2) = (-y1, -y2) in
          canvas#set_scroll_region
            ~x1:(float x1 -. border) ~y1:(float y1 -. border)
            ~x2:(float x2 +. border) ~y2:(float y2 +. border) ;

          let rec f_stmt group props = function
            [] -> ()
          | Odot.Stmt_node ((id,_), attrs) :: q ->
              let node = node_of_odot_node
                self#on_button1_press self#on_button3_press
                  group props id attrs
              in
              nodes <- node :: nodes;
              f_stmt group props q
          | Odot.Stmt_attr (Odot.Attr_node attrs) :: q ->
              let props = props_of_graph_node_attr props attrs in
              f_stmt group props q
          | Odot.Stmt_subgraph g2 :: q ->
              let (node, group2) = create_subgraph
                self#on_button1_press self#on_button3_press
                  group props g2
              in
              nodes <- node :: nodes;
              f_stmt group2 props g2.Odot.sub_stmt_list;
              f_stmt group props q
          | Odot.Stmt_edge (src, l, attrs) :: q ->
              let l_edges = edges_of_odot_edges group props (src :: l) attrs in
              edges <- l_edges @ edges;
              f_stmt group props q
          | _ :: q ->
              f_stmt group props q
          in
          f_stmt canvas#root default_props g.Odot.stmt_list;
          canvas#misc#show ()

    method refresh_dot () =
      self#clean_files;
      let g = self#build_graph in
      Odot.print_file dot_file g;
      let com = Printf.sprintf
        "%s -s%d -y %s > %s"
          (string_of_dot_program dot_program)
          (int_of_float default_dot_ppi)
          (Filename.quote dot_file)
          (Filename.quote annot_dot_file)
      in
      (
       match Sys.command com with
         0 ->
           self#load_graph annot_dot_file;
           self#display ()
       | n -> GToolbox.message_box "Error"
           (Printf.sprintf "Exec error %d: %s" n com)
      );

    method refresh () =
      self#refresh_data ;
      self#refresh_dot ()

    method on_button3_press ~x ~y _ =
      let entries = List.map
        (fun z ->
           let t = Printf.sprintf "%d%%" z in
           `I (t, fun () -> wcombo#entry#set_text t)
        )
          zooms
      in
      GToolbox.popup_menu ~entries ~button: 3 ~time: Int32.zero

    initializer
      ignore (vbox#connect#destroy (fun () -> self#clean_files));
      wcombo#entry#set_editable false;
      wcombo#entry#set_text "100%";
      ignore (wcombo#entry#connect#changed self#input_zoom );
      ignore (wb_refresh#connect#clicked self#refresh);
      ignore
        (canvas#event#connect#button_press ~callback:
         (fun evt ->
            match GdkEvent.Button.button evt with
            | 3 ->
                GdkEvent.get_type evt = `BUTTON_PRESS &&
                  (
                   let x = int_of_float (GdkEvent.Button.x evt) in
                   let y = int_of_float (GdkEvent.Button.y evt) in
                   self#on_button3_press x y None;
                   true
                  )
            | n -> false
         )
        );

      if not (Sys.file_exists annot_dot_file) then
        self#refresh ()
      else
        (
         self#refresh_data;
         self#load_graph annot_dot_file;
         self#display ()
        )

  end
*)