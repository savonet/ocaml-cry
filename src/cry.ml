(*
 * Copyright 2003-2009 Savonet team
 *
 * This file is part of Ocaml-cry.
 *
 * Ocaml-cry is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-cry is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-cry; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

 (** OCaml low level implementation of the shout source protocol. *)

type error = 
  | Connect
  | Close
  | Write
  | Read
  | Busy
  | Not_connected
  | Invalid_usage 
  | Bad_answer of string 
  | Http_answer of int*string*string

exception Error of error

let string_of_error e = 
  match e with
    | Connect -> "could not connect to host"
    | Write -> "could not write data to host"
    | Read -> "could not read data from host"
    | Close -> "could not close connection"
    | Busy -> "busy"
    | Not_connected -> "not connected"
    | Invalid_usage -> "invalid usage"
    | Bad_answer s -> Printf.sprintf "bad answer: %s" s
    | Http_answer (c,x,v) -> 
       Printf.sprintf "%i, %s (HTTP/%s)" 
                      c x v

type protocol = Icy | Http

type content_type = string

let ogg_application = "application/ogg"
let ogg_audio = "audio/ogg"
let ogg_video = "video/ogg"
let mpeg = "audio/mpeg"
let content_type_of_string s = s
let string_of_content_type x = x

type connection =
  {
    mount        : string;
    user         : string;
    password     : string;
    host         : string;
    port         : int;
    content_type : content_type;
    protocol     : protocol;
    headers      : (string,string) Hashtbl.t
  }

type audio_info = (string,string) Hashtbl.t 

type metadata = (string,string) Hashtbl.t

type status = Connected of connection | Disconnected 

type t = 
  { 
    socket          : Unix.file_descr;
    ipv6            : bool;
    bind            : string option;
    mutable icy_cap : bool;
    mutable status  : status
  }

let create_socket ?(ipv6=false) ?(timeout=30.) ?bind () = 
  let domain = if ipv6 then Unix.PF_INET6 else Unix.PF_INET in
  let socket = Unix.socket domain Unix.SOCK_STREAM 0 in
  Unix.setsockopt_float socket Unix.SO_RCVTIMEO timeout;
  Unix.setsockopt_float socket Unix.SO_SNDTIMEO timeout;
  begin
   match bind with
     | None -> ()
     | Some s ->
        let bind_addr_inet = (Unix.gethostbyname s).Unix.h_addr_list.(0) in
        (* Seems like you need to bind on port 0 *)
        let bind_addr = Unix.ADDR_INET(bind_addr_inet, 0) in
        Unix.bind socket bind_addr ;
  end;
  socket

let create ?(ipv6=false) ?timeout ?bind () = 
  { 
    socket  = create_socket ~ipv6 ?timeout ?bind (); 
    ipv6    = ipv6;
    bind    = bind;
    icy_cap = false;
    status  = Disconnected
  }

let get_status c = c.status
let get_icy_cap c = c.icy_cap 
let get_socket c = c.socket

let audio_info 
  ?(samplerate) ?(channels) ?(quality) ?(bitrate) () = 
  let infos = Hashtbl.create 10 in
  let f m x y = 
    match y with
      | Some v -> Hashtbl.add infos x (m v)
      | None -> ()
  in
  f string_of_int "channels" channels;
  f string_of_int "samplerate" samplerate;
  f string_of_int "bitrate" bitrate;
  f string_of_float "quality" quality;
  infos

let connection 
    ?(user_agent) ?(name) ?(genre) 
    ?(url) ?(public) ?(audio_info) 
    ?(description) ?(host="localhost") 
    ?(port=8000) ?(password="hackme")
    ?(protocol=Http) ?(user="source")
    ~mount ~content_type () =
  let headers = Hashtbl.create 10 in
  let public = 
    match public with
      | Some x -> if x then Some "1" else Some "0"
      | None -> None
  in
  let mount = 
    if mount.[0] = '/' then 
      mount 
    else
      Printf.sprintf "/%s" mount
  in 
  let f (x,y) = 
    match y with
      | Some v -> Hashtbl.add headers x v 
      | None -> ()
  in
  let x = 
    match protocol with
      | Http -> 
         let audio_info =
           match audio_info with
             | Some x ->
                 let f x y z =
                   let z = if z <> "" then z ^ ";" else z in
                   Printf.sprintf "%s%s=%s" z x y
                 in
                 Some (Hashtbl.fold f x "")
            | None -> None
         in
         ["User-Agent", user_agent;
          "ice-name", name;
          "ice-genre", genre;
          "ice-url", url;
          "ice-public", public;
          "ice-audio-info", audio_info;
          "ice-description", description]
      | Icy -> 
         ["icy-name", name;
          "icy-url", url;
          "icy-pub", public;
          "icy-genre", genre;
          "icy-br", match audio_info with
                      | None -> None
                      | Some x -> 
                          begin
                           try
                             Some (Hashtbl.find x "bitrate")
                           with
                            | Not_found -> None
                          end]
  in
  List.iter f x;
  {
    host = host;
    port = port;
    user = user;
    password = password;
    mount = mount;
    content_type  = content_type;
    protocol = protocol;
    headers = headers
  }


(** Base 64 encoding. *)
let encode64 s =
  let digit =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  in
  let extra = String.length s mod 3 in
  let s = match extra with 1 -> s ^ "\000\000" | 2 -> s ^ "\000" | _ -> s in
  let n = String.length s in
  let dst = String.create (4 * (n/3)) in
    for i = 0 to n/3 - 1 do
      let (:=) j v = dst.[i*4+j] <- digit.[v] in
      let c j = int_of_char s.[i*3+j] in
      let c0 = c 0 and c1 = c 1 and c2 = c 2 in
        0 := c0 lsr 2 ;
        1 := ((c0 lsl 4) land 63) lor (c1 lsr 4) ;
        2 := ((c1 lsl 2) land 63) lor (c2 lsr 6) ;
        3 := c2 land 63
    done ;
    if extra = 1 then begin
      dst.[4*(n/3)-2] <- '=' ;
      dst.[4*(n/3)-1] <- '='
    end else if extra = 2 then
      dst.[4*(n/3)-1] <- '=' ;
    dst

(* URL encoding/decoging according to RFC 1738, RFC 1630.
 * Borrowed from ocamlnet. *)

(** Converts k to a 2-digit hexadecimal string. *)
let to_hex2 =
  let hex_digits =
    [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
       '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]
  in
    fun k ->
      let s = String.create 2 in
        s.[0] <- hex_digits.( (k lsr 4) land 15 ) ;
        s.[1] <- hex_digits.( k land 15 ) ;
        s

let url_encode s = 
 let rec do_url_encode s s' =
   (** True if x is an acceptable char *)
   let range x = 
     (* 0-9 *)
     (x <= 0x30 && x >= 0x39) ||
     (* A-Z *)
     (x <= 0x41 && x >= 0x5A) ||
     (* a-z *)
     (x >= 0x61 && x <= 0x7a) ||
     (* _ *)
     x = 0x5f ||
     (* . *)
     x = 0x2e ||
     (* ! *)
     x = 0x21 ||
     (* * *)
     x = 0x2a ||
     (* - *)
     x = 0x2d
   in
   match String.length s with
     | 0 -> s'
     | l -> 
       let x = Char.code s.[0] in
       let s' =
         if not (range x) then 
           Printf.sprintf "%s%%%s" s' (to_hex2 x)
          else
           Printf.sprintf "%s%c" s' s.[0]
       in
       do_url_encode (String.sub s 1 (l-1)) s'
 in
 do_url_encode s ""

let http_header =
  Printf.sprintf "SOURCE %s HTTP/1.1\r\n%s\r\n\r\n"

let get_auth source = 
  Printf.sprintf "Basic %s" (encode64 (source.user ^ ":" ^ source.password))

let write_data socket request raise = 
  let len = String.length request in
  if Unix.write socket request 0 len < len then
    raise (Error Write)

let read_data socket raise =
  let in_c = Unix.in_channel_of_descr socket in  
  try
    input_line in_c
  with
     | End_of_file -> raise (Error Read)

let parse_http_answer s = 
  let f v c s =
    (v,c,s)
  in
  try
    Scanf.sscanf s "HTTP/%s %i %[^\r^\n]" f
  with
    | Scanf.Scan_failure s -> raise (Error (Bad_answer s))

let connect_http c source = 
  let auth = get_auth source in 
  let raise x =
    begin 
     try
       Unix.shutdown c.socket Unix.SHUTDOWN_ALL;
     with
       | _ -> ()
    end;
    raise x
  in
  Hashtbl.add source.headers "Authorization" auth;
  Hashtbl.add 
   source.headers 
       "Content-type" (string_of_content_type source.content_type);
  let f x y z = 
    Printf.sprintf "%s%s: %s\r\n" z x y
  in
  let headers = Hashtbl.fold f source.headers "" in
  let request = http_header source.mount headers in
  write_data c.socket request raise;
  (** Read input from socket. *)
  let ret = read_data c.socket raise in 
  let (v,code,s) = parse_http_answer ret in
  if code < 200 || code >= 300 then
    raise (Error (Http_answer (code,s,v)));
  c.icy_cap <- true; 
  c.status <- Connected source 

let connect_icy c source = 
  let f x y z =
    Printf.sprintf "%s%s: %s\r\n" z x y
  in
  let headers = Hashtbl.fold f source.headers "" in
  let request = Printf.sprintf "%s\r\n%s\r\n" source.password headers in
  let raise x =
    begin
     try
       Unix.shutdown c.socket Unix.SHUTDOWN_ALL;
     with
       | _ -> ()
    end;
    raise x
  in
  write_data c.socket request raise;
  (** Read input from socket. *)
  let ret = read_data c.socket raise in
  let f s =
    if s.[0] <> 'O' && s.[1] <> 'K' then
      raise (Error (Bad_answer s));
  in
  begin
   try
     Scanf.sscanf ret "%[^\r^\n]" f
   with
     | Scanf.Scan_failure s -> raise (Error (Bad_answer s))
  end;
  (* Read another line *)
  let ret = read_data c.socket raise in
  let f s = 
    c.icy_cap <- true
  in
  begin
   try
     Scanf.sscanf ret "icy-caps:%[1]" f
   with
     | Scanf.Scan_failure s -> ()
  end;
  c.status <- Connected source

let connect_socket socket host port = 
  try
    Unix.connect
      socket
      (Unix.ADDR_INET((Unix.gethostbyname host).Unix.h_addr_list.(0),port))
  with
    | _ -> raise (Error Connect)
 
let connect c source =
  if c.status <> Disconnected then
    raise (Error Busy);
  let port = 
    if source.protocol = Icy then source.port+1 else source.port 
  in
  connect_socket c.socket source.host port;
  match source.protocol with
    | Http -> connect_http c source
    | Icy -> connect_icy c source

let http_meta_request = 
  Printf.sprintf 
    "GET /admin/metadata?mode=updinfo&mount=%s%s HTTP/1.0\r\n%s\r\n"

let icy_meta_request = 
  Printf.sprintf 
    "GET /admin.cgi?mode=updinfo&pass=%s%s HTTP/1.0\r\n%s\r\n"

let update_metadata c m = 
  let source = 
    match c.status with
      | Connected x -> x 
      | _ -> raise (Error Not_connected)
  in
  if not c.icy_cap then
    raise (Error Invalid_usage);
  let socket = create_socket ~ipv6:c.ipv6 ?bind:c.bind () in
  let raise x =
    begin
     try
       Unix.shutdown c.socket Unix.SHUTDOWN_ALL;
       Unix.close socket
     with
       | _ -> ()
    end;
    raise x
  in
  connect_socket socket source.host source.port;
  let user_agent = 
    try
      Printf.sprintf "User-Agent: %s\r\n"
         (Hashtbl.find source.headers "User-Agent")
    with
      | Not_found -> ""
  in
  let f x y z =
    Printf.sprintf "%s&%s=%s" z (url_encode x) (url_encode y)
  in
  let meta = Hashtbl.fold f m "" in
  let request = 
    match source.protocol with
      | Http ->
          let headers = 
            Printf.sprintf "Authorization: %s\r\n%s" (get_auth source) user_agent
          in
          http_meta_request source.mount meta headers
      | Icy -> icy_meta_request source.password meta user_agent
  in
  write_data socket request raise;
  (** Read input from socket. *)
  let ret = read_data socket raise in
  let (v,code,s) = parse_http_answer ret in
  if code <> 200 then
    raise (Error (Http_answer (code,s,v)));
  try
    Unix.shutdown socket Unix.SHUTDOWN_ALL;
    Unix.close socket;
  with
    | _ -> raise (Error Close)

let send c x = 
  let out_e = Unix.out_channel_of_descr c.socket in
  output_string out_e x;
  flush out_e

let close x = 
    try
      Unix.shutdown x.socket Unix.SHUTDOWN_ALL;
      Unix.close x.socket;
      x.status <- Disconnected
    with
      | _ -> raise (Error Close)
 
