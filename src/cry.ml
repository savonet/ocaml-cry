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
  | Create of exn
  | Connect of exn
  | Close of exn
  | Write of exn
  | Read of exn
  | Busy
  | Not_connected
  | Invalid_usage
  | Bad_answer of string option 
  | Http_answer of int*string*string

exception Error of error
exception Timeout

let rec string_of_error = 
     function
      | Error (Create e) -> pp "could not initiate a new handler" e
      | Error (Connect e) -> pp "could not connect to host" e
      | Error (Write e) -> pp "could not write data to host" e
      | Error (Read e) -> pp "could not read data from host" e
      | Error (Close e) -> pp "could not close connection" e
      | Error Busy -> "busy"
      | Error Not_connected -> "not connected"
      | Error Invalid_usage -> "invalid usage"
    (*  | Unix.unix_error (code,name,param) ->
          Printf.sprintf "%s in %s(%s)" (Unix.error_message code)
                                         name param *)
      | Timeout -> "connection timeout"
      | Error (Bad_answer s) -> 
           Printf.sprintf "bad answer%s" 
             (match s with
                | Some s -> Printf.sprintf ": %s" s
                | None   -> "")
      | Error (Http_answer (c,x,v)) -> 
         Printf.sprintf "%i, %s (HTTP/%s)" 
                       c x v
      | e -> Printexc.to_string e
and
  pp s e =
    Printf.sprintf "%s: %s" s (string_of_error e)

type verb = Put | Post | Source

let string_of_verb = function
  | Put -> "PUT"
  | Post -> "POST"
  | Source -> "SOURCE"

type protocol = Icy | Http of verb

let string_of_protocol = function
  | Icy    -> "icy"
  | Http v -> Printf.sprintf "http(%s)" (string_of_verb v)

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
    chunked      : bool;
    content_type : content_type;
    protocol     : protocol;
    headers      : (string,string) Hashtbl.t
  }

let string_of_connection c = 
  Printf.sprintf 
    "{ \"mount\": %S,\n\
       \"user\":  %S,\n\
       \"password\": %S,\n\
       \"host\": %S,\n\
       \"port\": %d,\n\
       \"chunked\": %b,\n\
       \"content_type\": %S,\n\
       \"protocol\": %S,\n\
       \"headers\": { %s } }"
     c.mount
     c.user
     c.password
     c.host
     c.port
     c.chunked
     (string_of_content_type c.content_type)
     (string_of_protocol c.protocol)
     (Hashtbl.fold (fun x y z -> Printf.sprintf "%S: %S,\n%s" x y z)
                   c.headers "")

type audio_info = (string,string) Hashtbl.t 

type metadata = (string,string) Hashtbl.t

(* Metadata socket is only present if icy_cap is true *)
type connection_data = 
  { connection      : connection;
    data_socket     : Unix.file_descr }

type status_priv = PrivConnected of connection_data | PrivDisconnected 

type status = Connected of connection_data | Disconnected

type t = 
  { 
    ipv6               : bool;
    timeout            : float;
    connection_timeout : float option;
    bind               : string option;
    mutable icy_cap    : bool;
    mutable status     : status_priv;
    mutable chunk_cap  : bool;
  }

let get_connection_data x =
  match x.status with
    | PrivConnected d -> d
    | PrivDisconnected -> raise (Error Not_connected)

let create_socket ?(ipv6=false) ?bind () = 
  let domain = if ipv6 then Unix.PF_INET6 else Unix.PF_INET in
  let socket = 
    try
      Unix.socket domain Unix.SOCK_STREAM 0
    with
      | e -> raise (Error (Create e)) 
  in
  try
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
  with
    | e -> 
       begin
         try
           Unix.close socket
         with
           | _ -> ()
       end;
       raise (Error (Create e))

let create ?(ipv6=false) ?bind ?connection_timeout ?(timeout=30.) () = 
  { 
    ipv6               = ipv6;
    timeout            = timeout;
    connection_timeout = connection_timeout;
    bind               = bind;
    icy_cap            = false;
    status             = PrivDisconnected;
    chunk_cap          = false;
  }

(* Returns [false] if delay has been expired
 * while waiting for the requested operation. *)
let wait_for operation delay s =
  let events () =
   match operation with
     | `Read ->
          Unix.select [s] [] [] delay
     | `Write ->
          Unix.select [] [s] [] delay
     | `Both ->
          Unix.select [s] [s] [] delay
  in
  let r,w,_ = events () in
  match operation with
    | `Read -> r <> []
    | `Write -> w <> []
    | `Both -> r <> [] || w <> []

let write_data ~timeout socket request =
  let len = String.length request in
  let rec write ofs =
    if not (wait_for `Write timeout socket) then
      raise Timeout ;
    let rem = len - ofs in
    if rem > 0 then
      let ret = Unix.write socket request ofs rem in
      if ret = 0 then
        raise (Failure "connection closed.") ;
      if ret < rem then
        write (ofs+ret)
  in
  try
    write 0
  with
    | e -> raise (Error (Write e))

let close x =
    try
      let c = get_connection_data x in
      if x.chunk_cap then
        write_data ~timeout:x.timeout c.data_socket "0\r\n\r\n";
      Unix.close c.data_socket ;
      x.chunk_cap <- false;
      x.icy_cap <- false;
      x.status <- PrivDisconnected
    with
      | Error _ as e -> raise e
      | e -> raise (Error (Close e))

let get_status c = 
  match c.status with
    | PrivConnected d -> Connected d
    | PrivDisconnected -> Disconnected

let get_icy_cap c = c.icy_cap 

let audio_info 
  ?(samplerate) ?(channels) ?(quality) ?(bitrate) () = 
  let infos = Hashtbl.create 10 in
  let f m x y = 
    match y with
      | Some v -> Hashtbl.replace infos x (m v)
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
    ?(port=8000) ?(chunked=false)
    ?(password="hackme") ?(protocol=(Http Source))
    ?(user="source") ~mount ~content_type () =
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
      | Some v -> Hashtbl.replace headers x v 
      | None -> ()
  in
  let x = 
    match protocol with
      | Http _ -> 
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
    chunked = chunked;
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
     (x >= 0x30 && x <= 0x39) ||
     (* A-Z *)
     (x >= 0x41 && x <= 0x5A) ||
     (* a-z *)
     (x >= 0x61 && x <= 0x7a)
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
  Printf.sprintf "%s %s HTTP/1.%d\r\n%s\r\n\r\n"

let get_auth user password = 
  Printf.sprintf "Basic %s" (encode64 (user ^ ":" ^ password))

let buf = String.create 1024

(** Read and split data. 
  * There should always be at least
  * one element in the resulting list.
  * If not, something bad happened. *)
(* TODO: review all reading code as this 
 * seems a bit ad-hoc/naive.. *)
let read_data ~timeout socket =
  if not (wait_for `Read timeout socket) then
    raise Timeout ;
  try
     let ret = Unix.read socket buf 0 1024 in
     (* split data *)
     let buf = String.sub buf 0 ret in
     let rec f pos l = 
       try
         let x = String.index_from buf pos '\n' in
         f (x+1) (String.sub buf pos (x-pos-1)::l)
       with
         | Invalid_argument _ 
         | Not_found -> 
             if pos < ret then 
               String.sub buf pos (ret-pos) :: l 
             else 
               l
     in
     let ret = f 0 [] in
     if List.length ret = 0 then
       [""]
     else
       List.rev (f 0 [])
  with
     | e -> raise (Error (Read e)) 

let header_string headers source =
    let unique_headers = Hashtbl.create 10 in
    Hashtbl.iter (Hashtbl.replace unique_headers) headers;
    (* Icy headers are of the form: %s:%s *)
    let sep = 
      if source.protocol = Icy then
        ""
      else
        " "
    in
    (* "content-type" seems to be in lower case
     * for shoutcast. Also, it seems that
     * it is good to pass it last, see:
     *   http://forums.winamp.com/showthread.php?threadid=285035 *)
    let content_label =
      if source.protocol = Icy then
        "content-type"
      else
        "Content-Type"
    in
    let f x y z =
      Printf.sprintf "%s:%s%s" x sep y :: z
    in
    let headers = Hashtbl.fold f unique_headers 
                               (f content_label 
                                  source.content_type []) 
    in
    String.concat "\r\n" headers

let parse_http_answer s = 
  let f v c s =
    (v,c,s)
  in
  try
    Scanf.sscanf s "HTTP/%s %i %[^\r^\n]" f
  with
    | Scanf.Scan_failure s -> raise (Error (Bad_answer (Some s)))
    | _ -> raise (Error (Bad_answer None))

let add_host_header headers host port =
  try
    ignore(Unix.inet_addr_of_string host)
  with
    | Failure _ ->
        let host = if port = 80 then host else
          Printf.sprintf "%s:%d" host port
        in
        Hashtbl.replace headers "Host" host

let connect_http c socket source verb = 
  let auth = get_auth source.user source.password in 
  try
    let headers = Hashtbl.copy source.headers in
    Hashtbl.replace headers "Authorization" auth;
    add_host_header headers source.host source.port;
    if source.chunked then Hashtbl.replace headers "Transfer-Encoding" "chunked";
    let headers = header_string headers source in
    let request = http_header (string_of_verb verb) source.mount 
      (if source.chunked then 1 else 0) headers
    in
    write_data ~timeout:c.timeout socket request;
    (** Read input from socket. *)
    let ret = read_data ~timeout:c.timeout socket in 
    let (v,code,s) = parse_http_answer (List.hd ret) in
    if code < 200 || code >= 300 then
      raise (Error (Http_answer (code,s,v)));
    c.chunk_cap <- v = "1.1";
    c.icy_cap <- true;
    c.status <- 
      PrivConnected { connection = source;
                      data_socket = socket }
  with
    | e -> 
       begin
        try
          close c
        with
          | _ -> ()
       end ;
       raise e

let connect_icy c socket source = 
  let request = Printf.sprintf "%s\r\n" source.password in
  try
    write_data ~timeout:c.timeout socket request;
    (** Read input from socket. *)
    let ret = read_data ~timeout:c.timeout socket in
    let f s =
      if s.[0] <> 'O' && s.[1] <> 'K' then
        raise (Error (Bad_answer (Some s)));
    in
    begin
     try
       Scanf.sscanf (List.hd ret) "%[^\r^\n]" f
     with
       | Scanf.Scan_failure s -> raise (Error (Bad_answer (Some s)))
       | Error _ as e -> raise e
       | e -> raise (Error (Bad_answer None))
    end;
    (* Read another line *)
    let ret = 
      match ret with
        | x :: y :: _ -> y
        | _ -> List.hd (read_data ~timeout:c.timeout socket)
    in
    let f s =
      c.icy_cap <- true
    in
    begin
     try
       Scanf.sscanf ret "icy-caps:%[1]" f
     with
       | Scanf.Scan_failure s -> ()
    end;
    (* Now Write headers *)
    let headers = header_string source.headers source in
    let request = Printf.sprintf "%s\r\n\r\n" headers in
    write_data ~timeout:c.timeout socket request;
    c.status <- 
      PrivConnected { connection = source;
                      data_socket = socket }
  with
    | e ->
       begin
        try
          close c
        with
          | _ -> ()
       end ;
       raise e

(** This function does *not* close the socket in case of error.. *)   
let connect_socket ?timeout socket host port = 
  let do_timeout = timeout <> None in
  let check_timeout () =
    match timeout with
      | Some timeout ->
         (* Block in a select call for [timeout] seconds. *)
         let _, w, _ = Unix.select [] [socket] [] timeout in
         if w = [] then
           raise (Error (Connect Timeout)) ;
         Unix.clear_nonblock socket
       | None -> assert false
  in
  try
    if do_timeout then
      Unix.set_nonblock socket ;
    Unix.connect 
      socket 
      (Unix.ADDR_INET((Unix.gethostbyname host).Unix.h_addr_list.(0),port)) ;
    if do_timeout then
      Unix.clear_nonblock socket
  with
    | Unix.Unix_error(Unix.EINPROGRESS, _, _) -> check_timeout ()
    | Unix.Unix_error(Unix.EWOULDBLOCK, _, _) when Sys.os_type = "Win32" ->
        check_timeout ()
    | e -> raise (Error (Connect e))
 
let connect c source =
  if c.status <> PrivDisconnected then
    raise (Error Busy);
  let port = 
    if source.protocol = Icy then source.port+1 else source.port 
  in
  let socket = create_socket ~ipv6:c.ipv6
                             ?bind:c.bind ()
  in
  try
    connect_socket ?timeout:c.connection_timeout socket source.host port;
    (* We do not know icy capabilities so far.. *)
    c.icy_cap <- false;
    match source.protocol with
      | Http verb -> connect_http c socket source verb
      | Icy -> connect_icy c socket source
  with
    | e -> 
       begin
        try
         Unix.close socket
        with
          | _ -> ()
       end; 
       raise e

let http_meta_request mount charset meta headers = 
  let unique_headers = Hashtbl.create 10 in
  Hashtbl.iter (Hashtbl.replace unique_headers) headers;
  let header = 
     Hashtbl.fold (Printf.sprintf "%s: %s\r\n%s")
                   unique_headers ""
  in
  Printf.sprintf 
    "GET /admin/metadata?mode=updinfo&mount=%s%s%s HTTP/1.0\r\n%s\r\n"
     mount charset meta header

let icy_meta_request = 
  Printf.sprintf 
    "GET /admin.cgi?mode=updinfo&pass=%s%s HTTP/1.0\r\n%s\r\n"

let manual_update_metadata 
       ~host ~port ~protocol ~user ~password 
       ~mount ?(connection_timeout=5.) ?(timeout=30.) ?headers ?(ipv6=false)
       ?bind ?charset m =
  let mount = 
    if mount.[0] <> '/' then
      "/" ^ mount
    else
      mount
  in 
  let headers = 
    match headers with
      | Some x -> Hashtbl.copy x
      | None   -> Hashtbl.create 0
  in
  let socket = create_socket ~ipv6 ?bind () in
  let close () = 
    try
      Unix.close socket
    with
      | e -> raise (Error (Close e))
  in
  try
    connect_socket ~timeout:connection_timeout socket host port;
    let charset = 
      match charset with
        | Some c -> Printf.sprintf "&charset=%s" c
        | None   -> ""
    in
    let f x y z =
      if y <> "" then
        Printf.sprintf "%s&%s=%s" z (url_encode x) (url_encode y)
      else
        z
    in
    let meta = 
      Hashtbl.fold f m ""
    in
    let request = 
      match protocol with
        | Http _ ->
            Hashtbl.replace headers "Authorization" (get_auth user password);
            add_host_header headers host port;
            http_meta_request mount charset meta headers
        | Icy ->
            let user_agent =
              try
                Hashtbl.find headers "User-Agent"
               with
                | Not_found -> "ocaml-cry"
            in
            let user_agent =
              Printf.sprintf "User-Agent: %s (Mozilla compatible)\r\n" user_agent
            in
            icy_meta_request password meta user_agent
    in
    write_data ~timeout socket request;
    (** Read input from socket. *)
    let ret = read_data ~timeout socket in
    let (v,code,s) = parse_http_answer (List.hd ret) in
    if code <> 200 then
      raise (Error (Http_answer (code,s,v)));
    close ()
  with
    | e -> 
       begin
        try
         close ()
        with
          | _ -> () 
       end ;
       raise e

let update_metadata ?charset c m = 
  if not c.icy_cap then                                                     
    raise (Error Invalid_usage);
  let data = get_connection_data c in
  let source = data.connection in
  let user = source.user in
  let port = source.port in
  let password = source.password in
  let headers = Some source.headers in
  let protocol = source.protocol in
  let mount = source.mount in
  let host = source.host in
  let timeout = c.timeout in
  let connection_timeout = c.connection_timeout in
  manual_update_metadata 
       ~host ~port ~protocol 
       ~user ~password ~timeout
       ~mount ?headers ?connection_timeout
       ?charset m

let send c x =
  let d = get_connection_data c in
  if c.chunk_cap then
   begin
    if x <> "" then
      let x =
        Printf.sprintf "%X\r\n%s\r\n" (String.length x) x
       in
       write_data ~timeout:c.timeout d.data_socket x
   end
  else 
    write_data ~timeout:c.timeout d.data_socket x

