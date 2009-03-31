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
  | Connection 
  | Close
  | Write
  | Busy 
  | Not_connected
  | Invalid_usage
  | Bad_answer of string 
  | Http_answer of int*string*string

exception Error of error

val string_of_error : error -> string

type protocol = Icy | Http

type content_type
val ogg_application : content_type
val ogg_audio : content_type
val ogg_video : content_type
val mpeg : content_type
val content_type_of_string : string -> content_type
val string_of_content_type : content_type -> string

type connection =
  {
    mount        : string;
    user         : string;
    password     : string;
    host         : string;
    port         : int;
    content_type : content_type;
    protocol     : protocol;
    icy          : bool;
    headers      : (string, string) Hashtbl.t
  }

type audio_info = (string, string) Hashtbl.t

type metadata = (string,string) Hashtbl.t

type status = Connected of connection | Disconnected 

type t

val get_status : t -> status
val get_icy_cap : t -> bool

val create :
  ?ipv6:bool ->
  ?timeout:float -> 
  ?bind:string -> unit -> t

val audio_info :
  ?samplerate:int ->
  ?channels:int ->
  ?quality:float ->
  ?bitrate:int -> 
  unit -> audio_info

val connection :
  ?user_agent:string ->
  ?name:string ->
  ?genre:string ->
  ?url:string ->
  ?public:bool ->
  ?audio_info:audio_info ->
  ?description:string ->
  ?host:string ->
  ?port:int ->
  ?password:string ->
  ?protocol:protocol ->
  ?user:string ->
  ?icy:bool ->
  mount:string -> 
  content_type:content_type -> 
  unit -> connection

val connect : t -> connection -> unit

val update_metadata : t -> metadata -> unit

val send : t -> string -> unit

val close : t -> unit 

