(*
 * Copyright 2003-2016 Savonet team
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

open Cry_common

let () =
  Ssl_threads.init ();
  Ssl.init ()

let register fn =
  let connect_ssl ~host:hostname socket =
    let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
    (* SSL_VERIFY_NONE is default in shout. TODO: add option.. *)
    Ssl.set_verify ctx [] (Some Ssl.client_verify_callback);
    Ssl.set_verify_depth ctx 3;
    ignore (Ssl.set_default_verify_paths ctx);
    let ssl = Ssl.embed_socket socket ctx in
    Ssl.set_client_SNI_hostname ssl hostname; 
    Ssl.connect ssl;
    let shutdown () =
      Ssl.shutdown ssl;
      Unix.close (Ssl.file_descr_of_socket ssl)
    in
    let wait_for operation delay =
      let socket = Ssl.file_descr_of_socket ssl in
      let events () =
        match operation with
          | `Read -> Unix.select [socket] [] [] delay
          | `Write -> Unix.select [] [socket] [] delay
          | `Both -> Unix.select [socket] [socket] [] delay
      in
      let r, w, _ = events () in
      match operation with
        | `Read -> r <> []
        | `Write -> w <> []
        | `Both -> r <> [] || w <> []
    in
    { write = Ssl.write ssl; read = Ssl.read ssl; wait_for; close = shutdown }
  in
  fn connect_ssl
