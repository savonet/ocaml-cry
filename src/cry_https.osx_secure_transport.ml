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

let register fn =
  let connect_ssl ~host sock =
    let ctx =
      SecureTransport.init SecureTransport.Client SecureTransport.Stream
    in
    SecureTransport.set_connection ctx sock;
    SecureTransport.set_peer_domain_name ctx host;
    SecureTransport.handshake ctx;
    let close () =
      SecureTransport.close ctx;
      Unix.close sock
    in
    let wait_for operation delay =
      let events () =
        match operation with
          | `Read -> Unix.select [sock] [] [] delay
          | `Write -> Unix.select [] [sock] [] delay
          | `Both -> Unix.select [sock] [sock] [] delay
      in
      let r, w, _ = events () in
      match operation with
        | `Read -> r <> []
        | `Write -> w <> []
        | `Both -> r <> [] || w <> []
    in
    {
      write = SecureTransport.write ctx;
      read = SecureTransport.read ctx;
      wait_for;
      close;
    }
  in
  fn connect_ssl
