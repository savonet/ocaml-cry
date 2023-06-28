(lang dune 2.8)
(version 1.0.0)
(name cry)
(source (github savonet/ocaml-cry))
(license GPL-2.0-only)
(authors "Romain Beauxis <toots@rastageeks.org>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)
(use_standard_c_and_cxx_flags false)

(package
 (name cry)
 (synopsis "OCaml client for the various icecast & shoutcast source protocols")
 (description "The cry library is an implementation of the various icecast & shoutcast protocols to connect to streaming servers such as icecast")
 (depends (ocaml (>= 4.12.0)) dune base-bytes)
)
