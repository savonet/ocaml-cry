
# check for one particular file of the sources 
AC_INIT([ocaml-cry],[0.6.2],[savonet-users@lists.sourceforge.net])

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT([configuring $PACKAGE_STRING])

AC_BASE_CHECKS()

AC_CHECK_OCAML_BINDING([bytes],[],[],[1])
INC="$INC `$OCAMLFIND query bytes`"

AC_CHECK_OCAML_BINDING([ssl],[0.5.0])
AC_CHECK_OCAML_BINDING([osx-secure-transport])

if test "$W_SSL" != ""; then
  SSL_BEGIN=""
  SSL_END=""
	requires="$requires ssl"
	INC="$INC `$OCAMLFIND query ssl`"
else
  SSL_BEGIN="(*"
  SSL_END="*)"
fi

if test "$W_OSX_SECURE_TRANSPORT" != ""; then
  SECURE_TRANSPORT_BEGIN=""
  SECURE_TRANSPORT_END=""
        requires="$requires osx-secure-transport"
        INC="$INC `$OCAMLFIND query osx-secure-transport`"
else
  SECURE_TRANSPORT_BEGIN="(*"
  SECURE_TRANSPORT_END="*)"
fi

# substitutions to perform
AC_SUBST(SSL_BEGIN)
AC_SUBST(SSL_END)
AC_SUBST(SECURE_TRANSPORT_BEGIN)
AC_SUBST(SECURE_TRANSPORT_END)
AC_SUBST(VERSION)
AC_SUBST(INC)
AC_SUBST(requires)

# Finally create the Makefile and samples
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([src/META])
AC_CONFIG_FILES([src/Makefile])
AC_CONFIG_FILES([src/cry_ssl.ml])
AC_CONFIG_FILES([examples/Makefile.connect],[chmod a-w examples/Makefile.connect])
AC_OUTPUT
