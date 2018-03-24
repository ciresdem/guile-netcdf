# Process this file with autoconf to produce a configure script

##################
# Package info
AC_INIT([Guile-NetCDF],[0.0.3],[matthew.love@colorado.edu])

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options -Wall])

AC_PREREQ(2.60)
AC_USE_SYSTEM_EXTENSIONS([_GNU_SOURCE])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_MACRO_DIR([m4])

##################
# Set the library libtool version constant
LIBGUILE_NETCDF_INTERFACE="11:1:0"
AC_SUBST(LIBGUILE_NETCDF_INTERFACE)

##################
# Check for Programs
PKG_PROG_PKG_CONFIG
AC_PROG_CC
AC_CANONICAL_HOST
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AM_PROG_AR

# from guile.m4
# sets: GUILE_CFLAGS and GUILE_LDFLAGS
GUILE_PKG([2.2 2.0])
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

AC_MSG_CHECKING([the Guile effective version])
guile_effective_version="`$GUILE -c '(display (effective-version))'`"
AC_MSG_RESULT([$guile_effective_version])

if test "x$GUILD" = "x"; then
   AC_MSG_WARN(['guild' binary not found; please check your guile-2.x installation.])
   AC_PATH_PROG([GUILD], guild)
fi

################
# Guile has three directories that don't follow the GNU Filesystem
# Heirarchy Standards.  If one follows the GNU FHS, files get installed
# in directories in which Guile does not search for them by default.
AC_ARG_WITH([gnu-filesystem-hierarchy],
            [AS_HELP_STRING([--with-gnu-filesystem-hierarchy],
                            [Strictly follow GNU Filesystem Heirarchy when installing, instead of querying Guile to discover the install directories that it expects.])])

case "x$with_gnu_filesystem_hierarchy" in
     xyes)
       # Follow the GNU Filesystem Heirarchy Standard correctly
       # Installed .scm scheme libraries go here
       AC_MSG_CHECKING([Guile site directory])
       guilesitedir="\$(datadir)/guile/site/$guile_effective_version"
       AC_MSG_RESULT([$guilesitedir])
       AC_SUBST(guilesitedir)

       # Compiled libguile-*.so extension libraries go here
       AC_MSG_CHECKING([Guile extension directory])
       guileextensiondir="$libdir/guile/$guile_effective_version"
       AC_MSG_RESULT([$guileextensiondir])
       AC_SUBST(guileextensiondir)

       # Compiled .go scheme libraries go here
       AC_MSG_CHECKING([Guile object directory])
       guileobjectdir="$libdir/guile/$guile_effective_version/site-ccache"
       AC_MSG_RESULT([$guileobjectdir])
       AC_SUBST(guileobjectdir)
       ;;
     *)
       # Get the installation directories from Guile
       # Installed .scm scheme libraries go here
       AC_MSG_CHECKING([Guile site directory])
       guilesitedir=$GUILE_SITE
       AC_MSG_RESULT([$guilesitedir])
       AC_SUBST(guilesitedir)

       # Compiled libguile-*.so extension libraries go here
       AC_MSG_CHECKING([Guile extension directory])
       guileextensiondir=$GUILE_EXTENSION
       AC_MSG_RESULT([$guileextensiondir])
       AC_SUBST(guileextensiondir)

       # Compiled .go scheme libraries go here
       AC_MSG_CHECKING([Guile object directory])
       guileobjectdir=$GUILE_SITE_CCACHE
       AC_MSG_RESULT([$guileobjectdir])
       AC_SUBST(guileobjectdir)
       ;;
esac

AC_CHECK_LIB([netcdf], nc_inq_libvers, [], [
		       echo "NetCDF is required..."
		       exit -1])

AC_CHECK_LIB([m], fabs, [], [
		  echo "Math library is required..."
		  exit -1])


##################
# Check for Libtool

# New method for when Libtool 2.x is default
LT_INIT([dlopen disable-fast-install disable-static win32-dll])


##################
# Initial guess of platform specific options

case $host_os in
mingw* | cygwin* | os2* | pw32* | cegcc*)
	[building_dll=yes] ;;
*)
	[building_dll=no] ;;
esac
case $host_os in
mingw* | os2* | pw32* | cegcc*)
	[building_shell=no] ;;
*)
	[building_shell=yes] ;;
esac
case $host_os in
mingw* )
	[dll_version_info=no] ;;
*)
	[dll_version_info=yes] ;;
esac

AM_CONDITIONAL([BUILDING_DLL], [test "$building_dll" = yes])
AM_CONDITIONAL([BUILDING_SHELL], [test "$building_shell" = yes])
AM_CONDITIONAL([DLL_VERSION_INFO], [test "$dll_version_info" = yes])

##################
# Check for libraries

AC_ARG_ENABLE([warnings],
              [AS_HELP_STRING([--enable-warnings],
                              [Build with most GCC warnings enabled])])
AM_CONDITIONAL([WARN], [test "$enable_warnings" = yes])

##################
# Check for header files
AC_CHECK_HEADERS([ctype.h])

##################
#  Output

AC_CONFIG_FILES([Makefile src/Makefile src/netcdf/Makefile modules/Makefile \
modules/netcdf/Makefile modules/netcdf/scripts/Makefile scripts/Makefile])

AC_OUTPUT

if test "$with_gnu_filesystem_hierarchy" = "yes"; then
   # Guile has a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in $guilesitedir.]
   [The Guile extension libraries will be installed in $guileextensiondir.]
   [The compiled modules will be installed in $guileobjectdir.]
   [This is because you ran configure with the `--with-gnu-filesystem-hierarchy' flag]
   []
   [Remember that you will have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
else
   AC_MSG_NOTICE([]
   [The Guile modules will be installed in $guilesitedir.]
   [The Guile extension libraries will be installed in $guileextensiondir.]
   [The compiled modules will be installed in $guileobjectdir.]
   [These directories are the ones expected by $GUILE])
fi
AC_SUBST([guilesitedir])

