## Process this file with automake to produce Makefile.in

guileextension_LTLIBRARIES= libguile-netcdf.la

libguile_netcdf_la_SOURCES = guile_netcdf.c

libguile_netcdf_la_CPPFLAGS = $(GUILE_CFLAGS)

if WARN
libguile_netcdf_la_CPPFLAGS += -Wall -Wextra -Wstrict-prototypes \
-Wmissing-prototypes -Wpointer-arith -Wreturn-type -Wcast-qual \
-Wswitch -Wshadow -Wcast-align -Wwrite-strings -Wchar-subscripts \
-Winline -Wnested-externs -Wredundant-decls -Wunused -Wformat=2
endif

libguile_netcdf_la_LDFLAGS = -no-undefined -version-info \
$(LIBGUILE_NETCDF_INTERFACE) $(GUILE_LDFLAGS)
