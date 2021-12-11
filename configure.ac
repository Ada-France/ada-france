dnl Autoconf configure script for adafr

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/adafr.ads)

AM_GNAT_LIBRARY_PROJECT(adafr)

# Current release settings
ADAFR_MAJOR_VERSION=1
ADAFR_MINOR_VERSION=13
ADAFR_MICRO_VERSION=0
ADAFR_VERSION=$ADAFR_MAJOR_VERSION.$ADAFR_MINOR_VERSION.$ADAFR_MICRO_VERSION

# Set the version number of the project
AC_SUBST(ADAFR_VERSION)
AC_SUBST(ADAFR_MAJOR_VERSION)
AC_SUBST(ADAFR_MINOR_VERSION)
AC_SUBST(ADAFR_MICRO_VERSION)

ADAFR_LIBDIR="lib"
AC_SUBST(ADAFR_LIBDIR)

AC_OUTPUT(
Makefile.conf
)
