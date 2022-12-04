dnl Autoconf configure script for adafr

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/adafr.ads)

AM_GNAT_LIBRARY_PROJECT(adafr)

# Current release settings
ADAFR_MAJOR_VERSION=1
ADAFR_MINOR_VERSION=16
ADAFR_MICRO_VERSION=0
ADAFR_VERSION=$ADAFR_MAJOR_VERSION.$ADAFR_MINOR_VERSION.$ADAFR_MICRO_VERSION

# Set the version number of the project
AC_SUBST(ADAFR_VERSION)
AC_SUBST(ADAFR_MAJOR_VERSION)
AC_SUBST(ADAFR_MINOR_VERSION)
AC_SUBST(ADAFR_MICRO_VERSION)

ADAFR_LIBDIR="lib"
AC_SUBST(ADAFR_LIBDIR)

AM_ADA_UTIL_HARDWARE()
AM_ADA_UTIL_AWS_VERSION()

AC_CHECK_PROG(HAVE_PANDOC,pandoc,yes)
AC_CHECK_PROG(HAVE_DYNAMO,dynamo,yes)
AC_CHECK_PROG(HAVE_ARE,are,yes)

ROOT_DIR=`pwd`
GNAT_PROJECT_PATHS="$ROOT_DIR/awa/ada-util/.alire:$ROOT_DIR/awa/ada-util/.alire/xml:$ROOT_DIR/awa/ada-util/.alire/aws:$ROOT_DIR/awa/ada-util/.alire/unit"
GNAT_PROJECT_PATHS="$GNAT_PROJECT_PATHS:$ROOT_DIR/awa/ada-util:$ROOT_DIR/awa/ada-el/.alire:$ROOT_DIR/awa/ada-el:$ROOT_DIR/awa/ada-keystore/.alire"
GNAT_PROJECT_PATHS="$GNAT_PROJECT_PATHS:$ROOT_DIR/awa/ada-keystore:$ROOT_DIR/awa/ada-ado/.alire/sqlite:$ROOT_DIR/awa/ada-ado/.alire/mysql"
GNAT_PROJECT_PATHS="$GNAT_PROJECT_PATHS:$ROOT_DIR/awa/ada-ado/.alire/postgresql:$ROOT_DIR/awa/ada-ado/.alire/all:$ROOT_DIR/awa/ada-ado/.alire"
GNAT_PROJECT_PATHS="$GNAT_PROJECT_PATHS:$ROOT_DIR/awa/ada-ado:$ROOT_DIR/awa/ada-security/.alire:$ROOT_DIR/awa/ada-security:$ROOT_DIR/awa/ada-servlet/.alire"
GNAT_PROJECT_PATHS="$GNAT_PROJECT_PATHS:$ROOT_DIR/awa/ada-servlet/.alire/aws:$ROOT_DIR/awa/ada-servlet:$ROOT_DIR/awa/ada-asf/.alire:$ROOT_DIR/awa/ada-asf"
GNAT_PROJECT_PATHS="$GNAT_PROJECT_PATHS:$ROOT_DIR/awa/ada-wiki/.alire:$ROOT_DIR/awa/ada-wiki:$ROOT_DIR/awa/ada-wiki/.alire:$ROOT_DIR/awa/openapi-ada/.alire"
GNAT_PROJECT_PATHS="$GNAT_PROJECT_PATHS:$ROOT_DIR/awa/dynamo/.alire:$ROOT_DIR/awa/.alire:$ROOT_DIR/awa/awa"
GNAT_PROJECT_PATHS="$GNAT_PROJECT_PATHS:/usr/share/gpr"

AC_SUBST(GNAT_PROJECT_PATHS)

BUNDLE_DIR="$ROOT_DIR/awa/ada-asf/bundles"
AC_SUBST(BUNDLE_DIR)

DYNAMO_SEARCH_DIR="$ROOT_DIR/awa/awa/plugins"
AC_SUBST(DYNAMO_SEARCH_DIR)

# Check that Magick or convert are available but do not abort if they are not available
USE_CONVERT="-unused"
USE_MAGICK="-unused"
AC_CHECK_PROG(MAGICK_CHECK,magick,yes)
if test x"$MAGICK_CHECK" != x"yes" ; then
    AC_CHECK_PROG(CONVERT_CHECK,convert,yes)
    if test x"$CONVERT_CHECK" = x"yes" ; then
       USE_CONVERT=""
    fi
else
   USE_MAGICK=""
fi
AC_SUBST(USE_MAGICK)
AC_SUBST(USE_CONVERT)

AC_OUTPUT(
Makefile.conf
awa/dynamo/config/generator.properties
awa/awa/plugins/awa-images/config/images.xml)
