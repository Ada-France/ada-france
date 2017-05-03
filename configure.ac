dnl Autoconf configure script for adafr
dnl Process this file with autoconf to produce a configure script.
AC_INIT(src/adafr.ads)
AC_CANONICAL_SYSTEM
dnl AM_MAINTAINER_MODE

# Current release settings
ADAFR_MAJOR_VERSION=0
ADAFR_MINOR_VERSION=1
ADAFR_MICRO_VERSION=0
ADAFR_VERSION=$ADAFR_MAJOR_VERSION.$ADAFR_MINOR_VERSION.$ADAFR_MICRO_VERSION
AM_GNAT_CHECK_GPRBUILD
# checking for local tools
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S

AC_CHECK_PROGS(GNATMAKE, gnatmake, "")

# Set the version number of GtkAda
AC_SUBST(ADAFR_VERSION)
AC_SUBST(ADAFR_MAJOR_VERSION)
AC_SUBST(ADAFR_MINOR_VERSION)
AC_SUBST(ADAFR_MICRO_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)
ADAFR_LIBDIR="lib"
AC_SUBST(ADAFR_LIBDIR)

AC_MSG_CHECKING([number of processors])
NR_CPUS=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf NPROCESSORS_CONF 2>/dev/null || echo 1`
AC_MSG_RESULT($NR_CPUS)
AC_SUBST(NR_CPUS)
##########################################
# Check to enable/disable shared library
##########################################
AM_SHARED_LIBRARY_SUPPORT

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[util],
  [git@github.com:stcarrez/ada-util.git],
  [Building adafr requires the Ada Utility Library.],
  [
    UTIL_DIR=
  ])
AC_SUBST(UTIL_DIR)

##########################################
# Ada Expression Language library
##########################################
AM_GNAT_FIND_PROJECT([ada-el],[Ada Expression Language Library],[el],
  [git@github.com:stcarrez/ada-el.git],
  [Building adafr requires the Ada EL Library.],
  [
    EL_DIR=
  ])
AC_SUBST(EL_DIR)

##########################################
# Ada Server Faces library
##########################################
AM_GNAT_FIND_PROJECT([ada-asf],[Ada Server Faces],[asf],
  [git@github.com:stcarrez/ada-asf.git],
  [Building adafr requires the Ada Server Faces Library.],
  [
    ASF_DIR=
  ])
AC_SUBST(ASF_DIR)

##########################################
# Ada Security library
##########################################
AM_GNAT_FIND_PROJECT([ada-security],[Ada Security Library],[security],
  [git@github.com:stcarrez/ada-security.git],
  [Building adafr requires the Ada Security Library.],
  [
    SECURITY_DIR=
  ])
AC_SUBST(SECURITY_DIR)

##########################################
# Ada Database Objects library
##########################################
AM_GNAT_FIND_PROJECT([ada-ado],[Ada Database Objects],[ado],
  [git@github.com:stcarrez/ada-ado.git],
  [Building adafr requires the Ada Database Objects Library.],
  [
    ADO_DIR=
  ])
AC_SUBST(ADO_DIR)

##########################################
# Ada Web Application library
##########################################
AM_GNAT_FIND_PROJECT([ada-awa],[Ada Web Application],[awa],
  [git@github.com:stcarrez/ada-awa.git],
  [Building adafr requires the Ada Web Application Library.],
  [
    AWA_DIR=
  ])
AC_SUBST(AWA_DIR)

AC_OUTPUT(
Makefile adafr.gpr tests.gpr rules.mk
)