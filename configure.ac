dnl Autoconf configure script for adafr

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/adafr.ads)

AM_GNAT_LIBRARY_PROJECT(adafr)

# Current release settings
ADAFR_MAJOR_VERSION=1
ADAFR_MINOR_VERSION=6
ADAFR_MICRO_VERSION=0
ADAFR_VERSION=$ADAFR_MAJOR_VERSION.$ADAFR_MINOR_VERSION.$ADAFR_MICRO_VERSION

# Set the version number of the project
AC_SUBST(ADAFR_VERSION)
AC_SUBST(ADAFR_MAJOR_VERSION)
AC_SUBST(ADAFR_MINOR_VERSION)
AC_SUBST(ADAFR_MICRO_VERSION)

ADAFR_LIBDIR="lib"
AC_SUBST(ADAFR_LIBDIR)

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[utilada_sys],
  [git@github.com:stcarrez/ada-util.git],
  [Building adafr requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_utilada_sys}
  ])
AC_SUBST(UTIL_DIR)

##########################################
# Ada Expression Language library
##########################################
AM_GNAT_FIND_PROJECT([ada-el],[Ada Expression Language Library],[elada],
  [git@github.com:stcarrez/ada-el.git],
  [Building adafr requires the Ada EL Library.],
  [
    EL_DIR=${gnat_project_dir_elada}
  ])
AC_SUBST(EL_DIR)

##########################################
# Ada Servlet library
##########################################
AM_GNAT_FIND_PROJECT([ada-servlet],[Ada Servlet],[servletada],
  [git@github.com:stcarrez/ada-servlet.git],
  [Building adafr requires the Ada Servlet Library.],
  [
    SERVLET_DIR=${gnat_project_dir_servletada}
  ])
AC_SUBST(SERVLET_DIR)

##########################################
# Ada Server Faces library
##########################################
AM_GNAT_FIND_PROJECT([ada-asf],[Ada Server Faces],[asf],
  [git@github.com:stcarrez/ada-asf.git],
  [Building adafr requires the Ada Server Faces Library.],
  [
    ASF_DIR=${gnat_project_dir_asf}
  ])
AC_SUBST(ASF_DIR)

##########################################
# Ada Security library
##########################################
AM_GNAT_FIND_PROJECT([ada-security],[Ada Security Library],[security],
  [git@github.com:stcarrez/ada-security.git],
  [Building adafr requires the Ada Security Library.],
  [
    SECURITY_DIR=${gnat_project_dir_security}
  ])
AC_SUBST(SECURITY_DIR)

##########################################
# Ada Database Objects library
##########################################
AM_GNAT_FIND_PROJECT([ada-ado],[Ada Database Objects],[ado],
  [git@github.com:stcarrez/ada-ado.git],
  [Building adafr requires the Ada Database Objects Library.],
  [
    ADO_DIR=${gnat_project_dir_ado}
  ])
AC_SUBST(ADO_DIR)

##########################################
# Ada Wiki library
##########################################
AM_GNAT_FIND_PROJECT([ada-wiki],[Ada Wiki],[wikiada],
  [git@github.com:stcarrez/ada-wiki.git],
  [Building adafr requires the Ada Wiki Library.],
  [
    WIKI_DIR=${gnat_project_dir_wikiada}
  ])
AC_SUBST(WIKI_DIR)

##########################################
# Ada Web Application library
##########################################
AM_GNAT_FIND_PROJECT([ada-awa],[Ada Web Application],[awa],
  [git@github.com:stcarrez/ada-awa.git],
  [Building adafr requires the Ada Web Application Library.],
  [
    AWA_DIR=${gnat_project_dir_awa}
  ])
AC_SUBST(AWA_DIR)

AC_OUTPUT(
Makefile.conf adafr.gpr
)
