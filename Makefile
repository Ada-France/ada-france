NAME=adafr
GPRPATH=${NAME}.gpr
VERSION=1.22.0

DIST_DIR=adafr-$(VERSION)
DIST_FILE=adafr-$(VERSION).tar.gz

-include Makefile.conf

include Makefile.defaults

ROOT_DIR=$(shell pwd)

CLEAN_FILES= \
  awa/obj awa/ada-util/obj awa/ada-el/obj awa/ada-ado/obj \
  awa/ada-security/obj awa/ada-keystore/obj awa/ada-servlet/obj \
  awa/openapi-ada/obj awa/ada-lzma/obj awa/ada-security/obj \
  awa/ada-asf/obj awa/ada-wiki/obj awa/dynamo/obj awa/awa/obj \
  awa/plugins/awa-*/obj

DYNAMO=alr exec -- dynamo
BUILD_COMMAND=alr build -- -XSOCKET=openssl
HAVE_SETUP=yes

# Model generation arguments with Dynamo
# --package XXX.XXX.Models db uml/xxx.zargo
DYNAMO_ARGS=--package Adafr.Members.Models db uml/ada-france.zargo

ROOTDIR=.

build:: setup
	$(BUILD_COMMAND)

generate:: build-dynamo
	mkdir -p db
	$(DYNAMO) generate $(DYNAMO_ARGS)

package:
	rm -rf $(DIST_DIR)
	$(DYNAMO) dist $(DIST_DIR) package.xml
	tar czf $(DIST_FILE) $(DIST_DIR)

setup::

build-dynamo: bin/dynamo awa/dynamo/config/uml/AWA.xmi

bin/dynamo:
	$(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

# Install the AWA UML model in Dynamo UML search path
awa/dynamo/config/uml/AWA.xmi: awa/uml/awa.zargo
	unzip -cq awa/uml/awa.zargo awa.xmi > awa/dynamo/config/uml/AWA.xmi

# Give information about GPR path and Dynamo.xml files identified for the project
# => this indicates the search paths used to search for files (Ada, but also HTML, CSS, JS, SQL)
info:
	$(DYNAMO) info
