NAME=adafr
GPRPATH=${NAME}.gpr

-include Makefile.conf

include Makefile.defaults

ROOT_DIR=$(shell pwd)

ifeq (${HAVE_ALIRE},yes)
DYNAMO=alr exec -- dynamo
BUILD_COMMAND=alr build
HAVE_SETUP=yes
else
GPRFLAGS += -j$(PROCESSORS) -XROOT_DIR=$(ROOT_DIR) -XUTIL_OS=$(UTIL_OS) -XUTIL_AWS_IMPL=$(UTIL_AWS_VERSION)

ifeq (${HAVE_DYNAMO},yes)
HAVE_SETUP=no
GPRFLAGS += -XBUILD_DYNAMO=no
else
HAVE_SETUP=yes
GPRFLAGS += -XBUILD_DYNAMO=yes
DYNAMO=env DYNAMO_UML_PATH=awa/awa/uml DYNAMO_SEARCH_PATH=awa/awa/plugins $(ROOT_DIR)/bin/dynamo
BUILD_COMMAND=$(GNATMAKE) $(GPRFLAGS) -p -P "$(GPRPATH)" $(MAKE_ARGS)
endif

endif

LIBNAME=lib${NAME}

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

ifeq (${HAVE_SETUP},yes)
setup:: awa/dynamo/src/gen-configs.ads
awa/dynamo/src/gen-configs.ads:   Makefile.conf awa/dynamo/src/gen-configs.gpb
	cd awa/dynamo && sh ./alire-setup.sh

setup:: awa/ada-ado/src/drivers/ado-drivers-initialize.adb
awa/ada-ado/src/drivers/ado-drivers-initialize.adb: awa/ada-ado/src/drivers/ado-drivers-initialize.gpb Makefile.conf
	gnatprep -DHAVE_MYSQL=True \
	          -DHAVE_SQLITE=False \
	          -DHAVE_POSTGRESQL=False \
		  awa/ada-ado/src/drivers/ado-drivers-initialize.gpb $@

# Install the AWA UML model in Dynamo UML search path
awa/dynamo/config/uml/AWA.xmi: awa/awa/uml/awa.zargo
	unzip -cq awa/awa/uml/awa.zargo awa.xmi > awa/dynamo/config/uml/AWA.xmi

build-dynamo: bin/dynamo awa/dynamo/config/uml/AWA.xmi

bin/dynamo: setup
	$(BUILD_COMMAND)

else

setup::

build-dynamo:

endif
