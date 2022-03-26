NAME=adafr
GPRPATH=${NAME}.gpr

-include Makefile.conf

include Makefile.defaults

ROOT_DIR=$(shell pwd)
PLUGINS=

GPRFLAGS += -j$(PROCESSORS) -XROOT_DIR=$(ROOT_DIR) -XUTIL_OS=$(UTIL_OS) -XUTIL_AWS_IMPL=$(UTIL_AWS_VERSION)

ifeq (${HAVE_DYNAMO},yes)
GPRFLAGS += -XBUILD_DYNAMO=no
else
GPRFLAGS += -XBUILD_DYNAMO=yes
DYNAMO=$(ROOT_DIR)/bin/dynamo
endif

LIBNAME=lib${NAME}

# Model generation arguments with Dynamo
# --package XXX.XXX.Models db uml/xxx.zargo
DYNAMO_ARGS=--package Adafr.Members.Models db uml/ada-france.zargo

ROOTDIR=.

$(foreach PLUGIN,$(PLUGINS),$(eval include plugins/$(PLUGIN)/Makefile))

build:: setup
	$(GNATMAKE) $(GPRFLAGS) -p -P "$(GPRPATH)" $(MAKE_ARGS)

generate:: build-dynamo
	mkdir -p db
	$(DYNAMO) generate $(DYNAMO_ARGS)

package:
	rm -rf $(DIST_DIR)
	$(DYNAMO) dist $(DIST_DIR) package.xml
	tar czf $(DIST_FILE) $(DIST_DIR)

ifneq (${HAVE_DYNAMO},yes)
setup:: awa/dynamo/src/gen-configs.ads
awa/dynamo/src/gen-configs.ads:   Makefile.conf awa/dynamo/src/gen-configs.gpb
	gnatprep -DCONFIG_DIR='"$(ROOT_DIR)/awa/dynamo/config"' -DVERSION='"1.2.3"' \
		  awa/dynamo/src/gen-configs.gpb awa/dynamo/src/gen-configs.ads

setup:: awa/ada-ado/src/drivers/ado-drivers-initialize.adb
awa/ada-ado/src/drivers/ado-drivers-initialize.adb: awa/ada-ado/src/drivers/ado-drivers-initialize.gpb Makefile.conf
	gnatprep -DHAVE_MYSQL=False \
	          -DHAVE_SQLITE=True \
	          -DHAVE_POSTGRESQL=False \
		  awa/ada-ado/src/drivers/ado-drivers-initialize.gpb $@

# Install the AWA UML model in Dynamo UML search path
awa/dynamo/config/uml/AWA.xmi: awa/awa/uml/awa.zargo
	unzip -cq awa/awa/uml/awa.zargo awa.xmi > awa/dynamo/config/uml/AWA.xmi

build-dynamo: bin/dynamo awa/dynamo/config/uml/AWA.xmi

bin/dynamo: setup
	$(GNATMAKE) $(GPRFLAGS) -p -P "$(GPRPATH)" $(MAKE_ARGS)

else

setup::

build-dynamo:

endif
