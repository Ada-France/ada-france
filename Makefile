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

build::
	$(GNATMAKE) $(GPRFLAGS) -p -P "$(GPRPATH)" $(MAKE_ARGS)

generate::
	mkdir -p db
	$(DYNAMO) generate $(DYNAMO_ARGS)

package:
	rm -rf $(DIST_DIR)
	$(DYNAMO) dist $(DIST_DIR) package.xml
	tar czf $(DIST_FILE) $(DIST_DIR)
