NAME=adafr
GPRPATH=${NAME}.gpr

-include Makefile.conf

include Makefile.defaults

PLUGINS=

LIBNAME=lib${NAME}

# Model generation arguments with Dynamo
# --package XXX.XXX.Models db uml/xxx.zargo
DYNAMO_ARGS=--package Adafr.Members.Models db uml/ada-france.zargo

ROOTDIR=.

$(foreach PLUGIN,$(PLUGINS),$(eval include plugins/$(PLUGIN)/Makefile))

build::
	$(GNATMAKE) -m -p -P "$(GPRPATH)" $(MAKE_ARGS)

generate::
	mkdir -p db
	$(DYNAMO) generate $(DYNAMO_ARGS)

package:
	rm -rf $(DIST_DIR)
	$(DYNAMO) dist $(DIST_DIR) package.xml
	tar czf $(DIST_FILE) $(DIST_DIR)
