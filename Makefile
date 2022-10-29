NAME=adafr
GPRPATH=${NAME}.gpr
BUILD_COMMAND=alr build
DYNAMO=alr exec -- dynamo

-include Makefile.conf

include Makefile.defaults

ROOT_DIR=$(shell pwd)
PLUGINS=

LIBNAME=lib${NAME}

# Model generation arguments with Dynamo
# --package XXX.XXX.Models db uml/xxx.zargo
DYNAMO_ARGS=--package Adafr.Members.Models db uml/ada-france.zargo

ROOTDIR=.

build::
	$(BUILD_COMMAND)

generate::
	mkdir -p db
	$(DYNAMO) generate $(DYNAMO_ARGS)

package:
	rm -rf $(DIST_DIR)
	$(DYNAMO) dist $(DIST_DIR) package.xml
	tar czf $(DIST_FILE) $(DIST_DIR)

