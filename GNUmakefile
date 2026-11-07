# Copyright © 2026 Barry Schwartz
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions: The above copyright notice and
# this permission notice shall be included in all copies or
# substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

.DELETE_ON_ERROR:
.DEFAULT_GOAL := default

PACKAGE = gafoundry
VERSION = 1.0.0
COPYRIGHT = "Copyright © 2026 Barry Schwartz\n"

CALM1 = calm1
CALM1_R7RS = $(CALM1) --comment-style=r7rs
CALM1_FLAGS = \
	-D'PACKAGE=$(PACKAGE)' \
	-D'VERSION=$(VERSION)' \
	-D'COPYRIGHT=$(COPYRIGHT)'

CHICKEN_INSTALL = chicken-install
CHICKEN_UNINSTALL = chicken-uninstall

.PHONY: default all clean maintainer-clean
default: all

%.sld: %.sld.calm1
	$(CALM1_R7RS) $(CALM1_FLAGS) $(<) -o $(@)

all:: gafoundry.egg
gafoundry.egg: gafoundry.egg.calm1
	$(CALM1) $(CALM1_FLAGS) $(<) -o $(@)

all:: g25spec2gaspec
g25spec2gaspec: \
		gafoundry.egg \
		g25spec2gaspec.scm \
		gafoundry-lib/g25spec2gaspec.sld \
		gafoundry-lib/version.sld \
		gafoundry-lib/xml-parse.sld \
		gafoundry-lib/format.sld \
		gafoundry-lib/define-record-factory.sld
	$(CHICKEN_INSTALL) -n

gafoundry-lib/version.sld: GNUmakefile
all:: gafoundry-lib/version.sld
maintainer-clean:: ; -rm -f gafoundry-lib/version.sld

gafoundry-lib/format.sld: gafoundry-lib/format.scm
all:: gafoundry-lib/format.sld
maintainer-clean:: ; -rm -f gafoundry-lib/format.sld

gafoundry-lib/xml-parse.sld: gafoundry-lib/replacements-for-slib.scm
gafoundry-lib/xml-parse.sld: gafoundry-lib/alist.scm
gafoundry-lib/xml-parse.sld: gafoundry-lib/strsrch.scm
gafoundry-lib/xml-parse.sld: gafoundry-lib/xml-parse.scm
all:: gafoundry-lib/xml-parse.sld
maintainer-clean:: ; -rm -f gafoundry-lib/xml-parse.sld

gafoundry-lib/prec.sld: gafoundry-lib/replacements-for-slib.scm
gafoundry-lib/prec.sld: gafoundry-lib/alist.scm
gafoundry-lib/prec.sld: gafoundry-lib/strsrch.scm
gafoundry-lib/prec.sld: gafoundry-lib/prec.scm
all:: gafoundry-lib/prec.sld
maintainer-clean:: ; -rm -f gafoundry-lib/prec.sld

.PHONY: install
install:: g25spec2gaspec
	$(CHICKEN_INSTALL) -s

.PHONY: uninstall
uninstall::
	$(CHICKEN_UNINSTALL) -s -force gafoundry

.PHONY: test install-check
test install-check::
	$(CHICKEN_INSTALL) -s -test

clean::
	-rm -f gafoundry.build.sh
	-rm -f gafoundry.install.sh
	-rm -f gafoundry-lib.*.{c,o,so,link,import.scm}
