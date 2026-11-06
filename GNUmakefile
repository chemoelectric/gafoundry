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

CALM1 = calm1
CALM1_R7RS = $(CALM1) --comment-style=r7rs
CALM1_FLAGS =

.PHONY: default all clean maintainer-clean
default: all

%.sld: %.sld.calm1
	$(CALM1_R7RS) $(CALM1_FLAGS) $(<) -o $(@)

gafoundry/xml-parse.sld: gafoundry/replacements-for-slib.scm
gafoundry/xml-parse.sld: gafoundry/alist.scm
gafoundry/xml-parse.sld: gafoundry/strsrch.scm
gafoundry/xml-parse.sld: gafoundry/xml-parse.scm
all:: gafoundry/xml-parse.sld
maintainer-clean:: ; -rm -f gafoundry/xml-parse.sld

gafoundry/prec.sld: gafoundry/replacements-for-slib.scm
gafoundry/prec.sld: gafoundry/alist.scm
gafoundry/prec.sld: gafoundry/strsrch.scm
gafoundry/prec.sld: gafoundry/prec.scm
all:: gafoundry/prec.sld
maintainer-clean:: ; -rm -f gafoundry/prec.sld
