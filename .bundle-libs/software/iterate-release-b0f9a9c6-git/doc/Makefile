# -*- Mode: Makefile; tab-width: 4; indent-tabs-mode: t -*-
#
# Makefile --- Make targets for generating the documentation.
#
# Copyright (C) 2005-2006, Luis Oliveira  <loliveira at common-lisp.net>
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use, copy,
# modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

all: docs

docs:
	sh gendocs.sh -o manual --html "--css-include=style.css" iterate "The Iterate Manual"

clean:
	find . \( -name "*.info" -o -name "*.aux" -o -name "*.cp" -o -name "*.fn" -o -name "*.fns" -o -name "*.ky" -o -name "*.log" -o -name "*.pg" -o -name "*.toc" -o -name "*.tp" -o -name "*.vr" -o -name "*.dvi" -o -name "*.cps" -o -name "*.vrs" \) -exec rm {} \;
	rm -rf manual

upload-docs:
	rsync -av --delete -e ssh manual common-lisp.net:/project/iterate/public_html/
#	scp -r manual spec common-lisp.net:/project/cells/public_html/

# vim: ft=make ts=4 noet