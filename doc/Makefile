### This file is part of Adaptive Plot, a library for plotting
### functions from the MIT Scheme REPL
### Copyright (C) 2013 Alexey Radul
###
### Adaptive Plot is free software: you can redistribute it and/or
### modify it under the terms of the GNU Affero General Public License
### as published by the Free Software Foundation, either version 3 of
### the License, or (at your option) any later version.
###
### Adaptive Plot is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### Affero General Public License for more details.
###
### You should have received a copy of the GNU Affero General Public
### License along with Adaptive Plot.  If not, see
### <http://www.gnu.org/licenses/>.

SCMS = $(wildcard *.scm)
PNGS = $(subst .scm,.png,$(SCMS))

all: $(PNGS)

%.png: %.scm
	mit-scheme --compiler -heap 6000 --batch-mode --no-init-file \
	--eval '(set! load/suppress-loading-message? #t)' \
	--eval '(load "$<")'
