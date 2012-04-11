#################################################################################
#                Odot                                                           #
#                                                                               #
#    Copyright (C) 2005 Institut National de Recherche en Informatique et       #
#    en Automatique. All rights reserved.                                       #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as published          #
#    by the Free Software Foundation; either version 2.1 of the License, or     #
#    any later version.                                                         #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU Lesser General Public License for more details.                        #
#                                                                               #
#    You should have received a copy of the GNU General Public License          #
#    along with this program; if not, write to the Free Software                #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#################################################################################

include master.Makefile

PACKAGES=lablgtk2.gnomecanvas,unix
OF_FLAGS=-package $(PACKAGES)

COMPFLAGS=
LINKFLAGS=

LIB=odot.cmxa
LIB_A=$(LIB:.cmxa=.a)
LIB_CMI=$(LIB:.cmxa=.cmi)
LIB_BYTE=$(LIB:.cmxa=.cma)

# Compilation
#############

CMOFILES=\
	odot_version.cmo \
	odot_misc.cmo \
	odot_parser.cmo \
	odot_lexer.cmo \
	odot.cmo

CMXFILES= $(CMOFILES:.cmo=.cmx)
CMIFILES= $(CMOFILES:.cmo=.cmi)

GTK_CMOFILES=odot_view.cmo
GTK_CMXFILES= $(GTK_CMOFILES:.cmo=.cmx)
GTK_CMIFILES= $(GTK_CMOFILES:.cmo=.cmi)

LIB_GTK_O=$(LIB_GTK:.cmx=.o)
LIB_GTK_CMI=$(LIB_GTK:.cmx=.cmi)

all: byte opt
byte: $(LIB_BYTE) gtk_byte
opt: $(LIB) gtk

test: $(LIB_BYTE) test.ml
	$(OCAMLC) $(INCLUDES) -o test.x unix.cma $(LIB_BYTE) test.ml

gtk: $(LIB_GTK)
gtk_byte: $(LIB_GTK_BYTE)

$(LIB_GTK): $(GTK_CMIFILES) $(GTK_CMXFILES)
$(LIB_GTK_BYTE): $(GTK_CMIFILES) $(GTK_CMOFILES)

test_gtk: $(LIB_BYTE) gtk_byte test_gtk.ml
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -linkpkg -o test_gtk.x \
	$(LIB_BYTE) $(LIB_GTK_BYTE) test_gtk.ml

$(LIB): $(CMIFILES) $(CMXFILES)
	$(OCAMLOPT) -a -o $@ $(CMXFILES)

$(LIB_BYTE): $(CMIFILES) $(CMOFILES)
	$(OCAMLC) -a -o $@ $(CMOFILES)

TESTFILES=test.dot test2.dot test3.dot test4.dot test5.dot test6.dot
check: test $(TESTFILES)
	for i in $(TESTFILES) ; do \
	echo testing with file $$i ; \
	./test.x $$i > $$i.test ; \
	dot -Tps -o test.ps $$i ; \
	dot -Tps -o test2.ps $$i.test ; \
	(diff test.ps test2.ps > /dev/null && echo ok) || echo test failed on file $$i ; \
	rm -f test.ps test2.ps $$i.test ; done

# Documentation :
#################
dump.odoc: *.ml *.mli
	$(OCAMLDOC) $(OCAMLPP) $(COMPFLAGS) -keep-code -dump dump.odoc `ls $^ | grep -v test`

doc:
	$(MKDIR) ocamldoc
	$(OCAMLDOC) $(OCAMLPP) $(COMPFLAGS) \
	-t "The Odot library" \
	-html \
	-d ocamldoc \
	odot.mli \
	`if test "$(LIB_GTK)" != "" ; then echo odot_gtk.mli; fi`

# myself

master.Makefile: master.Makefile.in \
	odot_version.ml.in config.status
	./config.status

config.status: configure
	./config.status --recheck

configure: configure.in
	autoconf

# backup, clean and depend :
############################

distclean: clean
	$(RM) config.cache config.log config.status master.Makefile
	$(RM) autom4te.cache
	$(RM) odot_version.ml META

clean::
	$(RM) *~ \#*\#
	$(RM) *.cmo *.cmi *.cmx *.a *.o *.cma *.cmxa *.odoc *.annot
	$(RM) odot_parser.ml{i,} odot_lexer.ml odot_parser.output
	$(RM) $(EXE) $(EXE_BYTE) test.x test_gtk.x

.depend depend: odot_parser.ml odot_lexer.ml
	$(RM) .depend
	$(OCAMLDEP) *.ml *.mli > .depend

dummy:

include .depend

#################
# Installation
#################
install: dummy
	$(CP) $(LIB_BYTE) $(LIB_CMI) $(OCAMLLIB)
	if test -f $(LIB); then \
	$(CP) $(LIB) $(LIB_A) $(OCAMLLIB); fi
	if test -f $(LIB_GTK); then \
	$(CP) $(LIB_GTK) $(LIB_GTK_CMI) $(LIB_GTK_O) $(OCAMLLIB); fi
	if test -f $(LIB_GTK_BYTE); then \
	$(CP) $(LIB_GTK_BYTE) $(LIB_GTK_CMI) $(OCAMLLIB); fi


###########################
# additional dependencies
###########################
