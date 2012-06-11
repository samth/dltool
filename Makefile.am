moddir=$(prefix)/share/guile/site/2.0
godir = $(libdir)/guile/2.0/ccache

SOURCES = \
	dlhacks/elf.scm \
	dlhacks/dwarf.scm \
	dlhacks.scm
GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

CLEANFILES = $(GOBJECTS)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

TESTS = 					\
	tests/basic.test
TESTS_ENVIRONMENT = $(abs_top_builddir)/env $(GUILE) --no-auto-compile

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES) $(TESTS)
