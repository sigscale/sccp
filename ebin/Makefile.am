## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = sccp.app
EXTRA_DIST = sccp.app

beam_DATA = sccp_codec.beam

%.beam:$(top_srcdir)/src/%.erl	$(top_srcdir)/include/sccp.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' \
			-e 's#%APP_PRIV%#$(ERLANG_INSTALL_LIB_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/priv#' < $< > $@ 

CLEANFILES = $(beam_DATA) $(app_DATA)

