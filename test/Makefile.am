## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

testmods = sccp_codec_SUITE

check_DATA = .dialyzer_ok .common_test_ok

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer --src $(top_srcdir)/src

%.beam:$(top_srcdir)/test/%.erl $(top_srcdir)/include/sccp.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

test.spec:	$(top_srcdir)/test/test.spec
	cp $< $@

ct.cfg:	$(top_srcdir)/test/ct.cfg
	cp $< $@

.common_test_ok:	$(addsuffix .beam, $(testmods)) sys.config ct.cfg test.spec
	@if [ ! -d log ]; then mkdir log; fi
	ct_run -no_auto_compile -pa $(top_builddir)/ebin -logdir log -config ct.cfg -spec test.spec -dir . -erl_args -noshell -config sys -sname $(LOGNAME)-ct

sys.config:	$(top_srcdir)/test/sys.config-in
	sed -e "s#%VAR1%#VAL1#" < $< > $@

clean-local:
	-rm -r *.beam log/*

