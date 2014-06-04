LIBFILES= ui.pm \
	PsN.pm \
	common_options.pm \
	linear_algebra.pm \
	npde_util.pm \
	psn.conf_template \
	psn.conf \
	OSspecific.pm \
	binning.pm \
	array.pm \
	include_modules.pm \
	ext/Math/SigFigs.pm \
	ext/Math/MatrixReal.pm \
	ext/Statistics/Distributions.pm \
	ext/Config/Tiny.pm \
	ext/File/HomeDir.pm \
	ext/Carp.pm \
	matlab/bca.m \
	matlab/histograms.m \
	matlab/profiles.m \
	R-scripts/llp.R \
	R-scripts/cdd.R \
	R-scripts/bootstrap.R \
	status_bar.pm \
	nonmemrun.pm \
	nonmemrun/localunix.pm \
	nonmemrun/localwindows.pm \
	nonmemrun/slurm.pm \
	nonmemrun/sge.pm \
	nonmemrun/zink.pm \
	nonmemrun/ud.pm \
	nonmemrun/mosix.pm \
	nonmemrun/torque.pm \
	nonmemrun/lsf.pm \
	file.pm \
	data/individual.pm \
	data.pm \
	model/problem/record/option.pm \
	model/problem/record/init_option.pm \
	model/problem/record/theta_option.pm \
	model/problem/record.pm \
  model/problem/code_record.pm \
	model/problem/init_record.pm \
	model/problem/abbreviated.pm \
	model/problem/aes.pm \
	model/problem/aesinitial.pm \
	model/problem/contr.pm \
	model/problem/covariance.pm \
	model/problem/data.pm \
	model/problem/des.pm \
	model/problem/error.pm \
	model/problem/estimation.pm \
	model/problem/infn.pm \
	model/problem/input.pm \
	model/problem/msfi.pm \
	model/problem/mix.pm \
	model/problem/model.pm \
	model/problem/nonparametric.pm \
	model/problem/omega.pm \
	model/problem/omegap.pm \
	model/problem/omegapd.pm \
	model/problem/pk.pm \
	model/problem/prior.pm \
	model/problem/problem.pm \
	model/problem/pred.pm \
	model/problem/scatter.pm \
	model/problem/anneal.pm \
	model/problem/phis.pm \
	model/problem/level.pm \
	model/problem/etas.pm \
	model/problem/bind.pm \
	model/problem/sigma.pm \
	model/problem/sigmap.pm \
	model/problem/sigmapd.pm \
	model/problem/simulation.pm \
	model/problem/sizes.pm \
	model/problem/subroutine.pm \
	model/problem/table.pm \
	model/problem/theta.pm \
	model/problem/thetai.pm \
	model/problem/thetar.pm \
	model/problem/thetap.pm \
	model/problem/thetapv.pm \
	model/problem/tol.pm \
	model/cwres_module.pm \
	model/mirror_plot_module.pm \
	model/iofv_module.pm \
	model/shrinkage_module.pm \
	model/nonparametric_module.pm \
	model/problem.pm \
	output/problem/subproblem.pm \
	output/problem.pm \
	output.pm \
	model.pm \
	tool.pm \
	tool/modelfit.pm \
	tool/sir.pm \
	tool/llp.pm \
	tool/cdd.pm \
	tool/cdd/jackknife.pm \
	tool/sse.pm \
	tool/gls.pm \
	tool/frem.pm \
	tool/ebe_npde.pm \
	tool/xv_step.pm \
	tool/xv.pm \
	tool/lasso.pm \
	tool/npc.pm \
	tool/mcmp.pm \
	tool/pind.pm \
	tool/bootstrap.pm \
	tool/randtest.pm \
	tool/nonpb.pm \
	tool/scm/config_file.pm \
	tool/scm.pm \
	tool/pvar.pm \

BINFILES=$(wildcard bin/*)

RELFILES=$(addprefix PsN-Source/lib/,$(LIBFILES)) \
	$(addprefix PsN-Source/, \
	$(BINFILES) \
	setup.pl \
	AUTHORS \
	COPYING \
	README.txt )

TEXFILES=$(wildcard doc/*.tex)
PDFFILES=$(TEXFILES:.tex=.pdf)

.PHONY : clean

clean:
	@-rm -rf $(DOCUMENTS) PsN-Source psn_test_package.zip development/completion_files doc/*.aux doc/*.log doc/*.pdf doc/inputs/*eps-converted-to.pdf PsN-Source.tar.gz PsN-Source.zip

doc/%.pdf: doc/%.tex
	cd doc; pdflatex $*.tex >/dev/null; pdflatex $*.tex >/dev/null

doc: $(PDFFILES)

release: completion rel_dir $(RELFILES) $(PDFFILES)
	@ mkdir PsN-Source/development
	@ mkdir PsN-Source/development/completion_files
	@ cp development/completion_files/* PsN-Source/development/completion_files
	@ mkdir PsN-Source/test
	@ cp -r test/unit PsN-Source/test
	@ cp -r test/system PsN-Source/test
	@ rm PsN-Source/test/system/frem.t
	@ cp -r test/test_files PsN-Source/test
	@ cp test/includes.pm PsN-Source/test
	@ mkdir PsN-Source/doc
	@ cp doc/*.pdf PsN-Source/doc
	@ cp doc/*.scm PsN-Source/doc
	@ cp doc/*.xls PsN-Source/doc
	@ cd PsN-Source/doc/; zip PsN_pdf_documentation *.pdf *.xls *.scm
	@ cd PsN-Source/doc/; tar -czf PsN_pdf_documentation.tar.gz *.pdf *.xls *.scm
	@ chmod -R a+r PsN-Source/test/test_files
	@ zip -r PsN-Source PsN-Source/
	@ tar czf PsN-Source.tar.gz PsN-Source/

documentation: doc/*.pdf $(PDFFILES)
	@ cd PsN-Source/doc/; zip PsN_pdf_documentation *.pdf *.xls *.scm
	@ cd PsN-Source/doc/; tar -czf PsN_pdf_documentation.tar.gz *.pdf *.xls *.scm 

testpackage:
	@ zip -r psn_test_package test

COMPFILES=boot_scm \
	bootstrap \
	cdd \
	crossval \
	ebe_npde \
	execute \
	extended_grid \
	frem \
	gls \
	lasso \
	linearize \
	llp \
	sir \
	mcmp \
	mimp \
	nca \
	nonpb \
	npc \
	parallel_retries \
	pind \
	psn \
	psn_options \
	pvar \
	randtest \
	rawresults \
	runrecord \
	scm \
	se_of_eta \
	sse \
	sumo \
	update_inits \
	vpc \
	xv_scm \

development/completion_files:
	@ mkdir -p development/completion_files

.PHONY: completion
completion: development/completion_files
	cd bin; \
	$(foreach file, $(COMPFILES), perl ../development/genauto $(file) >../development/completion_files/$(file);)

PsN-Source/setup.pl: bin/setup.pl
	@ cp bin/setup.pl $@

PsN-Source/lib/matlab/% : matlab/%
	@ cp matlab/$* $@

PsN-Source/lib/R-scripts/% : R-scripts/%
	@ cp R-scripts/$* $@

PsN-Source/%: %
	@ cp -ar $* $@

rel_dir:
	@mkdir -p $(sort $(dir $(RELFILES)))
