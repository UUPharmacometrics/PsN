BIN=bootstrap cdd execute llp scm

HTML_STUBS=_synopsis.php _description.php _options.php _examples.php

DOCUMENTS=$(foreach pre,$(BIN),$(foreach suff,$(HTML_STUBS),$(addprefix html/$(pre),$(suff))))

DIA2CODE=dia2code
FILLSCRIPT=perl ./bin/fill_diacode.pl
DIRPM=libgen/
DIRSUBS=lib/
PERLDIRS=-I/home/pontus/perl
LIBFILES=debug.pm \
	ui.pm \
	status_bar.pm \
	nonmem.pm \
	file.pm \
	data/individual.pm \
	data.pm \
	table_file.pm \
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
	model/problem/simulation.pm \
	model/problem/sizes.pm \
	model/problem/subroutine.pm \
	model/problem/table.pm \
	model/problem/theta.pm \
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

RELFILES=$(addprefix PsN-Source/lib/,$(LIBFILES)) \
	$(addprefix PsN-Source/, \
	lib/PsN_template.pm \
	lib/common_options.pm \
	lib/linear_algebra.pm \
	lib/psn.conf_template \
	lib/psn.conf \
	lib/OSspecific.pm \
	lib/hotkey.pm \
	lib/binning.pm \
	lib/array.pm \
	lib/ext/Math/SigFigs.pm \
	lib/ext/Math/MatrixReal.pm \
	lib/ext/Statistics/Distributions.pm \
	lib/ext/Parallel/ForkManager.pm \
	lib/ext/Config/Tiny.pm \
	lib/ext/File/HomeDir.pm \
	lib/matlab/bca.m \
	lib/matlab/histograms.m \
	lib/matlab/profiles.m \
	lib/R-scripts/llp.R \
	lib/R-scripts/cdd.R \
	lib/R-scripts/bootstrap.R \
	bin/psn \
	bin/psn_options \
	bin/psn_clean \
	bin/execute \
	bin/cdd \
	bin/sse \
	bin/gls \
	bin/frem \
	bin/ebe_npde \
	bin/pind \
	bin/nonpb \
	bin/npc \
	bin/mcmp \
	bin/vpc \
	bin/llp \
	bin/scm \
	bin/bootstrap \
	bin/randtest \
	bin/sumo \
	bin/lasso \
	bin/runrecord \
	bin/extended_grid \
	bin/data_stats \
	bin/se_of_eta \
	bin/update_inits \
	bin/mimp \
	bin/parallel_retries \
	bin/xv_scm \
	bin/boot_scm \
	bin/linearize \
	lib/doc/lasso_userguide.pdf \
	lib/doc/sse_userguide.pdf \
	lib/doc/gls_userguide.pdf \
	lib/doc/frem_userguide.pdf \
	lib/doc/ebe_npde_userguide.pdf \
	lib/doc/npc_vpc_userguide.pdf \
	lib/doc/draft_vpc_automatic_binning.pdf \
	lib/doc/extended_grid_userguide.pdf \
	lib/doc/mcmp_userguide.pdf \
	lib/doc/bootstrap_userguide.pdf \
	lib/doc/scm_userguide.pdf \
	lib/doc/boot_scm_userguide.pdf \
	lib/doc/common_options_defaults_versions_psn.pdf \
	lib/doc/known_bugs_and_workarounds.pdf \
	lib/doc/psn_configuration.pdf \
	lib/doc/psn_installation.pdf \
	lib/doc/PsN_and_NONMEM7.pdf \
	lib/doc/runrecord_userguide.pdf \
	lib/doc/runrecord_template.xls \
	lib/doc/config_all_default_parameterizations_explicitly.scm \
	lib/doc/config_centering_bivariate_covariate.scm \
	lib/doc/config_different_parameterizations_different_covariates.scm \
	lib/doc/config_emax_and_other_parameterizations.scm \
	lib/doc/config_grouping_categorical_covariate.scm \
	lib/doc/config_template_backward.scm \
	lib/doc/config_template_linearize.scm \
	lib/doc/config_template_standard.scm \
	setup.pl \
	README.txt )

DIALIBFILES=debug.pm \
	ui.pm \
	status_bar.pm \
	nonmem.pm \
	file.pm \
	data/individual.pm \
	data.pm \
	table_file.pm \
	model/mirror_plot_module.pm \
	model/problem.pm \
	output/problem/subproblem.pm \
	output/problem.pm \
	output.pm \
	model.pm \
	tool.pm \
	tool/modelfit.pm \
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

PERLFILES=$(addprefix lib/,$(DIALIBFILES))

TEXFILES=$(wildcard doc/*.tex)
PDFFILES=$(TEXFILES:.tex=.pdf)


all: libgen $(PERLFILES)

GENFILES=$(addprefix libgen/,$(DIALIBFILES))
.PRECIOUS: $(GENFILES)

libgen:
	@ mkdir -p libgen

lib/%.pm: libgen/%.pm lib/%_subs.pm
	 @ cp libgen/$*.pm libgen/$*_temp.pm
	 @ $(FILLSCRIPT) libgen/$*_temp.pm lib/$*_subs.pm
	 @ cp libgen/$*_temp.pm $@
	 @ perl $(PERLDIRS) -I./$(@D) -I./lib -c $@ -W -t -T|| (rm $@ && false)

.PHONY : clean

clean:
	@-rm -rf $(PERLFILES) $(DOCUMENTS) libgen PsN-Source bin/completion_files doc/*.aux doc/*.log doc/*.pdf PsN-Source.tar.gz PsN-Source.zip

libgen/data%.pm : diagrams/data.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/data.dia

libgen/debug.pm : diagrams/debug.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/debug.dia

libgen/ui.pm : diagrams/ui.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/ui.dia

libgen/status_bar.pm : diagrams/status_bar.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/status_bar.dia

libgen/nonmem.pm : diagrams/nonmem.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/nonmem.dia	

libgen/file.pm : diagrams/file.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/file.dia

libgen/tool/modelfit.pm : diagrams/modelfit.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/modelfit.dia

libgen/tool/llp.pm : diagrams/llp.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/llp.dia

libgen/tool/cdd/jackknife.pm libgen/tool/cdd.pm : diagrams/cdd.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/cdd.dia

libgen/tool/sse.pm : diagrams/sse.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/sse.dia

libgen/tool/gls.pm : diagrams/gls.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/gls.dia

libgen/tool/frem.pm : diagrams/frem.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/frem.dia

libgen/tool/ebe_npde.pm : diagrams/ebe_npde.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/ebe_npde.dia

libgen/tool/xv_step.pm libgen/tool/xv.pm : diagrams/xv.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/xv.dia

libgen/tool/lasso.pm : diagrams/lasso.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/lasso.dia

libgen/tool/npc.pm : diagrams/npc.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/npc.dia

libgen/tool/mcmp.pm : diagrams/mcmp.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/mcmp.dia

libgen/tool/pind.pm : diagrams/pind.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/pind.dia

libgen/tool/nonpb.pm : diagrams/nonpb.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/nonpb.dia

libgen/tool/bootstrap.pm : diagrams/bootstrap.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/bootstrap.dia

libgen/tool/randtest.pm : diagrams/randtest.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/randtest.dia

libgen/tool/scm/config_file.pm libgen/tool/scm.pm : diagrams/scm.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/scm.dia

libgen/tool.pm : diagrams/tool.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/tool.dia

libgen/table_file.pm libgen/model%.pm : diagrams/model.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/model.dia

libgen/output%.pm : diagrams/output.dia
	$(DIA2CODE) -t perl -d $(DIRPM) diagrams/output.dia

documents: $(DOCUMENTS)

$(addprefix html/bootstrap,$(HTML_STUBS)) : bin/bootstrap lib/common_options.pm
	perl $< --help --html

$(addprefix html/cdd,$(HTML_STUBS)) : bin/cdd lib/common_options.pm
	perl $< --help --html

$(addprefix html/execute,$(HTML_STUBS)) : bin/execute lib/common_options.pm
	perl $< --help --html

$(addprefix html/llp,$(HTML_STUBS)) : bin/llp lib/common_options.pm
	perl $< --help --html

$(addprefix html/scm,$(HTML_STUBS)) : bin/scm lib/common_options.pm
	perl $< --help --html

doc/%.pdf: doc/%.tex
	cd doc; pdflatex $*.tex
	cp doc/*.pdf PsN-Source/lib/doc 

release: libgen rel_dir $(RELFILES) $(PDFFILES)
	@ cd PsN-Source/lib/doc/; zip PsN_pdf_documentation *.pdf *.xls *.scm
	@ cd PsN-Source/lib/doc/; tar -czf PsN_pdf_documentation.tar.gz *.pdf *.xls *.scm
	@ zip -r PsN-Source PsN-Source/
	@ tar czf PsN-Source.tar.gz PsN-Source/

documentation: lib/doc/*.pdf $(PDFFILES)
	@ cd PsN-Source/lib/doc/; zip PsN_pdf_documentation *.pdf *.xls *.scm
	@ cd PsN-Source/lib/doc/; tar -czf PsN_pdf_documentation.tar.gz *.pdf *.xls *.scm 


BINFILES= boot_scm \
	bootstrap \
	cdd \
	crossval \
	ebe_npde \
	execute \
	extended_grid \
	lasso \
	linearize \
	llp \
	mc_cdd \
	mcmp \
	mimp \
	nonpb \
	npc \
	parallel_retries \
	pind \
	psn \
	psn_options \
	randtest \
	runrecord \
	scm \
	se_of_eta \
	sse \
	sumo \
	update_inits \
	vpc \
	xv_scm \

bin/completion_files:
	@ mkdir -p bin/completion_files

.PHONY: completion
completion: bin/completion_files
	cd bin; \
	$(foreach file, $(BINFILES), perl genauto $(file) >completion_files/$(file);)





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
