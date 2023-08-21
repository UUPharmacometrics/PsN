LIBFILES= ui.pm \
	logging.pm \
	logging/logger.pm \
	pharmml.pm \
	so.pm \
	rplots.pm \
	math.pm \
	PsN.pm \
	model_transformations.pm \
	model_approximations.pm \
	code.pm \
	code_parsing.pm \
	common_options.pm \
	input_checking.pm \
	linear_algebra.pm \
	simeval_util.pm \
	psn.conf_template \
	OSspecific.pm \
	binning.pm \
	array.pm \
	citations.pm \
	table.pm \
	nmtable.pm \
	nmtablefile.pm \
	filter_data.pm \
	boxcox.pm \
	include_modules.pm \
	scm_util.pm \
	ext/Statistics/Distributions.pm \
	status_bar.pm \
	model_plus.pm \
	monitor.pm \
	scmlogfile.pm \
	scmlogstep.pm \
	scmplus.pm \
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
	model/problem/record_order.pm \
	model/problem/code_record.pm \
	model/problem/init_record.pm \
	model/problem/abbreviated.pm \
	model/problem/aes.pm \
	model/problem/aesinitial.pm \
	model/problem/contr.pm \
	model/problem/covariance.pm \
	model/problem/data.pm \
	model/problem/des.pm \
	model/problem/design.pm \
	model/problem/chain.pm \
	model/problem/rcov.pm \
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
	model/annotation.pm \
	model/annotation/tag.pm \
	output/problem/subproblem.pm \
	output/problem.pm \
	output.pm \
	model.pm \
	so/xml.pm \
	so/matrix.pm \
	so/table.pm \
	so/soblock.pm \
	so/parsers/nmoutput.pm \
	so/parsers/bootstrap.pm \
	so/parsers/execute.pm \
	so/parsers/sse.pm \
	so/parsers/vpc.pm \
	so/parsers/psn.pm \
	so/parsers/nca.pm \
	so/soblock/estimation.pm \
	so/soblock/rawresults.pm \
	so/soblock/simulation.pm \
	so/soblock/taskinformation.pm \
	so/soblock/modeldiagnostic.pm \
	so/soblock/modeldiagnostic/diagnosticstructuralmodel.pm \
	so/soblock/estimation/individualestimates.pm \
	so/soblock/estimation/ofmeasures.pm \
	so/soblock/estimation/populationestimates.pm \
	so/soblock/estimation/precisionpopulationestimates.pm \
	so/soblock/estimation/residuals.pm \
	so/soblock/estimation/individualestimates/estimates.pm \
	so/soblock/estimation/individualestimates/randomeffects.pm \
	so/soblock/estimation/populationestimates/bootstrap.pm \
	so/soblock/estimation/precisionpopulationestimates/bootstrap.pm \
	so/soblock/estimation/precisionpopulationestimates/mle.pm \
	so/soblock/rawresults/datafile.pm \
	so/soblock/simulation/simulationblock.pm \
	so/soblock/simulation/simulationblock/simulationtable.pm \
	so/soblock/taskinformation/message.pm \
	tool.pm \
	tool/modelfit.pm \
	tool/sir.pm \
	tool/benchmark.pm \
	tool/llp.pm \
	tool/cdd.pm \
	tool/cdd/jackknife.pm \
	tool/sse.pm \
	tool/gls.pm \
	tool/frem.pm \
	tool/simeval.pm \
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
	tool/precond.pm \
	tool/pvar.pm \
	tool/proseval.pm \
	tool/nonparametric.pm \
	tool/resmod.pm \
	tool/linearize.pm \
	tool/qa.pm \
	tool/boot_randtest.pm \
	utils/file.pm \
	utils/phitable.pm \
	pharmpy.conf

BINFILES=$(wildcard bin/*)

RELFILES=$(addprefix PsN-Source/lib/,$(LIBFILES)) \
	$(addprefix PsN-Source/, \
	$(BINFILES) \
	setup.pl \
	AUTHORS \
	LICENSE \
	README.txt )

TEXFILES=$(wildcard doc/*.tex)
PDFFILES=$(TEXFILES:.tex=.pdf)
VERSION=`sed -n 's/.*\$version\s*=\s*.\([0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\).;/\1/p' lib/PsN.pm`
TARFILE="PsN-${VERSION}.tar.gz"
NMOUTPUT2SOFILE="nmoutput2so-${VERSION}.zip"

main:
	@ cp bin/update_inits bin/update

.PHONY : clean

clean:
	@-rm -rf $(DOCUMENTS) nmoutput2so nmoutput2so-*.zip PsN-Source doc/*.bbl doc/*.bcf doc/*.blg doc/*-blx.bib doc/*.xml doc/*.aux doc/*.log doc/*.pdf doc/inputs/*eps-converted-to.pdf doc/inputs/version.tex PsN-*.tar.gz

version:
	@cd doc; sed -n 's/.*\$version\s*=\s*.\([0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\).;/\\newcommand\{\\psnversion\}\{\1\}/p' ../lib/PsN.pm >inputs/version.tex

doc/%.pdf: version doc/%.tex
	cd doc; latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $*.tex

doc: $(PDFFILES) 

release: main rel_dir $(RELFILES) $(PDFFILES)
	rm -f $(TARFILE)
	mkdir -p PsN-Source/development
	mkdir -p PsN-Source/test
	cp -ar test/unit PsN-Source/test
	cp -ar test/system PsN-Source/test
	cp -ar test/test_files PsN-Source/test
	cp -ar R-scripts PsN-Source/lib
	cp test/includes.pm PsN-Source/test
	cp test/runsystem PsN-Source/test
	cp doc/PsN.bib PsN-Source/lib
	chmod -R a+r PsN-Source/test/test_files
	sed -i 's/dev\s*=\s*1;/dev = 0;/' PsN-Source/lib/PsN.pm
	cp -ar PsNR PsN-Source
	ls pharmpy
	cd pharmpy; tox -e plain -- pharmpy --version
	cp pharmpy/.tox/dist/*.zip PsN-Source/
	cp pharmpy/requirements.txt PsN-Source/
	tar czf $(TARFILE) PsN-Source/

# Release the nmoutput2so separately
nmoutput2so: version
	@ rm -rf nmoutput2so
	@ cd doc; pdflatex nmoutput2so_userguide.tex >/dev/null; pdflatex nmoutput2so_userguide.tex >/dev/null; biber nmoutput2so_userguide >/dev/null; pdflatex nmoutput2so_userguide.tex >/dev/null
	@ mkdir nmoutput2so
	@ mkdir nmoutput2so/bin
	@ mkdir nmoutput2so/doc
	@ cp doc/nmoutput2so_userguide.pdf nmoutput2so/doc
	@ cp bin/nmoutput2so nmoutput2so/bin
	@ cp -r lib/ nmoutput2so
	@ rm -r nmoutput2so/lib/tool
	@ rm -r nmoutput2so/lib/nonmemrun
	@ mv nmoutput2so/lib/psn.conf_template nmoutput2so/lib/psn.conf
	@ zip -r ${NMOUTPUT2SOFILE} nmoutput2so/


COMPFILES=boot_scm \
	bootstrap \
	cdd \
	crossval \
	simeval \
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
	npfit \
	parallel_retries \
	pind \
	psn \
	psn_options \
	pvar \
	randtest \
	rawresults \
	runrecord \
	scm \
	sse \
	sumo \
	update_inits \
	vpc \
	xv_scm \

PsN-Source/setup.pl: bin/setup.pl
	@ cp bin/setup.pl $@

PsN-Source/%: %
	@ cp -ar $* $@

rel_dir:
	@ rm -rf PsN-Source
	@mkdir -p $(sort $(dir $(RELFILES)))
