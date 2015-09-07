<script>
    $(document).ready(function(){
       $('#show_older_changes').click(function(){
            $('#show_older_changes').hide();
            $('#older_changes').slideDown("slow");
            
       });
    });
</script>
<script>
    $(document).ready(function(){
       $('#show_bugfixes').click(function(){
            $('#show_bugfixes').hide();
            $('#bugfixes').slideDown("slow");
            
       });
    });
</script>
<script>
    $(document).ready(function(){
       $('#show_features').click(function(){
            $('#show_features').hide();
            $('#features').slideDown("slow");
            
       });
    });
</script>
<h2>Perl-speaks-NONMEM</h2>
  <p>Perl-speaks-NONMEM (PsN) is a collection of Perl modules and programs aiding in the development
    of non-linear mixed effect models using NONMEM. The functionality ranges from solutions to simpler
    tasks such as parameter estimate extraction from output files, data file sub setting and resampling,
    to advanced computer-intensive statistical methods. PsN includes stand-alone tools for the end-user
    as well as development libraries for method developers. You can read more about the different parts of PsN under <a href="docs.php">Documentation</a>.</p>
<h3>Latest news</h3>
 <p>
Version 4.4.8 was released September 7th 2015, and is available <a href="download.php">here</a>.
</p>
 <p>
The focus of this release is bug fixes and extensions of the test suite. Several of the bugs have been in PsN since version 3.7.6, some are even older. There are only minor new features.
</p>

 <li>Installation on Windows: Users who install PsN for the first time need to make sure they install a Perl version where
the needed extra modules are available, see instructions on
<a href="install.php">installation page</a>.
</li>
 <li> Click <a href="http://sourceforge.net/p/psn/wiki/Bug_list_current_release" target="_blank">here</a> to open the known bugs wiki with the most recent information regarding known bugs in the latest release.
</li>
</p>

<div style="text-align:left;"><a style="font-size:small;color:gray;" href="#" id="show_older_changes">Click here to show change notes for older versions</a></div>
<div id="older_changes" style="display:none;">  
<h3>PsN 4.4.0</h3>
<p>
Version 4.4.0 was released April 20th 2015.
</p>
<p>
New features of version 4.4.0 include precond, a new tool for automatic preconditioning of a NONMEM model to improve its numerical properties,
major improvements to sir, a tool for obtaining parameter uncertainty faster than with a bootstrap and more accurately than based on the covariance step,
and the -rplots option, a simple yet powerful mechanism for generating default or custom R plots for visualizing PsN output.
</p>
<p>
The <a href="http://www.page-meeting.org/default.asp?abstract=3586" target="_blank">preconditioning method</a>, 
which has been developed by Yasunori Aoki, was presented during the 
Stuart Beal Methodology Session at PAGE 2015.
</p>
<p>
Default R templates are available for a number of tools, including 
<a href="http://www.page-meeting.org/default.asp?abstract=3103" target="_blank">nca</a> (non-compartmental 
analysis using the ncappc package available from CRAN), 
<a href="http://www.page-meeting.org/default.asp?abstract=2907" target="_blank">sir</a>, 
sse for <a href="http://www.page-meeting.org/default.asp?abstract=3263" target="_blank">parametric power estimation</a>, vpc, 
bootstrap and execute.
</p>
<p>
The complete list of new features and bugfixes in 4.4.0 is found  
<a href="pdfdocs/release_notes_PsN_4_4_0.txt" target="_blank">here</a></dt>.
</p><p>The list of bugs in 4.4.0 that were known at release of 4.4.8 is found
<a href="pdfdocs/bug_list_PsN_4_4_0_at_next_release.txt" target="_blank">here</a>
</p>
<h3>Known issues with version 4.4.0</h3>
<li>
It is important that ext,cov,coi,cor are included in the nm_output list in the [default_options] section in psn.conf, otherwise a number of bugs will be triggered. 
If psn.conf is created by the installation script then the setting of nm_output will be appropriate. 
A suitable setting is</br></br>
[default_options]</br>
nm_output=ext,cov,cor,coi,phi</br></br>
This bug is further described on the 
<a href="http://sourceforge.net/p/psn/wiki/Bug_list_current_release" target="_blank">knows bugs wiki</a>.
</li>

<h3>PsN 4.2.0</h3>
<p>
Version 4.2.0 was released May 21st 2014.
</p>
<p>
New features include new scripts nca, pvar, sir, frem and rawresults and a new test library for verification of PsN. 
Under the hood there have been big changes. PsN has been completely rewritten using the Moose object system. 
</p>
<p>
The list of new features and bugfixes in 4.2.0 is found
<a href="pdfdocs/release_notes_PsN_4_2_0.txt" target="_blank">here</a></p>
The list of bugs in 4.2.0 that were known at release of 4.4.0 is found
<a href="pdfdocs/bug_list_PsN_4_2_0_at_next_release.txt" target="_blank">here</a></p>

<h3>PsN 3.7.6</h3>
<p>
Version 3.7.6 was released December 4th 2013.
</p>
<p>
New features include automatic binning and 10 times faster DV processing in vpc, a new program randtest for randomization testing, and more alternatives for simulation with uncertainty.
</p>
<p>
The list of new features and bugfixes in 3.7.6 is found
<a href="pdfdocs/release_notes_PsN_3_7_6.txt" target="_blank">here</a></p>

<h3>PsN 3.6.2</h3>
 <p>
Version 3.6.2 was released June 4th 2013. 
</p>
 <p>
The most important change in 3.6.2 is a completely redesigned NONMEM output file parser, which affects every single PsN run. 
PsN 3.6.2 supports NM 7.3 beta, while PsN 3.5.3 is guaranteed to crash with NM 7.3 beta due to changes in the
output file format.
There are very few new features compared to 3.5.3, the focus has been on bugfixes and the new parser.
</p>
<p>
The list of new features and bugfixes in 3.6.2 is found  
<a href="pdfdocs/release_notes_PsN_3_6_2.txt" target="_blank">here</a></dt>.
</p>

<h3>New features in PsN 3.5.3</h3>
<ul>
<li> Installation
<ul>
Major redesign of the installation script. Extensive error checking, automatic generation of configuration
file psn.conf for basic installations, automatic copying of documentation to user-specified location.
Allow multiple missing levels in installation path when creating installtion directory. 
Added possibility to install symlinks/extra .bat-files psn_<program_name> for all psn programs, 
to avoid conflicts will already installed software.
</ul>
</li>
<li> NONMEM interaction
<ul>
<li>
Support for $SIZES and $BIND.
</li>
<li>
Support for NMQual8. See instructions in psn_configuration.pdf.
</li>
<li>
The user can now decide which NONMEM7 output files PsN should copy back to the calling directory. 
By default only the lst-file is copied back (in addition to tables and extra output files).
If option -nm_output, e.g. -nm_output=ext,cov,phi,coi,cor is set, then the NM7 output files with those 
extensions will also be copied to the calling directory. Note that the automatically
generated psn.conf, if used, has nm_output=ext,cov,phi,coi,cor set.
</li>
<li>
Set name of custom nmfe wrapper in psn.conf, and invoke NONMEM via this wrapper instead of running nmfe directly. Useful e.g. if running NONMEM remotely 
on a cluster requires some environment variable changes that can be implemented
in a wrapper. 
</li>
</ul>
<li> General
<ul>
<li>
New formatting of control streams: In earlier PsN versions $OMEGA was always printed before $PK. 
The new default is to print $OMEGA after $THETA. To print $OMEGA before $PK set option -omega_before_pk. 
</li>
<li>
Running on LSF: New option run_on_lsf_nmfe for submitting nmfe directly via bsub instead of
submitting a perl process with nonmem.pm. Improved error checking, better error handling when submisison
fails, and more logging to disk. Jobs are submitted via a script, lsf_jobscript, which simplifies debugging when submission fails.  
</li>
<li>
When option -silent is set, all messages will be printed to file run_messages.txt in the main run 
directory instead of to screen. This helps diagnostics when runs fail.
</li>
<li>
New option -tbs, Transform Both Sides, for automatic Box-Cox transformation and estimation of lambda.
See common_options_defaults_versions_psn.pdf for details.
</li>
<li>
New optionc d2u. Default not set. If set then run dos2unix on model files and regular data files 
(not extra data files) before starting NONMEM if they seem to have windows type line breaks.  
</li>
<li>
When a run is resumed (same command with -dir set to existing directory) then the new command is appended 
to command.txt, instead of overwriting old command.txt. Gives better logging of runs in e.g. sse.
</li>
<li>
New option -add_retries. By default, if file stats-runs.csv found in NM-run subdirectory then 
that run is considered finished and no check of tries is done. If option -add_retries is set when a PsN run 
is restarted, PsN will ignore the existence of stats-runs.csv and investigare existing retries 
to see if more are needed based on (new) settings of e.g. retries and picky. 
</li>
<li>
Name raw results after the model file, 
e.g. raw_results_run1.csv if running execute run1.mod. This makes it possible to open
several raw results files in Excel without renaming them first.
</li>
<li>
New columns in raw results: subprob_est_time, subprob_cov_time, model_run_time, est_methods. 
For Bayes/SAEM nburn_set, nburn_iter, nburn_conv.
</li>
<li>
New principle for which retry is selected, and when a retry should be performed. 
See common_options_defaults_versions.pdf for details. 
</li>
<li>
Partial support for $PRIOR NWPRI. Explicitly handled in vpc, sse and update_inits. Not handled in lasso, gls, scm
and with option -tbs. Implicitly handled, but not well testen, 
in all scripts where new $THETA are not added by PsN.
</li>
</ul>
<li>  <a href='pdfdocs/gls_userguide.pdf' target="_blank">gls</a>
<ul>
A new script for using generalized least squares approximations in the residual error model.
</li>
</ul>
</li>
<li>  <a href='pdfdocs/mimp_userguide.pdf' target="_blank">mimp</a>
<ul>
A new script for multiple imputation of missing covariate values.
</li>
</ul>
</li>
<li> update_inits
<ul>
<li>
New option -sigdig=N. Reduce the number of significant digits in parameter estimates to N. 
Only works for NONMEM7 and later.
</li>
<li>
New option -degree=X. Randomly perturb initial estimates by degree X after updating them.
</li>
<li> 
New option -add_prior. Add $PRIOR NWPRI based on output files. Experimental, output must
be checked before being used.   
</li>
<li> 
Added possibility to invoke update_inits with command update. 
</li>
</ul>
<li>  <a href='pdfdocs/sumo_userguide.pdf' target="_blank">sumo</a>
<ul>
<li>
Added model run time, subproblem estimation time, subproblem covariance time in output. 
Add explanatory message about results of sd_rse setting. 
</li>
<li>
Read NM-version from lst-file directly to get settings right, instead of trusting default in psn.conf. 
</li>
<li>
Added reading of default_sumo_options from psn.conf, to make it possible to set defaults for 
this program.
If user wants to set own sumo defaults then must be done 
in section [default_sumo_options].
</li>
<li>
New sumo userguide document.
</li>
</ul>
</li>
<li>  <a href='pdfdocs/boot_scm_userguide.pdf' target="_blank">boot_scm</a>
<ul>
New PsN program boot_scm for bootstrapped stepwise covariate modeling (scm).
</li>
</ul>
</li>
<li>  <a href='sse_userguide.pdf' target="_blank">sse</a>
<ul>
<li> 
Simulation with uncertainty from $PRIOR TNPRI, $PRIOR NWPRI or a boostrap results file (options 
-rawres_input and -offset_rawres)
</li>
<li> 
New option -in_filter to be used with -rawres_input. Filter sets of input parameters
before using for simulation with uncertainty.
</li>
<li> 
New option -out_filter, e.g. only compute statistics for runs with minimization successful.
</li>
<li> 
New option -recompute. Create new sse_results file, e.g. using new setting of -out_filter,
without rerunning any models. 
</li>
<li> 
If -add_models and -estimate_simulation set then reread estimated simulation models, if existing, from 
original run. This saves time.
If no-estimate_simulation is set then no estimated simulations will be read even if they exist.
</li>
<li> 
Keep $TABLE in simulation models, file names are numbered with -sim-N.
</li>
</ul>
</li>
<li>  <a href='scm_userguide.pdf' target="_blank">scm</a>
<ul>
<li>
Data file format: To handle functionality in boot_scm and xv_scm, which in turn call the scm
program, the data file for any scm run must have a header line, and the column headers must 
match $INPUT of the input model.
</li>
<li>  
New option -time_varying=<list of covariates> to correctly compute median and mean 
of time-varying covariates. Instructions and formula details in userguide. 
</li>
<li> 
Detection of time-varying covariates during computation of covariate statistics
(single individual has multiple values of covariate). Warn the user that centering
will not be correct if option time_varying is not set for this covariate.
</li>
<li> 
Pre-filtering of data if IGNORE()/ACCEPT() found in control stream. Will make
sure covariate statistics are computed based only on included observations.
</li>
<li> 
Allow setting -nmfe_options in scm config file.
</li>
<li> 
New option -noabort for scm -linearize: add NOABORT to $EST for the linearized models. 
Option has no effect unless -linearize is set.
</li>
<li> 
Added possibility to use 'mean' in [code], [inits] and bounds sections, just as median, minimum and maximum.
</li>
<li> 
Warn if $PRIOR NWPRI in input model (not supported).
</li>
<li> 
Allow user to call scm with 'scm config.scm' directly instead of having to set scm -config_file=config.scm.
</li> 
</ul>
</li>
<li> <a href='pdfdocs/npc_vpc_userguide.pdf' target="_blank">vpc/npc</a>
<ul>
<li>
If -tte is set: More cleaning to save disk space. 
Allow -stratify_on to be a comma-separated list of variables. PsN
will print all those variables in the file used as Xpose input to kaplan.plot.
</li>
<li>
Simulation with uncertainty added. Note: It is normally not appropriate to use simulation with uncertainty 
in vpc. Invoked by setting options -rawres_input and -offset_rawres 
or by having $PRIOR in the input/simulation model, same as for sse. 
</li>
</ul>
</ul>


<h3>Bug fixes in PsN 3.5.3</h3>

<ul>

<li> General
<ul>
<li>
Improvements of NONMEM output file reading: More robust detection of starttime line. 
Handle NM7.2-new variants of 'HESSIAN OF POSTERIOR DENSITY NON...'.
Handle NPARAMETR lines when reading iteration path. 
When reading initial estimates from the lst-file and find OMEGA BLOCK STRUCTURE but 
not OMEGA HAS BLOCK FORM, assume only diagonal elements are non-zero 
and read last init (diagonal element) on each row. 
</li>
<li> 
Simply ignore PsN option -nmfe_options unless NM7.2 or later instead of aborting execution with error message.
</li>
<li> 
Improved stratification in cross-validation subsetting (lasso, xv_scm). Make sure same number of individuals 
in each bin even if number of individuals in factor category not evenly divisible with number of bins. 
Also warn if number of IDs with same value of stratification variable is less than number of bins.  
</li>
<li> 
Improved PsN detection of non-computer-crash lst-file errors: If there is an end time properly printed by 
nmfe in the lst-file then never invoke handle crashes functionality. Assume some error in 
NONMEM run that made NONMEM terminate on its own, eg hessian not positive definite.
</li>
<li> 
Make PsN look for signs for nmtran errors or compilation errors even if psn.lst exists, which is
needed for NM7.2 which produces a lst-file even in the case of nmtran error or 
compilation error. 
Never invoke crash restart functionality for such errors. 
Changed error message when FDATA and lst-file do not exist, to include suggestion of file system problem.
</li>
<li> 
Make sure grid submit (lsf, sge, slurm,...) returns -1 by default (when failing to extract jobID) and handle 
this case as run finished and crashed.
</li>
<li> 
Fixed bug with run_on_sge_nmfe where setting option -nodes resulted in other options beeing ignored.
</li>
<li> 
Bugfix when option -min_retries is set: Now PsN will remember if one of the early tries was accepted as 
successful. PsN will then not do more retries after min_retries reached, even if the most recent retry was 
not accepted.
</li>
<li> 
If handle_msfo is set and msfo file already exists in main directory, PsN will 
automatically use existing msf file as input (set msfi). Bugfix: Do not do this if existing msf file is empty.
If handle_msfo: if MSFO already set in model then use that name as base_msfo_name instead of psn_msfo.
</li>
<li> 
Bugfix parafile and nodes option on windows, which caused crash in PsN-3.4.2. 
(There should be no double quotes around parafile and nodes.) 
</li>
</ul>
</li>

<li> <a href='pdfdocs/npc_vpc_userguide.pdf' target="_blank">vpc/npc</a>
<ul>
<li>
When using a separate simulation model (option -flip_comments or -sim_model), 
do not update initial estimates of MAXEVAL=0 model. 
Let options -lst and -msfo refer to initial parameter estimates for the simulation model. 
If not lst or msfo given, try to find lst-file for simulation model and only update that model. 
In vpc_results.csv, set information about simulation model 
(auto-generated, flip comments or sim_model filename) instead of 'parameter values from' 
</li>
<li> 
Skip multiplication of numerator and denominator with conjugate in predcorr, could lead to division by 0.
</li>
</ul>
<li>  <a href='pdfdocs/scm_userguide.pdf' target="_blank">scm</a>
<ul>
<li>
Better checking that output exists before trying to read e.g. ofv, give sensible error message if missing. 
</li>
<li> 
If derivatives_data given as input with -linearize then covariate statistics is now 
computed based on that file, instead of original datafile for model (important in e.g. boot_scm).
</li>
<li> 
Pass on option -only_successful to later iterations.
</li>
<li> 
Fixed bug where, if linearize set, numbering of THETAs in user-defined parameterizations would not 
work for parameterizations with more than one THETA per line. 
</li>
<li> 
Allow space after EXP in EXP(ETA(1)) etc when detecting ETA relation forms in linearized scm.
</li>
</ul>
</li>
<li>    <a href='llp_userguide.pdf' target="_blank">llp</a>
<ul>
<li> 
Fixed bug where omega and sigma diagonal entries lower limit of 0 was not respected. 
Make sure that if sigma/omega and on diagonal then lower limit stored as 0. 
</li>
<li> 
Fixed bug where computed guess -nan was not recognized as 'not a number'. 
</li>
</ul>
</li>
<li>  <a href='sse_userguide.pdf' target="_blank">sse</a>
<ul>
Add handling of BLOCK SAME in preparation of results.
</ul>
</li>
<li>  <a href='pdfdocs/sumo_userguide.pdf' target="_blank">sumo</a>
<ul>
<li> 
Fixed bug when checking if omega/sigma estimate is close to bound.
</li>
<li> 
Fixed bug when SIGMA 0 block was printed even if no sigma in model. 
</li>
</ul>
</li>
<li>  update_inits
<ul>
Fixed bug in MSFO-filename renumbering. Complete rewrite to make script more stable.
</ul>
</li>
<li>  <a href='nonpb_userguide.pdf' target="_blank">nonpb</a>
<ul>
Made nonpb and pind work with NONMEM7 (removed fortran77 comment line in user-written routine). 
</ul>
</li>
<li>  <a href='pdfdocs/runrecord_userguide.pdf'PsN-Toolkit--a collection of computer intensive statistical methods for non-linear mixed effect modeling using NON MEM.
Comput Methods Programs Biomed. 2005 Sep;79(3):241-57.
Lindbom L, Ribbing J, Jonsson EN.
Perl-speaks-NONMEM (PsN)--a Perl module for NONMEM related programming.
Comput Methods Programs Biomed. 2004 Aug;75(2):85-94. target="_blank">RunRecord</a>
<ul>
Handle 'Based on <myself>' input error in runrecord. Print warning and ignore 'Based on' tag. 
Otherwise this input error would make runrecord produce no output.
</ul>
</li>
</ul>
<br>


<h3>New features in PsN 3.4.2</h3>
<ul>
<li> NONMEM 7.2 support
<ul>
<li>
Options -parafile, -nodes and -nmfe_options can be used to pass on a parafile name, 
the number of nodes and nmfe options such as xmloff to nmfe. This requires that 
NONMEM version 7.2 has been specified in psn.conf and that option -nmfe or 
-run_on_sge_nmfe is set.
<br>
If -nmfe or -run_on_sge_nmfe is not set then NM7.2 is not supported.
</li>
<li>
A new option -nmqual_options can be used to pass on options when -nmqual is set.
</li>
<li>
PsN does not support any but the default value of ORDER in $EST. If option ORDER 
is found it will be removed by PsN.
</li>
<li>
PsN will keep STANDARD CORRELATION CHOLESKY in model file if found, but print warning
that output handling, especially setting initial estimates in a new file to the final 
estimates from a previoud run, may not work correctly.
</li>
<li>
Cleaning of NM7.2 output files: If clean>=1 then the following files are removed	
LINKC.LNK, compile.lnk, gfortran.txt, ifort.txt, garbage.out,
newline, nmexec.set, parafile.set, prcompile.set, prdefault.set,
prsame.set, psn.log, rundir.set, runpdir.set, temporaryfile.xml;
temp.out, trashfile.xxx, trskip.set, worker.set, xmloff.set
prsizes.f90, licfile.set, background.set, FMSG, FSIZES.
If clean >=2 then PsN will remove the temp_dir subdirectory of NM_run.
</li>
<li>
PsN will set retry numbering for NM7.2 new output files .xml .phm .shk .grd .smt .rmt
just as for .lst etc, and copy the output from the selected run back to the calling 
directory.
</li>
</ul>
<li> General
<ul>
<li>
Installation: If windows and path to perl binary chosen by user is not the same as default 
in psn.conf, write warning that psn.conf must be edited. Require y or n answer 
for yes/no questions instead of treating no anwer as no, which could lead to 
premature termination of the installation process. 
</li>
<li>
Installation: More explicit error checking of each step in the installation process,
and more detailed error messages.
</li>
<li>
New options -torque_queue and -torque_prepend_flags to allow more flexible settings 
when running on torque.
</li>
<li>
Made option -retries and -min_retries consistent. Option -min_retries is 
defined as before, but option -retries is now the maximum number of 
*extra* tries, instead of the maximum number of tries as before.  
</li>
<li>
Support for SLURM queueing system, options -run_on_slurm, -email_address,
-send_email, -slurm_project, -max_runtime (maps to squeue -t). Currently 
-run_on_slurm only works with option -nmfe set.
</li>
<li>
Changed meaning of option max_runtime. Now only allowed for run_on_slurm.
</li>
<li>
Iterations output to screen: New option -display_iterations common to all 
PsN scripts. By default not set. In template psn.conf set as default for 
execute program but not the others. 
</li>
<li>
Option stop_motion for educational purposes: See 
common_options_defaults_versions.pdf for instructions.
</li>
<li>
New option -maxevals. Set on command-line to allow more evalutations than
NONMEMs 9999 using sequence of msfo-msfi files.
</li>
<li>
Removed option significant_digits_rerun. Created new option 
significant_digits_accept with same functionality but correct documentation.
</li>
<li>
modified sge_monitor to read stderr directly instead of creating file with
JobID and then opening it.
</li>
<li>
Make all scripts print <scriptname> done at the end, e.g. "sse done". 
</li>
<li>
Skipped reset of FORMAT and DELIM option to make formatting more flexible,
e.g. allow more significant digits. If however user sets delimiter to other 
than default then PsN will not be able to parse output.
</li>
<li>
Make PsN use higher precision for initial estimates in NM7 model files.
Up to 15 sigdigits.
</li>
<li>
Changed output parsing so that a model with only a $TABLE and no $EST or 
$SIM will not be handled as crashed. In read_eststep handle finding of 
'TABLES STEP OMITTED' (set est step initiated, run, sim step run to 0) 
separately from 'COVARIANCE STEP OMITTED' (error). In read_inits stop 
reading if encounter 'TABLES STEP OMITTED' in same places as ESTIMATION 
or SIMULATION STEP OMITTED.
</li>
<li>
Changed handling of retries. After a NONMEM run is finished then psn.mod .lst .cov etc 
will be moved instead of copied to numbered retry files psn-N.mod etc. After all retries 
are finished and the best retry has been selected, then that retry's files are copied to 
psn.mod, psn.lst. The file stats-runs.csv contains, as in earlier PsN versions, a summary 
of the best run's results.
</li>
<li>
Changed restart procedure. PsN will check if stats-runs.csv already exists in the NM_run 
subdirectory. If is exists it is proof that all retries of the model are already finished, 
and PsN will not start any NONMEM run for this model. PsN will parse psn.lst as the selected 
model and use this when producing raw_results and other output later. This procedure is new, 
in earlier PsN versions the existing psn.mod file would always be overwritten, and would 
also be rerun unless a numbered retry file was present in the directory. The fact that 
stats-runs.csv is used as a flag for all retries done makes it possible to restart and 
reuse old partial results also if -clean=2. In older versions PsN would use the numbered 
retry file, which is removed if -clean=2, as the signal. 
<br>
If stats-runs.csv does not exist but psn.lst does, then PsN will assume the main PsN 
process has been interrupted while the NONMEM run subprocess has finished, e.g. in a 
process submitted to a grid, giving the psn.lst output file. PsN will check for existing 
numbered retry files in the NM_run directory to figure out which retry the psn.lst file 
belongs to, and summarize the results or start a new retry based on the contents of psn.lst 
and the settings for retries.
<br>
When restarting PsN will also check for numbered crash files, so that the counting of 
restarts after a crash is not reset every time the main PsN process is restarted. 
<br>
PsN will not copy psn.mod to NM_run if there already is such a file there, because it 
either means the previous run was interrupted, or that everything is finished for that 
model (stats-runs.csv exists).
</li>
</ul>
<li>  <a href='pdfdocs/execute_userguide.pdf' target="_blank">parallel_retries</a>
<ul>
A new script for running retries in parallel. Very similar to execute, and described in the execute userguide.
</li>
</ul>
</li>
<li> psn_clean
<ul>
<li>
changed psn_clean to remove both .dta and .dat files if option dta is set, 
not only .dta
</li>
<li>
New option -level to psn_clean, values 2-4, mimics -clean to regular PsN 
scripts. Extra level 4 for complete removal of m1. Make psn_clean keep file
stats-runs.csv even if option .csv is set, to enable reusage of runs after 
clean level 2.
</li>
<li>
psn_clean does not yet support NM7.2.
</li>
</ul>
<li> sumo
<ul>
Suppress off-diagonal zeros in OMEGA and SIGMA output.
</li>
</ul>
</li>
<li> <a href='pdfdocs/npc_vpc_userguide.pdf' target="_blank">vpc/npc</a>
<ul>
<li>
Accept models with stratification variable different from STRT and not in $INPUT 
  or found in $PRED/$PK/$ERROR. Print warning and continue. Similar for idv.
</li>
<li>
More flexible definition of simulation model using -sim_model or -flip_comments.
</li>
<li>
tte option to vpc for (repeated) time-to-event models. Needs Xpose for plotting (as all vpc).
</li>
<li>
option boxcox_lambda to handle Box-Cox transformed data.
</li>
<li>
new option confidence_interval, default is 95 (%)
</li>
<li>
New option -censor=<variable> to vpc and npc. Variable to indicated if an observation 
  is missing. Can be used e.g. when modelling dropout.
</li>
<li>
Added possibility in vpc to use -lloq or -uloq in combination with predcorr.
</li>
</ul>
<li>  <a href='pdfdocs/xv_scm_userguide.pdf' target="_blank">xv_scm</a>
<ul>
New PsN program xv_scm for cross-validated stepwise covariate modeling (scm).
</li>
</ul>
</li>
<li>  <a href='lasso_userguide.pdf' target="_blank">lasso</a>
<ul>
New script for covariate model building using the method presented in <br>
The lasso, a novel method for predictive covariate model building in nonlinear 
mixed effects models. <br>
Jakob Ribbing, Joakim Nyberg, Ola Caster, E. Niclas Jonsson. <br>
J Pharmacokinet Pharmacodyn (2007) 34:485–517<br>
</li>
</ul>
</li>
<li>  <a href='nonpb_userguide.pdf' target="_blank">nonpb</a>
<ul>
<li> 
More documentation
</li>
<li> 
Changed default method to 1 (short version)
</li>
</ul>
</li>
<li>  <a href='scm_userguide.pdf' target="_blank">scm</a>
<ul>
<li> 
A set of template and example configuration files. Found in the documentation directory together with all userguides.
</li>
<li> 
Add support in scm linearize for more than 50 items in derivatives_covariates 
dataset. 
</li>
<li> 
Changed the default value of scm:s -foce option. Now the default is to use 
foce (-foce on commandline, foce=1 in config file). 
</li>
<li> 
new option scm: max_steps. Do not add more covariates even if more left to 
test and last addition significant. 
</li>
</ul>
</li>
<li>  update_inits
<ul>
<li> 
Set -renumber=Y by default (renumber table files and msfo files) 
if input model and output model
match pattern runX.mod/ctl runY.mod/ctl where X and Y are numbers.
</li>
<li> 
Added feature to only update single problem in subroutine update_inits 
</li>
</ul>
</li>
</ul>

<h3>Bug fixes in PsN 3.4.2</h3>

<ul>

<li> General
<ul>
<li>
use function localtime instead of date and time functions when printing start and 
finish times of nm runs to lst-files. Gets rid of annoying error messages 
on some windows versions
</li>
<li> 
Fixed bug introduced in 3.1.12 that if there is code preceeding " FIRST in 
$PK then PsN will move this code to after "FIRST... section.
</li>
<li> 
Do not add $EST if not present when setting maxeval=0 for NM7. Fixed bug in 
set_maxeval_zero so that estimation record is not added if it is not present in input model
</li>
<li> 
Fixed bug in parsing of NM7 raw output that if PsN does not find the line 
with ofv and final estimates in psn.ext, parsing of that file will be aborted 
and PsN will pick everything from lst-file instead. Handles for example the 
case when NONMEM prints an empty table to psn.ext.
</li>
<li> 
Bugfixing in wrap_data feature, and added NM7 support (max 50 items per line 
instead of 20), add support for using items from CONT=1 in $PRED. Handle 
existing MDV correctly, handle DV correctly. Added warning if wrap_data 
without MDV in dataset. 
</li>
<li> 
When submitting to lsf: also check lsf_err for JobId, to handle case when 
diagnostic output is redirected to stderr. BSUB_STDERR redirectes LSF 
diagnostic messages to stderr. 
</li>
<li> 
fixed bug in sge_submit: prepend jobname with psn: if jobname (model name) starts with digit 
</li>
<li> 
fixed bug in problem_subs.pm drop_dropped that PsN handles both VALUE=DROP and 
DROP=VALUE, also in data_subs.pm
</li>
<li> 
clarified error message in model_subs.pm when modelfile does not exist.
</li>
<li> 
add check in subproblem_subs.pm that matrix reference is not empty when calling MatrixReal -> new_from_cols
</li>
</ul>
</li>

<li> <a href='pdfdocs/npc_vpc_userguide.pdf' target="_blank">vpc/npc</a>
<ul>
<li>
Accept any number of seeds in existing $SIM.
</li>
<li> 
Removed risk of divide by 0 in message of how many percent DV processing 
is done.
</li>
<li> 
remove MSFO option from $NONP if it is there.
</li>
<li> 
make npc and vpc write changed models to disk in m1 so that m1-models 
reflect what is run in NM_run1 and 2.
</li>
<li> 
add check in npc_subs that reading of original data file gave back some individuals 
and not an undefined value
</li>
</ul>
<li>  <a href='pdfdocs/scm_userguide.pdf' target="_blank">scm</a>
<ul>
<li>
Fixed bugs in handling of initial estimates that sometimes could cause
inits to be outside boundaries: Do not round initial values (handled by 
init_option_subs - check_and_set_init). Only call format_init_bounds if 
first recursion level (step_number==1). Set bounds before inits in add_code 
and add_code_linearize. 
</li>
<li> 
fixed bug in scm, bad detection of logit parameter ETAs (= instead of ==)
</li>
<li> 
set default FORMAT in linearized model in scm to s1PE17.10 to avoid case
when NM considers its own OMEGA output as non-positive definite. Is solved 
with higher precision.
</li>
<li> 
better handling of crashed models in scm: print FAILED in logfile and 
continue instead of crashing because trying to retrieve ofv from model with 
no output.
</li>
<li> 
in scm -linearize set IGNORE=@ in linearized model
</li>
</ul>
</li>
<li>  <a href='sse_userguide.pdf' target="_blank">sse</a>
<ul>
<li>
Fixed bug in labeling in sse_results.csv, numbering of alternative models 
in section on Type I and Type II error rates when option -add_models is used. 
</li>
<li> 
Fixed bug in sse when restarting and the program tries to reread estimation 
models for the simulation model even if -no-estimate_simulation was set
</li>
<li> 
fixed bug in sse that PsN handles DROP in $INPUT even if skip_data_parsing
</li>
</ul>
</li>
<li>  <a href='nonpb_userguide.pdf' target="_blank">nonpb</a>
<ul>
<li> 
More documentation
</li>
<li> 
Changed default method to 1 (short version)
</li>
</ul>
</li>
<li>  <a href='pdfdocs/runrecord_userguide.pdf' target="_blank">RunRecord</a>
<ul>
fixed bug in runrecord so that lst-files from crashed NONMEM runs are handled. 
</ul>
</li>
</ul>

<br>



<h3>New features in PsN 3.2.12</h3>
<ul>
<li> General
<ul>
<li>
Changed shrinkage computation. All ETA values that are exactly zero are 
skipped, computations only include non-zero ETAs. PsN will never
read shrinkage values from NONMEM output, and the option -shrinkage
must be set to make PsN compute shrinkage.
</li>
<li>
Save time by turning off redundant data file parsing for bootstrap,
vpc/npc, sse, cdd, llp, execute, update_inits and scm.
</li>
<li>
Much improved handling of failed/crashed NONMEM runs. 
When the lst-file is missing a row will still be printed to 
raw_results indicating the cause of the error (perl installation 
problems, NMtran failure, compilation failure). The message is 
also printed to screen. PsN will not terminate. If additional 
models are to be run after the one that failed, e.g.
in sse or bootstrap, execution will continue after the error,
and results will be based on the models that did finish.
The user can choose to terminate the PsN run after a single failed
run by setting the option abort_on_fail.
</li>
<li>
To handle possibility of slow file sync, PsN will on unix-type systems
do a `ls -la 2>&1` to try forcing a sync, wait 70s if
psn.lst is not found, but continue immediately if the file 
psn_nonmem_error_messages.txt is found (proof that psn.lst will
never appear).
</li>
<li>
tweak_inits no longer takes a value on the command-line, it is set/unset
with -tweak_inits/-no-tweak_inits
</li>
<li>
Save time by skipping second output parsing when output files
are copied back to the base directory of the model file.
</li>
</ul>
</li>
<li>  <a href='pdfdocs/mcmp_userguide.pdf' target="_blank">mcmp</a>
<ul>
<li>A new script for computing Monte-Carlo Mapped power.
Method presented at PAGE-2010:
Camille Vong, Martin Bergstrand, Mats O. Karlsson.
Rapid sample size calculations for a defined likelihood ratio test-based 
power in mixed effects models.
</li>
</ul>
</li>
<li> <a href='pdfdocs/npc_vpc_userguide.pdf' target="_blank">vpc/npc</a>
<ul>
<li>
Changed default behaviour for lst_file option: if neither -lst_file 
nor -msfo_file is specified as option, PsN will check if there is a 
lst-file with the same stem as the modelfile (e.g. run55.lst for run55.mod) 
and then update initial estimates from this lst-file before running the 
simulations. 
</li>
</ul>
</li>
<li>  <a href='pdfdocs/sse_userguide.pdf' target="_blank">sse</a>
<ul>
<li>
Changed default value of sse option -parallel_simulations: If option 
-parallel_simulations is not set, it will get the same value as option 
-threads. Before the default for -parallel_simulations was 1. 
</li>
<li>
Save disk space by letting datafiles be read from m1 instead of copied 
to NM_run directories for each nonmem run. After simulation simulated data 
is deleted from NM_run. Copies of simulated data are kept in m1, and read 
from there during estimation. 
</li>
<li>
Set default clean level to 1 in sse to make restarts work better.
</li>
</ul>
</li>
<li>  <a href='pdfdocs/scm_userguide.pdf' target="_blank">scm</a>
<ul>
<li>
Removed option -sum_covariates and added option -logit that can be set
individually for each parameter. Adapt parameterizations and covariate
addition to logit transformed parameters.
</li>
<li>
Option -linearize has been developed further and extensively tested. Details
in the userguide.
</li>
<li>
Extended possibilities to define new parameterizations of covariates, to
use different parameterizations for different parameter-covariate pairs.
</li>
<li>
Possibility to use median, maximum and minimum of covariate in user-defined
parameterizations without having to enter the numerical values.
</li>
<li>
Shortcut codes for pre-defined parameterizations to simplify using 
different parameterizations for different parameter-covariate pairs.
</li>
<li>
option -parallel_states to try all possible parameterizations for a
covariate on a parameter simultaneously, instead of only trying more
complex parameterizations after a more simple one has been found 
significant and included in the model.
</li>
<li>
Copying of final models to final_models sub-directory of run directory.
</li>
<li>
If search_direction=both the forward and backward searches are done
in the same run directory.
</li>
<li>
Possibility to set option nm_version in the configuration file.
</li>
<li>
Complete reformatting of logfile.
</li>
<li>
Anchor functionality. By default scm adds all covariate code first
in $PK/$PRED, but if the line <br>
;;;SCM-ANCHOR<br>
is found the covariate code will be added after this line instead.
</li>
<li>
Provided that perl package Statistics::Distributions is installed: 
Option p_forward and p_backward accept any value. Model selection 
in each step is based on p-value instead of as before a combination 
of p-value and ofv which could give errors when comparing models with 
different numbers of extra parameters.
</li>
<li>
Added handling of shrinkage, if requested on the command-line.
</li>
<li>
If included_relations is set but base_criteria_values is not, scm will
run the base model with the included relations instead of terminating the
run.
</li>
<li>
More input checking and improved handling of exceptions.
</li>
</ul>
</li>
<li>update_inits (use update_inits -h for help)
<ul>
<li>Will not change values that are fixed in original model.
</li>
</ul>
</li>
</ul>
<h3>Bug fixes in PsN 3.2.12</h3>
<ul>
<li>General 
<ul>
torque-queue is now read from the configuration file. Before torque-queue 
could not be set.
</li>
<li>
Wrap lines that exceed 150 characters when printing THETA/OMEGA/SIGMA
records.
</li>
<li>
Fixed bug in parsing of OMEGA/SIGMA initial estimates for block omegas/sigmas
when the block size exceeds what NONMEM will print in the lst-file.
</li>
<li>
Fixed bug in parsing of diagonal OMEGA/SIGMA when the matrix dimension
exceeds 20.
</li>
<li>
If JobStat file cannot be opened when running on sge, simply try again
instead of terminating program run.
</li>
<li>
Handle parsing of lst-files with multiple $EST when the last $EST has
MAXEVAL=0.
</li>
</ul>
</li>

<li>
vpc/npc
<ul>
<li>
Copy MSFO-file to run directory if option -msfo is used.
</li>
</ul>
</li>

<li>
scm
<ul>
<li>
Fixed handling of user-defined parameterizations that span multiple lines.
</li>
<li>
Fixed handling of user-defined parameterizations that involve more than one
THETA.
</li>
<li>
Provided that perl package Statistics::Distributions is installed: 
Model selection in each step is based on p-value instead of as before 
a combination of p-value and ofv which could give errors when comparing 
models with different numbers of extra parameters.
</li>
<li>
Changed the exponential parameterization in case of missing covariate values
so that the missing value number could never be exponentiated.
</li>
<li>
Fixed bug where the two thetas of the hockey-stick parameterization
would be given the same coordinate string label, causing errors when
updating initial estimates later. 
</li>
<li>
Covariates added to code in new way to handle all cases of TVparameter
code, including<br>
TVparameter = THETA(1)<br>
IF (X.GT.1) TVparameter = THETA(2)<br>
which before was handled incorrectly.
</li>
<li>
Fixed bug in multiple-line definitions of parameterizations in [code] 
section.
</li>
<li>
Fixed bug in passing on common PsN options to all parts of the program.
Before parallelization options etc were not passed on.
</li>
</ul>
</li>
<li>
pind and nonpb
<ul>
<li>
Update initial estimates before fixing parameters, update_inits function
will not change fixed values.
</li>
</ul>
</li>
</ul>

<h3>New features in PsN 3.2.4</h3>
<ul>
<li>  <a href='pdfdocs/bootstrap_userguide.pdf' target="_blank">bootstrap</a>
<ul>
<li>Save disk space by not copying bootstrapped datasets to the NM_run directories, use relative path instead.
</li>
</ul>
</li>
<li> <a href='pdfdocs/npc_vpc_userguide.pdf' target="_blank">vpc/npc</a>
<ul>
<li>
Save diskspace by always removing copied table output from NM_run directories.
</li>
<li>
When previously produced data is reanalyzed (option -directory) with new stratification/binning options, old output vpctab and vpc_results.csv/npc_results.csv will be renamed to avoid loss of results.
</li>
<li>
When reanalyzing previously produced data, the MAXEVAL=0 run (on original data) is only rerun if stratifying on a new variable that is not already in the table output, or if using an idv not already in the table output.
</li>
<li>
Added possibility to run the simulations of a vpc or npc in parallel. Use option -n_simulation_models=N which will make PsN split the simulations over N modelfiles. 
</li>
<li>
Complete redesign of prediction and variability correction (options -predcorr and -varcorr) functionality. These options are now stable.
</li>
<li>
New option -lnDV for transformation of data (mostly for -predcorr/-varcorr).
</li>
<li>
new format for additional features information in vpc_results.csv, and more robust connection with <a href='http://xpose.sourceforge.net' target="_blank">Xpose</a>. Will only run with Xpose-4.2.1 or later.
</li>
<li>
changed vpctab numbering to append anything between 'run' and '.mod' to vpctab
</li>
</ul>
</li>
<li>  <a href='pdfdocs/sse_userguide.pdf' target="_blank">sse</a>
<ul>
<li>New option -add_models. It is possible to reuse the previously simulated datasets when wanting to try additional alternative models. Old output sse_results.csv and raw_results will not be overwritten, the new output will be numbered add1, add2 etc.
</li>
<li>
Better error handling in sse. Even if some samples do not finish for some models, statistics will still be computed on the finished samples, using a smaller N. All runs which produce a non-zero OFV will be used in the statistics.
</li>
<li>
It is possible to have alternative models with more than one $PROBLEM (for simulation from the estimated model).  
</li>
<li>
Option clean is set to 3 as default to save disk space. For unstable models it is recommended to set clean=1 to make it possible to restart runs. Option -add_models works even if -clean=3.
</li>
<li>
sse renumbers filename in MSFO=filename so that it has same numbering as modelfile (1,2,3 etc for simulation model, 1-1,1-2,...,2-1,2-2... for alternative models). Makes it possible to output MSFO in first $PROBLEM and use $MSFI in second $PROBLEM of alternative models. 
</li>
</ul>
</li>
<li>  <a href='pdfdocs/scm_userguide.pdf' target="_blank">scm</a>
<ul>
<li> A new userguide. 
</li>
<li>
More input checking.
</li>
<li>
Added option to either consider or not consider models without minmization successful when selecting the best one in the step, option -only_successful. 
</li>
<li>
New and experimental feature -linearize, for accelerated covariate search. Work in progress. Akash Khandelwal will give a talk on this topic at PAGE 2010.
</li>
<li>
Added scm option -sum_covariates. The default is to multiply covariates when computing typical value of a parameter. With option -sum_covariates the covariate function values are summed instead. Relevant with logit-transformed models. Using this option requires the user to set a [code] section in the scm configuration file. Examples are found in the userguide. 
</li>
</ul>
</li>
<li>psn_clean (use psn_clean -h for help)
<ul>
<li>A new script for removing obsolete files in PsN run directories after a successful run.
</li>
</ul>
</li>
<li>  <a href='pdfdocs/runrecord_userguide.pdf' target="_blank">RunRecord</a>
<ul>
<li>A new tool. RunRecord is a PsN based script that facilitates the creation of run records for NONMEM runs. It takes a range of run numbers as input, extracts information and generates a txt file that can be imported into Excel for further manipulation. The information extracted includes parameter estimates, standard errors, condition number, objective function value (OFV), parameter names and information about model components. Some of this comes from the NONMEM output files while some parts are extracted from user-supplied information in the NONMEM model file. <br><br>
An additional way to use the runrecord script is to create a ¡Èraw_results¡É-like file from a set of lst-files, for example to recover results from a bootstrap that did not finish.
</li>
</ul>
</li>

<li>update_inits (use update_inits -h for help)
<ul>
<li>The -renumber='new number' option will now also change MSFO='anything''some number' to MSFO='anything''new number'
</li>
<li>
option -add_tags to add empty tags for RunRecord, and option -based_on to set runrecord tag ¡ÈBased on.¡É
</li>
</ul>
</li>


<li>  <a href='pdfdocs/psn_configuration.pdf' target="_blank">Configuration</a> of PsN
<ul>
<li>New possibility to run perl code "pre_compile_command" before PsN's fortran compilation. Can e.g. be used to modify environment variables for a specific compiler. 
</li>
</ul>
</li>

<li> General
<ul>
<li>
New option -sge_prepend_flags for setting extra flags for qsub when using option run_on_sge (sometimes e.g. need -V). 
</li>
<li>
Added option -run_on_mosix, will have effect on Unix systems only. If used, PsN will start the perl processes running NONMEM with 'mosenv -e perl...' instead of with the default 'perl...'. (As before, the name of the perl binary is configurable in psn.conf)
</li>
<li>
New option -nmqual, invoke nonmem using nmqual generated perl script (similar to option -nmfe). See <a href='pdfdocs/psn_configuration.pdf' target="_blank">psn_configuration.pdf</a>.
</li>
<li>
Changed handling of final parameter estimates. Unlike in 3.1.0, off-diagonal OMEGA/SIGMA zeroes will now be stored by the output parser. Removal of uninteresting zeros is done by the scripts. Zeroes removal is however not implemented yet in sumo.
</li>
</ul>
</li>
</ul>
<h3>Bug fixes in PsN 3.2.4</h3>
<ul>
<li>General 
<ul>
Fixed bug where very small but non-zero final parameter estimates were rounded off to 0.000000 and put as initial estimates in model. Most commonly encountered in scm. Now for NM6, for THETAS and diagonal OMEGA/SIGMA values x, where 0 < x < 0.000001, x is replaced with 0.000001 (smallest positive non-zero number that NM6 will accept) and values x, where -0.00001 < x < 0, are replaced with -0.00001 (largest negative non-zero number NM6 will accept). Off-diagonal OMEGA/SIGMA is rounded off to 0 as before if very small absolute value. For NM7, values are given in E notation if necessary, and the same precision as the default in raw output is used.
</li>
<li>
PsN will skip $WARNINGS record instead of crashing if it is encountered 
</li>
<li>
Fixed some over-active error checking (string matching of estimation method names in raw and additional output, missing method name when only simulating)
</li>
<li>
Changed verbatim fortran code for option -extra_data_file to work with NM7
</li>
<li>
Fixed errors in -run_on_zink option 
</li>
<li>
Fixed bug in reading "FIRST in $PK/$PRED/$ERROR, made it optional to have space between " and FIRST
</li>
<li>
Handle parsing of list file when there is no row OMEGA HAS BLOCK STRUCTURE when omega is a sequence of size 1 blocks.
</li>
<li>
If the nonmem version number in the lst-file does not match the version specified in psn.conf for the -nm_version used, then psn will print a warning and then continue parsing, instead of stopping as in earlier versions.
</li>
<li>
When shrinkage cannot be computed because omegas are missing, print error message and continue execution instead of writing error message and stopping.
</li>
<li>
Look for executable nmfe script (instead of just existing file) when option -nmfe is used.
</li>
<li>
minor torque bug fixed, make sure jobnames do not start with number/contain spaces. 
</li>
<li>
PsN will accept multiple $PROB in modelfiles when running NM7
</li>
<li>
Better mathematical exception handling (avoid  division by small number) in compute_comegas_or_csigmas function in output parser.
</li>
<li>
fixed bug regarding missing parameter estimates in raw_results file for second $PROBLEMs that use $MSFI record
</li>
<li>
Fixed bug in output parsing for NONMEM6 and models with only $THETAs (no omega or sigma). Before fix PsN would not detect the end of the final parameter estimate section and interpret standard errors as additionl final estimates of THETA.
</li>
</ul>
</li>

<li>
bootstrap
<ul>
<li>
Fixed -bca option, run would crash for certain types of output.
</li>
<li>
Pass on parallelization options to jackknife</li>
</ul>
</li>

<li>
sse
<ul>
<li>
If the number of finished samples is 1 then stdev, skewness and kurtosis are not computed, otherwise would give division by 0.
</li>
<li>
-shrinkage may be requested with sse. The shrinkage values will not be reported in sse_results.csv, but will be written to raw_results.csv
</li>
<li>
fixed bug so that $NONPARAMETRIC is removed for simulation model with ONLYSIM
</li>
<li>
Fixed bug in sse where lack of header in original dataset and no IGNORE=@ in $DATA caused a crash.
</li>
</ul>
</li>

<li>
vpc/npc
<ul>
<li>
Fixed bug so that ISAMPLE settings in $EST METH=IMP are kept when running vpc. Also set ISAMPLE=1 when generating new IMP step for run with skipped estimation when ofv is not needed.
</li>
</ul>
</li>

<li>
scm
<ul>
<li>
Fixed bug in covariate function theta boundary calculation for cases when median of covariate equals max or min of covariate (would give division by 0).
</li>
</ul>
</li>
<li>
cdd
<ul>
<li>
Handling of failed covariance step.
</li>
</ul>
</li>
</ul>
<h3>Documentation updates in PsN 3.2.4</h3>
<ul>
<li>The new document <a href='pdfdocs/scm_userguide.pdf' target="_blank">scm_userguide.pdf</a> describes the scm tool and gives examples on how to set the many input options. For the new RunRecord script there is a <a href='pdfdocs/runrecord_userguide.pdf' target="_blank">userguide</a> and <a href='pdfdocs/runrecord_template.xls' target="_blank">Excel template</a>. The <a href='pdfdocs/npc_vpc_userguide.pdf' target="_blank">vpc/npc userguide</a> has a "Getting started" section for new users. There are additions in <a href='pdfdocs/known_bugs_and_workarounds.pdf' target="_blank">known_bugs_and_workarounds.pdf</a>. </li>
</ul>
<h3>New features in PsN 3.1.0</h3>
<ol>
<li>Extensive NONMEM7 support. All PsN tools accept modelfiles with multiple 
  $ESTIMATION. All estimation methods are supported. Note:
  only results from last $EST will be presented and used in data processing.
</li>
<li>New parser for THETA, OMEGA and SIGMA. With the new parser all known bugs
  are removed. All formatting variants described in the NONMEM userguide
  are supported. 
</li>
<li>tweak_inits will ensure that OMEGA and SIGMA are strictly diagonally dominant.
  This reduces the risk of NONMEM halting due to non-postive definite initial
  estimate matrices.  Also, tweak_inits will not change initial estimates 
  that are zero, regardless if they are FIXED or not. This preserves band matrices.
</li>
<li>New options -predcorr (prediction correction) and -varcorr
  (variability correction, experimental) to vpc. More details in the vpc <a href='pdfdocs/npc_vpc_userguide.pdf' target="_blank">userguide</a>.
</li>
<li>New options -last_est_complete and -niter_eonly for tools that run some models with 
  skipped estimation step (cdd, npc, vpc, execute with mirror_plots option). 
  More details in <a href='pdfdocs/PsN_and_NONMEM7.pdf' target="_blank">PsN_and_NONMEM7.pdf</a>.
</li>
<li>update_inits now has options for renumbering output files in $TABLE and adding
  comments in $PROBLEM
</li>
<li>Both run information files version_and_option_info.txt and command.txt will be 
  overwritten at each rerun in the same PsN directory, also for npc and vpc. The
  files from the original PsN call are saved as original_command.txt and
  original_version_and_option_info.txt 
</li>
<li>The logic for the flag minimization_successful has been adapted to new the estimation 
  methods. Affects retries and sumo output. More details in <a href='pdfdocs/PsN_and_NONMEM7.pdf' target="_blank">PsN_and_NONMEM7.pdf</a>.
</li>
<li>For developers only: Many accessors to the output object have changed. 
  Contact the PsN development team for more information.
</li>
</ol>
<h3>Minor changes and bug fixes in PsN 3.1.0</h3>
<ol>
<li>When option -nmfe is used, PsN will first look for nmfe script in the 'run' 
  and then in the 'util' subdirectory of the NONMEM installation directory. 
  In PsN-3.0.0 the order was 'util' then 'run'.</li>

<li>shrinkage: PsN will report percentages in raw_results, instead of fraction.
  Eta shrinkage will be taken from the lst-file when available.</li>

<li>sse tool will keep IGNORE=list statements (IGNORE=single character will still 
  be changed to IGNORE=@). ACCEPT statements are kept as before.</li>

<li>Headers in raw_results will look different, using full indexes for SIGMA and 
  OMEGA, e.g. SIGMA(2,1), OMEGA(3,3).</li>

<li>sumo and update_inits can now handle a model that lacks OMEGAs.</li>

<li>three unstable functionalities disabled for NM7: summarize, handle_maxevals,
  handle_msfo.</li>

<li>PsN now accepts both comma- and space-separated $INPUT.</li>
</ol>

<h3>New documentation in PsN 3.1.0</h3>
<ol>
<li>The document <a href='pdfdocs/PsN_and_NONMEM7.pdf' target="_blank">PsN_and_NONMEM7.pdf</a> describes how PsN deals with the
  many new features and functions of NONMEM7. This document is essential for
  anyone who intends to use PsN with NONMEM7.</li>
</ol>
<h3>New features in PsN 3.0.0</h3>
<ol>
<li> NONMEM7 support for NONMEM6 type input<br>
PsN 3.0.0 has partial support for NONMEM7. It will handle NONMEM6 type input, i.e. single $ESTIMATION and classical estimation methods, with any PsN tool (bootstrap, scm...). PsN 3.0.0 has full support for NONMEM6. Details in <a href="http://downloads.sourceforge.net/project/psn/release_notes_PsN_3_0_0.txt">release notes</a>. 
</li>
<li> New option nmfe:<br>
There is a new commandline option -nmfe (no arguments) that can be used
with any PsN tool. When enabled, PsN will invoke nmfe6 or nmfe7 to run NONMEM
instead of doing stepwise compiling and execution. See <a href='pdfdocs/psn_configuration.pdf' target="_blank">psn_configuration.pdf</a> and <a href='pdfdocs/common_options_defaults_versions_psn.pdf' target="_blank">common_options_defaults_version.pdf</a> for details.
When nmfe is set the compiler configuration is ignored.
</li>
<li>
Compiler configuration
The compiler configuration can be set differently for each NONMEM version.
See <a href='pdfdocs/psn_configuration.pdf' target="_blank">psn_configuration.pdf</a> for details.
</li>
</ol>
<h3>Minor changes and bug fixes in PsN 3.0.0</h3>
<ol>
<li> 
Fixed the -clean option and documentation so that the behaviour matches
 the documentation.</li>

<li>Fixed a bug that caused bootstrap to crash when dataset has no header.</li>

<li>The psn.mod file in NM_run subdirectory is now the selected model from the
set of retries, instead of the last model that was run.</li>

<li>Fixed a bug to -iofv option for NONMEM6 on Windows. (-iofv is not supported
for NONMEM7 since iofv values are obtained from phi additional output.)</li>

<li>Removed old experimental database code.</li>

<li>Added a temporary solution to file path problem on windows and
old perl5.8 build versions. Thanks to Joachim Grevel.</li>

<li>Fixed a bug to rewriting model compartments (line break at wrong place).</li>

<li>Added handling of commas in comments to parameters using a solution
provided by Bill Denney and Jeroen Elassaiss-Schaap.</li>

<li>Changed sge queueing of jobs. Awaiting test result from Justin Wilkins
who reported the bug.</li>

<li>Changed npc/vpc to use CWRES in $TABLE if NONMEM7 is used , and to allow
longer variable names.</li>

<li>Changed installer script setup.pl so that it no longer installs required
perl modules, but instead optionally checks if required modules are installed.</li>

<li>Changed installer script so that documentation is copied to doc
subdirectory of PsN installation directory.</li>

<li>Updated error messages to give more information.</li>
</ol>

<h3>New documentation in PsN 3.0.0</h3>
<ol>
<li>Installation instructions are found in psn_installation.pdf.</li>

<li>In <a href='pdfdocs/psn_configuration.pdf' target="_blank">psn_configuration.pdf</a> it is described how to
edit psn.conf to configure PsN correctly. Please read that
document before running PsN for the first time.
Old psn.conf files must be edited to work correctly with PsN3.</li>

<li>The document <a href='pdfdocs/known_bugs_and_workarounds.pdf' target="_blank">known_bugs_and_workarounds.pdf</a> lists the most commonly
encountered unfixed bugs and how to work around them.</li>
</ol>

<h3>Changes between PsN 2.3.1 and 2.3.2</h3>
  <ol>
    <li>
  <p>Added support for censored and categorical data in vpc.</br>
  Please see the updated <a href="pdfdocs/npc_vpc_userguide.pdf" target="_blank">userguide</a> for details.
    </li>
    <li>
    <p>Added some simple error checking in $OMEGA and $SIGMA parsing.</p>
    </li>
    <li>
    <p>Enabled negation of some common options (e.g. -compress).</p>
    </li>
    <li>
  <p>Changed the default value of option -restarts from 5 to 0 <br>in the source code.
    Please note: Users can set their own <br>defaults in psn.conf, see 
<a href="pdfdocs/common_options_defaults_versions_psn.pdf" target="_blank">instructions here.</a></p>
    </li>
    <li>
  <p>Fixed a bug to the sse that caused options not to be passed correctly.</p>
    </li>
    <li>
  <p>Fixed a bug to sumo that prevented the user from setting some options.</p>
    </li>
    <li>
  <p>Fixed a bug in the code for  running on Torque (the torgue bug).</p>
    </li>
    <li>
  <p>Replaced the computational principles of nonpb version 1.</p>
    </li>
    <li>
  <p>Fixed a bug in the input checking of vpc/npc.</p>
    </li>
    </ol>
  <h3>Changes between PsN 2.3.0 and 2.3.1</h3>
  <ol>
    <li>
      Updated the <a href="pdfdocs/sse_userguide.pdf" target="_blank">sse_userguide</a>.
    </li>
    <li>
  <p>Added support for models with block SIGMA structure in NONMEM VI<br>
    2.0.</p>
    </li>
    <li>
  <p>Fixed a bug to the bootstrap that caused options not to be passed<br>
    correctly</p>
    </li>
    <li>
      <p>Added support for the Torque grid computing system.</p>
    </li>
    <li>
      <p>Fixed a bug so that the -mplots flag works with bootstrap and llp.</p>
    </li>
    </ol>
  <h3>Changes between PsN 2.2.5 and 2.3.0</h3>

<ol>
  <li>
    <p> Support for NONMEM VI 2.0 has been added.</p>
  </li>
  <li>
    <p>Major documentation effort. All command-line help texts have been<br>
      reviewed and updated.</p>
  </li>
  <li>
    <p>User-guide documents for <a href="pdfdocs/common_options_defaults_versions_psn.pdf" target="_blank">common PsN options</a>, <a href="pdfdocs/bootstrap_userguide.pdf" target="_blank">bootstrap</a>,<a href="pdfdocs/execute_userguide.pdf" target="_blank"> execute</a>,<br>
      <a href="pdfdocs/sse_userguide.pdf" target="_blank">sse</a>, <a href="pdfdocs/npc_vpc_userguide.pdf" target="_blank">vpc, npc</a>,<a href="pdfdocs/llp_userguide.pdf" target="_blank"> llp</a> and <a href="pdfdocs/cdd_userguide.pdf" target="_blank">cdd</a> are now available in the subfolder &quot;doc&quot;<br>
      under the PsN Core and Toolkit installation directory, which is<br>
      chosen during the installation (e.g. C:\Perl\site\lib\doc).  There<br>
      are still gaps, but they are smaller than before.</p>
  </li>
  <li>
    <p>New feature to help make runs more reproducible: A new file,<br>
      &quot;version_and_option_info.txt&quot; is created in the run directory<br>
      (&lt;scriptname&gt;_dirXX) whenever execute/sse/bootstrap/npc or any of<br>
      the other major PsN scripts are run. The file contains the PsN<br>
      version, the date and time, the actual values of the optional<br>
      options to the script, the command that started the run and the<br>
      actual values of optional general PsN options. The general PsN<br>
      options include nm_version (NONMEM version as named in psn.conf) and<br>
      seed.</p>
  </li>
  <li>
    <p>Changed principle for selecting best result from a set of retries.<br>
      Please see the <a href="https://sourceforge.net/project/shownotes.php?release_id=635567&group_id=101419">change log</a> or the README.txt file for details. </p>
  </li>
  <li>    
    <p>New script called <strong>&quot;<a href="pdfdocs/common_options_defaults_versions_psn.pdf">psn</a></strong>&quot; that lists all PsN scripts available using &quot;psn -h&quot; or &quot;psn -help&quot;, and lists all available NONMEM versions (the ones listed in psn.conf) using &quot;psn -nm_versions&quot;</p>
  </li>
  <li>
    <p>New script called <strong>&quot;<a href="pdfdocs/common_options_defaults_versions_psn.pdf">psn_options</a>&quot;</strong> that lists options common to most scripts (&quot;psn_options -h&quot;).<br>
      </p>
  </li>
  <li>
    <p>New version of sumo, including help texts. see &quot;sumo -help&quot;</p>
  </li>
  <li> Lots of new additions and changes, please see the <a href="https://sourceforge.net/project/shownotes.php?release_id=635567&group_id=101419">change log</a> or the README.txt file for details.<br>
  </li>
  </ol>
</div>
<h3>More information</h3>
<p>
Perl-speaks-NONMEM is copyright <br>
&copy;2013-2015 by Mats Karlsson, Andrew Hooker, Rikard Nordgren and Kajsa Harling.<br>
&copy;2008-2012 by Mats Karlsson, Niclas Jonsson, Andrew Hooker and Kajsa Harling.<br>
&copy;2006-2007 by Lars Lindbom.<br>
&copy;2000-2005 by Lars Lindbom and Niclas Jonsson.<br>
All rights reserved.</p>
<p>Perl-speaks-NONMEM is licensed under <a href="http://www.gnu.org/licenses/old-licenses/gpl-2.0.html">version 2 of the GNU General
Public License</a> as published by the Free Software Foundation.</p>
<p>
PsN is developed and maintained by <a href="mailto:kajsa.harling@farmbio.uu.se">Kajsa Harling</a> and <a href="mailto:rikard.nordgren@farmbio.uu.se">Rikard Nordgren</a>
.</p>

<p>PsN was originally developed by Niclas Jonsson and continued by Lars 
Lindbom for his doctoral thesis. Additional implementation has been done by Pontus Pihlgren, Jakob Ribbing, Kristin Karlsson, 
Maria Kjellsson and Joakim Nyberg. Additional contributions by Radojka Savic, Paul Baverel, Martin Bergstrand, Elodie Plan, Yasunori Aoki and many more.</p>

<h4>References</h4>
<ul>
    <li class="reference"> <a href="http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=pubmed&cmd=Retrieve&dopt=AbstractPlus&list_uids=
16023764&query_hl=1&itool=pubmed_docsum">Lindbom L, Pihlgren P, Jonsson EN.</a> <br>
                PsN-Toolkit--a collection of computer intensive statistical methods for non-linear mixed effect modeling using NON
MEM. <br>
                Comput Methods Programs Biomed. 2005 Sep;79(3):241-57. </li>
    <li class="reference"> <a href="http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=pubmed&cmd=Retrieve&dopt=AbstractPlus&list_uids=
15212851&query_hl=1&itool=pubmed_docsum">Lindbom L, Ribbing J, Jonsson EN. </a> <br>
                Perl-speaks-NONMEM (PsN)--a Perl module for NONMEM related programming.<br>
        Comput Methods Programs Biomed. 2004 Aug;75(2):85-94.</li>
</ul>

<h4>Acknowledgments</h4>
<p>
PsN was developed in parts with funding from the <a href="http://www.ddmore.eu">DDMoRe</a> and <a href="http://www.ideal.rwth-aachen.de">IDeAl</a> projects. 
NONMEM&reg; is a registered trademark of ICON plc. The Perl camel
logo is a registered trademark of O'Reilly Media, Inc. and is used with
permission. All logos and trademarks in this site are property of
their respective owners.
</p>
