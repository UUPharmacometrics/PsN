
<h2>Installation instructions
  </h2>
<p>
Before you install PsN, you should make sure that the following programs/modules are installed on your computer. 

<ul>
<li>NONMEM. <p>
Please note that with NMQual8 the nm72.xml file must be slightly modified (to expect .mod instead of .ctl as the control stream suffix) before NONMEM installation, see document psn_configuration.pdf, otherwise PsN's NMQual8 support will not work.<p>
You must verify that all NONMEM installations you intend to
use can be run directly via the nmfe/NMQual scripts and that they 
produce complete output files. If you plan to use the parallelization features 
of NONMEM7.2 you must also verify that you can run NONMEM in parallel directly 
with nmfe72. <p>
If running NONMEM with nmfe/NMQual does not work then PsN will not work.
</li>
<li>Perl5. <br>
Note 1: For Windows we recommend 
<a target="_blank" href="http://www.activestate.com/activeperl/downloads/">ActiveState Perl</a>, 
not Strawberry Perl. Windows users who are already running an older version of PsN do not need to 
upgrade Perl, but Windows users who are installing PsN for the first time
need to choose a sub-version of ActiveState Perl
in which the extra modules needed for PsN are
available. On Dec 4th 2013 the recommended Perl version was 5.16. For updated information 
on which version provides which modules see 
package information pages for 
<a target="_blank" href="http://code.activestate.com/ppm/Math-Random/">Math::Random</a>, 
<a target="_blank" href="http://code.activestate.com/ppm/File-Copy-Recursive/">File::Copy::Recursive</a>, 
<a target="_blank" href="http://code.activestate.com/ppm/Archive-Zip/">Archive::Zip</a> 
and
<a target="_blank" href="http://code.activestate.com/ppm/Statistics-Distributions/">Statistics::Distributions</a> </br>
Note 2: Mac users should use the Perl version that is in MacOS by default, not ActiveState.</li>
<li>Perl modules Math::Random, Statistics::Distributions, Archive::Zip and File::Copy::Recursive. 
PsN can be run without Statistics::Distributions, Archive::Zip and File::Copy::Recursive 
but some scm/vpc features and a scm bugfix will not be available.</li>
<li>Perl module Storable (needed for Unix only, installation as Math::Random)</li>
</ul>
</p>

<h3>Installing Math::Random, Statistics::Distributions, Archive::Zip, File::Copy::Recursive</h3>
You must have internet access for the installation procedure to work.
<h4>Windows (ActivePerl)</h4>
<ol>
<li>Start a DOS command window.</li>
<li>Run the following commands (hit Enter after each command):<br>
ppm install math-random<br>
ppm install statistics-distributions<br>
ppm install archive-zip<br>
ppm install file-copy-recursive<br>
</li>
</ol>
<h4>Linux/Unix/MacOS</h4>
You might need to answer some questions interactively when using the cpan -i command.
<ol>
<li>Start a command window.</li>
<li>Run the following commands (hit Enter after each command), prepend sudo if necessary:<br>
cpan -i Math::Random<br>
cpan -i Statistics::Distributions<br>
cpan -i Archive::Zip<br>
cpan -i File::Copy::Recursive<br>
cpan -i Storable<br>
</li>
</ol>

<h3>PsN installation</h3> 
<ol>
  <li>
    <p>Unpack the file you downloaded from psn.sf.net. It will create a directory
       called <tt>PsN-Source.</tt></p>
  </li>
  <li>
    <div style="text-align: justify;">

    <p align="justify">Run the installation script from within <tt>PsN-Source</tt>.
       If you are running windows and have ActiveState Perl
      installed you should be able to double-click on
      <tt>setup.pl</tt>. Otherwise open a command line window (Windows Start-&gt;Run, type 'cmd'), go to
      the <tt>PsN-Source </tt>directory and type: </p> 
    </div>
    <p><tt>perl setup.pl</tt></p>
    <p>Unix users should open their favorite terminal go to the <tt>PsN-Source</tt> directory and type: </p>
    <p><tt>perl setup.pl</tt></p>
  </li>
  <li>
    Read all messages carefully and answer the questions on screen. The default is probably the
       best for most users. It is recommended to let the installation script automatically create psn.conf.
  </li>
  <li> If you did not let the installation script create a configuration file <tt>psn.conf</tt> for you, 
or if you are running NONMEM via NMQual or on a cluster, you need to edit <tt>psn.conf</tt> 
in the PsN installation directory. 
The document <a href='pdfdocs/psn_configuration.pdf'>psn_configuration.pdf</a> in 
PsN-Source/doc describes how to make correct settings in the psn.conf file. 
The document is also found on <a href="http://psn.sourceforge.net/docs.php" target="_blank">the PsN website</a> under Documentation.
  </li>
  <li>When the installation is
    done you can safely remove the <tt>PsN-Source</tt>
    directory.
</li>
</ol>
<br>

