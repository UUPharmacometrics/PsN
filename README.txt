************************
Installation
************************

- Depending on your Perl distribution you may have to install the following
perl packages:

Math::Random
Storable::Store (Unix only)

- Recommended perl packages, not absolutely required when running PsN 
but necessary for some features:

Statistics::Distributions
Archive::Zip
File::Copy::Recursive

All available from CPAN ( www.cpan.org )

- Before installing PsN, verify that all NONMEM installations you intend to
use can be run directly via the nmfe (or NMQual) scripts and that they 
produce complete output files. 

Please note that with NMQual8 the nm72.xml file must be slightly modified 
(to expect .mod instead of .ctl as the control stream suffix) before NONMEM 
installation, see document psn_configuration.pdf, otherwise PsN's NMQual8 
support will not work.

This version of PsN has not been tested with NMQual, but is believed to work
with NMQual version 8.2.4 and on. However the PsN reference output that comes
with NMQual may be out of date.

If you plan to use the parallelization features of NONMEM7.2 you must first
verify that you can run NONMEM in parallel directly with nmfe72.

If running NONMEM with nmfe (or NMQual) does not work then PsN will not work. 

- Unpack the installation package downloaded from psn.sf.net. It will 
create a directory called PsN-Source. Run the installation script 
setup.pl from within PsN-Source.

- More detailed installation instructions are found 
on the PsN website http://psn.sourceforge.net/ under
Installation. 

- The installation script can automatically generate a minimal configuration 
file psn.conf, both on Windows and Unix-type systems. If PsN is to be run on 
a cluster and/or with NMQual then some manual editing of psn.conf is still 
needed, see psn_configuration.pdf

************************
Documentation
************************
All documentation is found in the doc subdirectory of the PsN 
installation directory. During installation it is also possible 
to have all documentation copied to a user-specified directory.

All documentation can also be downloaded from
the PsN website http://psn.sourceforge.net/


************************
Dependencies 
************************

- Xpose must, if used, be version 4.2.1 or later to handle PsN-3.5.3 or later 
  output format. It is recommended to use the latest Xpose release
  from http://xpose.sf.net

************************
Known issues 
************************

- Installation problem on some Windows systems where 
perl 'system' command does not work. Make sure perl module File::Copy::Recursive
is installed *before* (re)running the installation script setup.pl

- On Windows 7 with ActiveState Perl 5.16.3 the installation script failed
to detect a correctly installed Math::Random module. If you think you have 
Math::Random installed even though the installation script complains
then just go ahead with the installation and test running execute with 
option -min_retries=1 . If it works then you do have Math::Random and all 
is well.



