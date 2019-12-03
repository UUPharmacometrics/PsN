************************
Perl-speaks-NONMEM
************************

Perl-speaks-NONMEM (PsN) is a collection of Perl modules and programs aiding 
in the development of non-linear mixed effect models using NONMEM. The 
functionality ranges from solutions to simpler tasks such as parameter 
estimate extraction from output files, data file sub setting and resampling, 
to advanced computer-intensive statistical methods. PsN includes stand-alone 
tools for the end-user as well as development libraries for method developers.

Please find more information on the webpage: https://uupharmacometrics.github.io/PsN 

************************
Installation
************************

- PsN4 requires at least Perl version 5.10.1 (please note that users of
the free version of ActiveState Perl probably need an even higher version)

- Depending on your Perl distribution you may have to install the following
perl packages which are required for PsN4:

Math::Random
Moose
MooseX::Params::Validate
YAML::XS

- Recommended perl packages, not absolutely required when running PsN 
but necessary for some features:

Statistics::Distributions
Archive::Zip
File::Copy::Recursive

All available from CPAN ( www.cpan.org )

- Before installing PsN, verify that all NONMEM installations you intend to
use can be run directly via the nmfe (or NMQual) scripts and that they 
produce complete output files. If running NONMEM with nmfe (or NMQual) does 
not work then PsN will not work.

If you plan to use the parallelization features of NONMEM7.2 you must first
verify that you can run NONMEM in parallel directly with nmfe72.

This version of PsN supports NMQual8. Please read psn_configuration.pdf
regarding how to configure PsN to run with NMQual8.

- Unpack the installation package downloaded from psn.sf.net. It will 
create a directory called PsN-Source. Run the installation script 
setup.pl from within PsN-Source.

- More detailed installation instructions are found 
on the PsN website https://uupharmacometrics.github.io/PsN under
Installation. 

- The installation script can automatically generate a minimal configuration 
file psn.conf, both on Windows and Unix-type systems. If PsN is to be run on 
a cluster and/or with NMQual then some manual editing of psn.conf is still 
needed, see psn_configuration.pdf

************************
Documentation
************************

All documentation can be found at the PsN website:
https://uupharmacometrics.github.io/PsN

************************
Testing 
************************

To install and run the PsN test suite see the developers_guide.pdf 
on the homepage

************************
Licensing 
************************

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

Please find the GPL version 2 in the file called LICENSE

************************
Authors 
************************

Please see the file called AUTHORS

************************
Dependencies 
************************

- Xpose must, if used, be version 4.2.1 or later to handle PsN-3.5.3 or later 
  output format. It is recommended to use the latest Xpose release
  from http://xpose.sf.net

************************
Known issues 
************************

- Windows users need to be careful to choose 
  a Perl version where the extra modules needed for PsN are available. See the 
  instructions on https://uupharmacometrics.github.io/PsN/install.html

- Installation problem on some Windows systems where 
perl 'system' command does not work. Make sure perl module File::Copy::Recursive
is installed *before* (re)running the installation script setup.pl
