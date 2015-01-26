package standardised_output::so_block;

# Populating an SOBlock from an lst file

use strict;
use warnings;
use Moose;
use MooseX::Params::Validate;
use include_modules;
use XML::LibXML;
use output;
use data;
use array;
use IO::File;
use math;
use utils::file;

