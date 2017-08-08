package pharmml;

use strict;
use warnings;
use File::Spec::Functions qw(devnull);
use PsN;
use nonmemrun;
use MooseX::Params::Validate;

sub is_pharmml
{
    my $filename = shift;

    open my $fh, '<', $filename;
    my $line = <$fh>;
    if ($line =~ /^\<\?xml/) {    # Magic xml descriptor.
        seek $fh, 0, 0;
        while ($line = <$fh>) {            # Check if file contains start of PharmML element
            if ($line =~ /\<PharmML/) {
                close $fh;
                return 1;
            }
        }
    }

    close $fh;
    return 0;
}

sub is_java_installed
{
    if (system('java -version >' . devnull . ' 2>&1') == 0) {
        return 1;
    } else {
        return 0;
    }
}

sub _get_classpath
{
    my $classpath = $PsN::config->{'_'}->{'converter_path'};

    return $classpath;
}

sub convert_file
{
    my $filename = shift;
    my $classpath = _get_classpath;

    my $rc = system("java -cp \"$classpath/*\" eu.ddmore.convertertoolbox.cli.Main -in $filename -out . -sn PharmML -sv 0.3.0 -tn NMTRAN -tv 7.2.0");

    return $rc;
}

sub check_converted_model
{
    my $filename = shift;
    my $ok;

    # Run nmtran to test converted file before using it with PsN

    my $ref = nonmemrun::setup_paths(nm_version => $PsN::nm_version, nmqual => 0);
    my $command = $ref->{'full_path_nmtran'} . "<$filename";

    system($command);
    unlink('FCON', 'FSIZES', 'FSTREAM', 'prsizes.f90', 'FSUBS', 'FSUBS2', 'FSUBS.f90');
    unlink('FSUBS_MU.F90', 'FLIB', 'LINK.LNK', 'FWARN', 'trash.tmp');
    if (not -e 'FREPORT') {
        $ok = 0;
    } else {
        $ok = 1
    }

    unlink('FDATA', 'FREPORT');
    return $ok;
}

sub create_minimal_pharmml
{
    # Create a minimal PharmML from a NONMEM model.
    # The intent is to use it in conjunction with an SO to
    # automatically extract types of variability parameters for example
    my %parm = validated_hash(\@_,
        model => { isa => 'model' },
        filename => { isa => 'Str' },
    );
    my $model = $parm{'model'};
    my $filename = $parm{'filename'};

    eval { require so::xml; };
    if ($@) {
        die "Unable to find libxml2\n";
    }

    open my $fh, '>', $filename;

    print $fh <<'END';
<?xml version="1.0" encoding="UTF-8"?>
<PharmML xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
    xmlns="http://www.pharmml.org/pharmml/0.8/PharmML"
    xsi:schemaLocation="http://www.pharmml.org/pharmml/0.8/PharmML"
    xmlns:math="http://www.pharmml.org/pharmml/0.8/Maths"
    xmlns:ct="http://www.pharmml.org/pharmml/0.8/CommonTypes"
    xmlns:ds="http://www.pharmml.org/pharmml/0.8/Dataset"
    xmlns:mdef="http://www.pharmml.org/pharmml/0.8/ModelDefinition"
    xmlns:mstep="http://www.pharmml.org/pharmml/0.8/ModellingSteps"
    xmlns:design="http://www.pharmml.org/pharmml/0.8/TrialDesign"
    writtenVersion="0.8.1">

    <ct:Name>Minimal model generated  nmoutput2so</ct:Name>
    <ModelDefinition xmlns="http://www.pharmml.org/pharmml/0.8/ModelDefinition">
        <VariabilityModel blkId="vm_err" type="residualError">
            <Level referenceLevel="false" symbId="DV"/>
        </VariabilityModel>
        <VariabilityModel blkId="vm_mdl" type="parameterVariability">
            <Level referenceLevel="true" symbId="ID"/>
        </VariabilityModel>
END

    print_parameter_model(file => $fh, model => $model);

    print $fh <<'END';
    </ModelDefinition>
</PharmML>
END

    close $fh;
}

sub print_parameter_model
{
    my %parm = validated_hash(\@_,
        file => { isa => 'Ref' },
        model => { isa => 'model' },
    );
    my $file = $parm{'file'};
    my $model = $parm{'model'};

    print $file <<'END';
        <ParameterModel blkId="pm">
END

    for my $record (@{$model->problems->[0]->thetas}, @{$model->problems->[0]->omegas}, @{$model->problems->[0]->sigmas}) {
        for my $option (@{$record->options}) {
            my $name = $option->label;
            if (not defined $name) {
                $name = $option->coordinate_string;
            }
            if (not so::xml::match_symbol_idtype($name)) {
                $name = so::xml::mangle_symbol_idtype($name);
            }

            print $file '            <PopulationParameter symbId="' . $name . '"/>' . "\n"; # FIXME if no label and symbol washing
        }
    }

print $file <<'END';
        </ParameterModel>
END
}

# Naming for ETAS
# Correlation between ETAs
# Filter out SAME and FIX?

1;
