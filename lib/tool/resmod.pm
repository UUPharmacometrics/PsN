package tool::resmod;

use strict;
use Moose;
use MooseX::Params::Validate;
use List::Util qw(max);
use include_modules;
use log;
use model;
use model::problem;
use tool::modelfit;
use output;
use nmtablefile;

extends 'tool';

has 'model' => ( is => 'rw', isa => 'model' );
has 'model_names' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'idv' => ( is => 'rw', isa => 'Str', default => 'TIME' );
has 'run_models' => ( is => 'rw', isa => 'ArrayRef[model]' );

# This array of hashes represent the different models to be tested. The 0th is the base model
our @residual_models =
(
	{
		name => 'base',
	    prob_arr => [
			'$PROBLEM CWRES base model',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ <dvidaccept>',
			'$PRED',
            'Y = THETA(1) + ETA(1) + ERR(1)',
			'$THETA .1',
			'$OMEGA 0.01',
			'$SIGMA 1',
			'$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC',
        ]
	}, {
		name => 'omega_on_epsilon',
	    prob_arr => [
			'$PROBLEM CWRES omega-on-epsilon',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ <dvidaccept>',
			'$PRED',
            'Y = THETA(1) + ETA(1) + ERR(1) * EXP(ETA(2))',
			'$THETA .1',
			'$OMEGA 0.01',
            '$OMEGA 0.01',
			'$SIGMA 1',
			'$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC',
        ]
    }, {
        name => 'AR1',
        prob_arr => [
			'$PROBLEM CWRES AR1',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ <dvidaccept>',
			'$PRED',
            '"FIRST',
            '" USE SIZES, ONLY: NO',
            '" USE NMPRD_REAL, ONLY: C=>CORRL2',
            '" REAL (KIND=DPSIZE) :: T(NO)',
            '" INTEGER (KIND=ISIZE) :: I,J,L',
            '"MAIN',
            '"C If new ind, initialize loop',
            '" IF (NEWIND.NE.2) THEN',
            '"  I=0',
            '"  L=1',
            '"  OID=ID',
            '" END IF',
            '"C Only if first in L2 set and if observation',
            '"C  IF (MDV.EQ.0) THEN',
            '"  I=I+1',
            '"  T(I)=TIME',
            '"  IF (OID.EQ.ID) L=I',
            '"',
            '"  DO J=1,I',
            '"      C(J,1)=EXP((-0.6931/THETA(2))*(TIME-T(J)))',
            '"  ENDDO',
            'Y = THETA(1) + ETA(1) + EPS(1)',
            '$THETA  -0.0345794',
            '$THETA  (0.001,1,10)',
            '$OMEGA  2.41E-006',
            '$SIGMA  0.864271',
            '$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC',
        ]
    }, {
		name => 'power_ipred',
        need_ipred => 1,
	    prob_arr => [
			'$PROBLEM CWRES power IPRED',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ <dvidaccept>',
			'$PRED',
            'Y = THETA(1) + ETA(1) + ERR(1)*(IPRED)**THETA(2)',
			'$THETA .1',
			'$THETA .1',
			'$OMEGA 0.01',
			'$SIGMA 1',
			'$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC',
        ]
	}, {
		name => 'laplace',
	    prob_arr => [
			'$PROBLEM CWRES laplace',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ <dvidaccept>',
			'$PRED',
            'Y = THETA(1) + ETA(1) + ERR(1)',
			'$THETA .1',
			'$OMEGA 0.01',
			'$SIGMA 1',
			'$ESTIMATION METHOD=1 INTER LAPLACE MAXEVALS=9990 PRINT=2 POSTHOC',
        ]
    }, {
        name => 'laplace_2ll_df100',
        prob_arr => [
			'$PROBLEM CWRES laplace 2LL DF=100',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ <dvidaccept>',
			'$PRED',
            'MYIPRED = THETA(1) + ETA(1)',
            'W = THETA(2)',
            'DF = THETA(3) ; degrees of freedom of Student distribution',
            'SIG1 = W ; scaling factor for standard deviation of RUV',
            'IWRES = (DV - MYIPRED) / SIG1',
            'PHI = (DF + 1) / 2 ; Nemesapproximation of gamma funtion(2007) for first factor of t-distrib(gamma((DF+1)/2))',
            'INN = PHI + 1 / (12 * PHI - 1 / (10 * PHI))',
            'GAMMA = SQRT(2 * 3.14159265 / PHI) * (INN / EXP(1)) ** PHI',
            'PHI2 = DF / 2 ; Nemesapproximation of gamma funtion(2007) for second factor of t-distrib(gamma(DF/2))',
            'INN2 = PHI2 + 1 / (12 * PHI2 - 1 / (10 * PHI2))',
            'GAMMA2 = SQRT(2*3.14159265/PHI2)*(INN2/EXP(1))**PHI2',
            'COEFF=GAMMA/(GAMMA2*SQRT(DF*3.14159265))/SIG1 ; coefficient of PDF of t-distribution',
            'BASE=1+IWRES*IWRES/DF ; base of PDF of t-distribution',
            'POW=-(DF+1)/2 ; power of PDF of t-distribution',
            'L=COEFF*BASE**POW ; PDF oft-distribution',
            'Y=-2*LOG(L)',
			'$THETA .1',
			'$THETA (0,1)',
			'$THETA 100 FIX',
			'$OMEGA 0.01',
			'$ESTIMATION METHOD=1 LAPLACE MAXEVALS=9990 PRINT=2 -2LL',
        ],
    }, {
        name => 'laplace_2ll_dfest',
        prob_arr => [
			'$PROBLEM CWRES laplace 2LL DF=est',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ <dvidaccept>',
			'$PRED',
            'MYIPRED = THETA(1) + ETA(1)',
            'W = THETA(2)',
            'DF = THETA(3) ; degrees of freedom of Student distribution',
            'SIG1 = W ; scaling factor for standard deviation of RUV',
            'IWRES = (DV - MYIPRED) / SIG1',
            'PHI = (DF + 1) / 2 ; Nemesapproximation of gamma funtion(2007) for first factor of t-distrib(gamma((DF+1)/2))',
            'INN = PHI + 1 / (12 * PHI - 1 / (10 * PHI))',
            'GAMMA = SQRT(2 * 3.14159265 / PHI) * (INN / EXP(1)) ** PHI',
            'PHI2 = DF / 2 ; Nemesapproximation of gamma funtion(2007) for second factor of t-distrib(gamma(DF/2))',
            'INN2 = PHI2 + 1 / (12 * PHI2 - 1 / (10 * PHI2))',
            'GAMMA2 = SQRT(2*3.14159265/PHI2)*(INN2/EXP(1))**PHI2',
            'COEFF=GAMMA/(GAMMA2*SQRT(DF*3.14159265))/SIG1 ; coefficient of PDF of t-distribution',
            'BASE=1+IWRES*IWRES/DF ; base of PDF of t-distribution',
            'POW=-(DF+1)/2 ; power of PDF of t-distribution',
            'L=COEFF*BASE**POW ; PDF oft-distribution',
            'Y=-2*LOG(L)',
			'$THETA .1',
			'$THETA (0,1)',
			'$THETA (3,10)',
			'$OMEGA 0.01',
			'$ESTIMATION METHOD=1 LAPLACE MAXEVALS=9990 PRINT=2 -2LL',
        ],
    },
);

our @phi_models =
(
	{
		name => 'base',
	    prob_arr => [
            '$PROBLEM PHI mod',
            '$INPUT <phiinputcolumns>',
            '$DATA <phiname> IGNORE=@',
            '$PRED',
            'BXPAR = THETA(2) ; Shape parameter',
            'PHI = EXP(ETA(1)) ; Exponential trans',
            'ETATR = (PHI ** BXPAR - 1) / BXPAR',
            'Y = THETA(1) + ETATR + ERR(1) * SQRT(<etc>)',
            '$THETA 0.00576107',
            '$THETA 0.1576107',
            '$OMEGA 0.0173428',
            '$SIGMA 1.37603',
            '$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC',
        ],
    }
    # , {
#        name => 'boxcox',
    #       prob_arr => [
#    $PROBLEM    MOXONIDINE PK,FINAL ESTIMATES,ALL DATA
#;;

#$INPUT      SUBJ ID ET1 ET2 DV ETC1 ETC21 ETC22 ETC31 ETC32 ETC33 OBJX
#$DATA      run1.phi IGNORE=@
#$PRED   
#BXPAR = THETA(2) ; Shape parameter
#PHI = EXP(ETA(1)) ; Exponential trans
#ETATR = (PHI**BXPAR-1)/BXPAR 

#     Y     = THETA(1) +ETATR +ERR(1)*SQRT(ETC33)
#$THETA  0.00576107 ; TV
#$THETA  0.1576107 ; TV_
#$OMEGA  0.0173428  ; IIV (CL-V)
#$SIGMA  1.37603
#$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC

    #       ],

);

sub BUILD
{
    my $self = shift;

	my $model = $self->models()->[0]; 
    $self->model($model);
}

sub modelfit_setup
{
	my $self = shift;

    # Find a table with ID, TIME, CWRES and extra_input (IPRED)
    my @columns = ( 'ID', $self->idv, 'CWRES' );
    my $cwres_table = $self->model->problems->[0]->find_table(columns => \@columns, get_object => 1);
    my $cwres_table_name = $self->model->problems->[0]->find_table(columns => \@columns);
    if (not defined $cwres_table) {
        die "Error original model has no table containing ID, IDV and CWRES\n";
    }

    # Do we have MDV or DVID?
    my $have_ipred = 0;
    my $have_dvid = 0;
    for my $option (@{$cwres_table->options}) {
        if ($option->name eq 'IPRED') {
            $have_ipred = 1;
            push @columns, 'IPRED';
        } elsif ($option->name eq 'DVID') {
            $have_dvid = 1;
            push @columns, 'DVID';
        }
    }

    my $unique_dvid;
    my $number_of_dvid = 1;
    if ($have_dvid) {
        my $table = nmtablefile->new(filename => "../$cwres_table_name"); 
        my $dvid_column = $table->tables->[0]->header->{'DVID'};
        $unique_dvid = array::unique($table->tables->[0]->columns->[$dvid_column]);
        $number_of_dvid = scalar(@$unique_dvid);
    }

    # Create $INPUT
    my $input_columns;
    my @found_columns;
    for my $option (@{$cwres_table->options}) {
        my $found = 0; 
        for (my $i = 0; $i < scalar(@columns); $i++) {
            if ($option->name eq $columns[$i] and not $found_columns[$i]) {
                $found_columns[$i] = 1;
                my $name = $option->name;
                $name = 'DV' if ($name eq 'CWRES');
                $input_columns .= $name;
                $found = 1;
                last;
            }
        }
        if (not $found) {
            $input_columns .= 'DROP';
        }
        last if ((grep { $_ } @found_columns) == scalar(@columns));
        $input_columns .= ' ';
    }

	my @models_to_run;
    for my $model_properties (@residual_models) {
        next if ($model_properties->{'need_ipred'} and not $have_ipred);

        my $accept = "";
        for (my $i = 0; $i < $number_of_dvid; $i++) {
            if ($have_dvid) {
                $accept = "ACCEPT=(DVID.EQN." . $unique_dvid->[$i] . ")";
            }

            my @prob_arr = @{$model_properties->{'prob_arr'}};
            for my $row (@prob_arr) {
                $row =~ s/<inputcolumns>/$input_columns/;
                $row =~ s/<cwrestablename>/$cwres_table_name/;
                $row =~ s/<dvidaccept>/$accept/;
            }
            my $sh_mod = model::shrinkage_module->new(
                nomegas => 1,
                directory => 'm1/',
                problem_number => 1
            );
            my $cwres_problem = model::problem->new(
                prob_arr => \@prob_arr,
                shrinkage_module => $sh_mod,
            );
            my $dvid_suffix = "";
            $dvid_suffix = "_DVID" . $unique_dvid->[$i] if ($have_dvid);
            my $cwres_model = model->new(directory => 'm1/', filename => $model_properties->{'name'} . "_cwres$dvid_suffix.mod", problems => [ $cwres_problem ]);
            $cwres_model->_write();
            push @{$self->model_names}, $model_properties->{'name'} . "_cwres$dvid_suffix";
            push @models_to_run, $cwres_model;
        }
    }

    $self->run_models(\@models_to_run);

    my $run_phi_modelling = 0;      # Skip phi-modelling for now
    if ($run_phi_modelling) {
        my $phiname = '../' . $self->model->filename();
        $phiname =~ s/\.mod$/.phi/;
        if (-e $phiname) {
            my $phitable = nmtablefile->new(filename => $phiname); 
            # Loop over ETAs
            for (my $i = 1; $i <= scalar(grep { /ETA/ } @{$phitable->tables->[0]->header_array}); $i++) {

                my @a;
                for my $colname (@{$phitable->tables->[0]->header_array}) {
                    my $newname = $colname;
                    $newname =~ s/[(,)]//g;
                    $newname =~ s/^ETA/ET/;
                    $newname =~ s/^ET$i/DV/;
                    push @a, $newname;
                }
                my $phiinputs = join(' ', @a);

                for my $model_properties (@phi_models) {

                    my @model_code;
                    for my $row (@{$model_properties->{'prob_arr'}}) {
                        my $newrow = $row;
                        $newrow =~ s/<phiinputcolumns>/$phiinputs/;
                        $newrow =~ s/<phiname>/$phiname/;
                        $newrow =~ s/<etc>/ETC$i$i/;
                        push @model_code, $newrow;
                    }
                    my $sh_mod = model::shrinkage_module->new(
                        nomegas => 1,
                        directory => 'm1/',
                        problem_number => 1);
                    my $phi_problem = model::problem->new(
                        prob_arr => \@model_code,
                        shrinkage_module => $sh_mod,
                    );
                    my $phi_model = model->new(directory => 'm1/', filename => $model_properties->{'name'} . "${i}_phi.mod", problems => [ $phi_problem ]);
                    $phi_model->_write();
                    push @models_to_run, $phi_model;
                    push @{$self->model_names}, $model_properties->{'name'} . "${i}_phi";
                }
            }
        }
    }

	my $modelfit = tool::modelfit->new(
		%{common_options::restore_options(@common_options::tool_options)},
		models => \@models_to_run, 
		base_dir => $self->directory . 'm1/',
		directory => undef,
		top_tool => 0,
        copy_data => 0,
	);

	$self->tools([]) unless defined $self->tools;
	push(@{$self->tools}, $modelfit);
}

sub modelfit_analyze
{
    my $self = shift;

    open my $fh, '>', 'results.csv';
    print $fh "Name,OFV\n";
    for (my $i = 0; $i < scalar(@{$self->run_models}); $i++) {
        my $model = $self->run_models->[$i];
        my $ofv;
        if ($model->is_run()) {
            my $output = $model->outputs->[0];
            $ofv = $output->get_single_value(attribute => 'ofv');
        }
        if (defined $ofv) {
            $ofv = sprintf("%.2f", $ofv);
        } else {
            $ofv = 'NA';
        }
        print $fh $self->model_names->[$i], ", ", $ofv, "\n";
    }
    close $fh;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
