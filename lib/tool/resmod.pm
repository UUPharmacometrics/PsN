package tool::resmod;

use strict;
use Moose;
use MooseX::Params::Validate;
use List::Util qw(max);
use include_modules;
use log;
use array;
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
has 'quartiles' => ( is => 'rw', isa => 'ArrayRef' );
has 'unique_dvid' => ( is => 'rw', isa => 'ArrayRef' );
has 'numdvid' => ( is => 'rw', isa => 'Int' );

# This array of hashes represent the different models to be tested. The 0th is the base model
our @residual_models =
(
	{
		name => 'base',
	    prob_arr => [
			'$PROBLEM CWRES base model',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ IGNORE=(DV.EQN.0) <dvidaccept>',
			'$PRED',
            'Y = THETA(1) + ETA(1) + ERR(1)',
			'$THETA .1',
			'$OMEGA 0.01',
			'$SIGMA 1',
			'$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC',
        ],
        base => 1,
	}, {
		name => 'IIV_on_RUV',
	    prob_arr => [
			'$PROBLEM CWRES omega-on-epsilon',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ IGNORE=(DV.EQN.0) <dvidaccept>',
			'$PRED',
            'Y = THETA(1) + ETA(1) + ERR(1) * EXP(ETA(2))',
			'$THETA .1',
			'$OMEGA 0.01',
            '$OMEGA 0.01',
			'$SIGMA 1',
			'$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC',
        ],
        parameters => [
            { name => "%CV", parameter => "OMEGA(2,2)", recalc => sub { sqrt($_[0])*100 } },
        ],
        use_base => 1,
    }, {
		name => 'power',
        need_ipred => 1,
	    prob_arr => [
			'$PROBLEM CWRES power IPRED',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ IGNORE=(DV.EQN.0) <dvidaccept>',
			'$PRED',
            'Y = THETA(1) + ETA(1) + ERR(1)*(IPRED)**THETA(2)',
			'$THETA .1',
			'$THETA .1',
			'$OMEGA 0.01',
			'$SIGMA 1',
			'$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC',
        ],
        parameters => [
            { name => "power", parameter => "THETA2" },
        ],
        use_base => 1,
	}, {
        name => 'time_varying_RUV',
        prob_arr => [
            '$PROBLEM CWRES time varying',
            '$INPUT <inputcolumns>',
            '$DATA ../<cwrestablename> IGNORE=@ IGNORE=(DV.EQN.0) <dvidaccept>',
            '$PRED',
            'Y = THETA(1) + ETA(1) + ERR(4)',
            'IF (<idv>.LT.<q1>) THEN',
            '    Y = THETA(1) + ETA(1) + ERR(1)',
            'END IF',
            'IF (<idv>.GE.<q1> .AND. <idv>.LT.<median>) THEN',
            '    Y = THETA(1) + ETA(1) + ERR(2)',
            'END IF',
            'IF (<idv>.GE.<median> .AND. <idv>.LT.<q3>) THEN',
            '    Y = THETA(1) + ETA(1) + ERR(3)',
            'END IF',
            '$THETA -0.0345794',
            '$OMEGA 0.5',
            '$SIGMA 0.5',
            '$SIGMA 0.5',
            '$SIGMA 0.5',
            '$SIGMA 0.5',
            '$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC',
        ],
        parameters => [
            { name => "sdeps_0-t1", parameter => "SIGMA(1,1)", recalc => sub { sqrt($_[0]) } },
            { name => "sdeps_t1-t2", parameter => "SIGMA(2,2)", recalc => sub { sqrt($_[0]) } },
            { name => "sdeps_t2-t3", parameter => "SIGMA(3,3)" , recalc => sub { sqrt($_[0]) } },
            { name => "sdeps_t3-inf", parameter => "SIGMA(4,4)", recalc => sub { sqrt($_[0]) } },
			{ name => "QUARTILES" },
        ],
        use_base => 1,
    }, {
        name => 'autocorrelation',
        prob_arr => [
			'$PROBLEM CWRES AR1',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ IGNORE=(DV.EQN.0) <dvidaccept>',
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
        ],
        parameters => [
            { name => "half-life", parameter => "THETA2" },
        ],
        use_base => 1,
    }, {
		name => 'tdist_base',
	    prob_arr => [
			'$PROBLEM CWRES t-distribution base mode',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ IGNORE=(DV.EQN.0) <dvidaccept>',
			'$PRED',
			'IPRED = THETA(1) + ETA(1)',
			'W     = THETA(2)',
			'IWRES=(DV-IPRED)/W',
			'LIM = 10E-14',
			'IF(IWRES.EQ.0) IWRES = LIM',
			'LL=-0.5*LOG(2*3.14159265)-LOG(W)-0.5*(IWRES**2)',
			'L=EXP(LL)',
			'Y=-2*LOG(L)',
			'$THETA  .1 ; Mean',
			'$THETA  (0,1) ; W : SD',
			'$OMEGA  0.0001',
			'$ESTIMATION MAXEVAL=99999 -2LL METH=1 LAPLACE PRINT=2 POSTHOC',
		],
		base => 2,
    }, {
        name => 'tdist_2ll_dfest',
        prob_arr => [
			'$PROBLEM CWRES laplace 2LL DF=est',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ IGNORE=(DV.EQN.0) <dvidaccept>',
			'$PRED',
            'IPRED = THETA(1) + ETA(1)',
            'W = THETA(2)',
            'DF = THETA(3) ; degrees of freedom of Student distribution',
            'SIG1 = W ; scaling factor for standard deviation of RUV',
            'IWRES = (DV - IPRED) / SIG1',
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
        parameters => [
            { name => "df", parameter => "THETA3" },
        ],
		use_base => 2,
    }, {
        name => 'dtbs_base',
        need_ipred => 1,
        prob_arr => [
			'$PROBLEM    CWRES dtbs base model',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ IGNORE=(DV.EQN.0) <dvidaccept>',
			'$SUBROUTINE CONTR=contr.txt CCONTR=ccontra.txt',
			'$PRED',
			'IPRT   = THETA(1)*EXP(ETA(1))',
			'WA     = THETA(2)',
			'LAMBDA = THETA(3)',
			'ZETA   = THETA(4)',
			'IF(IPRT.LT.0) IPRT=10E-14',
			'W = WA*IPRED**ZETA',
			'IPRTR = IPRT',
			'IF (LAMBDA .NE. 0 .AND. IPRT .NE.0) THEN',
			'	IPRTR = (IPRT**LAMBDA-1)/LAMBDA',
			'ENDIF',
			'IF (LAMBDA .EQ. 0 .AND. IPRT .NE.0) THEN',
			'	IPRTR = LOG(IPRT)',
			'ENDIF',
			'IF (LAMBDA .NE. 0 .AND. IPRT .EQ.0) THEN',
			'	IPRTR = -1/LAMBDA',
			'ENDIF',
			'IF (LAMBDA .EQ. 0 .AND. IPRT .EQ.0) THEN',
			'	IPRTR = -1000000000',
			'ENDIF',
			'IPRT = IPRTR',
			'Y = IPRT + ERR(1)*W',
			'IF(ICALL.EQ.4) Y=EXP(DV)',
			'$THETA  0.973255 ; IPRED 1',
			'$THETA  (0,1.37932) ; WA',
			'$THETA  0 FIX ; lambda',
			'$THETA  0 FIX ; zeta',
			'$OMEGA  0.0001',
			'$SIGMA  1  FIX',
			'$SIMULATION (1234)',
			'$ESTIMATION METHOD=1 INTER MAXEVALS=99999 PRINT=2 POSTHOC',
        ],
		base => 3,
    }, {
		name => 'dtbs',
        need_ipred => 1,
		prob_arr => [
			'$PROBLEM    CWRES dtbs model',
			'$INPUT <inputcolumns>',
			'$DATA ../<cwrestablename> IGNORE=@ IGNORE=(DV.EQN.0) <dvidaccept>',
			'$SUBROUTINE CONTR=contr.txt CCONTR=ccontra.txt',
			'$PRED',
			'IPRT   = THETA(1)*EXP(ETA(1))',
			'WA     = THETA(2)',
			'LAMBDA = THETA(3)',
			'ZETA   = THETA(4)',
			'IF(IPRT.LT.0) IPRT=10E-14',
			'W = WA*IPRED**ZETA',
			'IPRTR = IPRT',
			'IF (LAMBDA .NE. 0 .AND. IPRT .NE.0) THEN',
			'	IPRTR = (IPRT**LAMBDA-1)/LAMBDA',
			'ENDIF',
			'IF (LAMBDA .EQ. 0 .AND. IPRT .NE.0) THEN',
			'	IPRTR = LOG(IPRT)',
			'ENDIF',
			'IF (LAMBDA .NE. 0 .AND. IPRT .EQ.0) THEN',
			'	IPRTR = -1/LAMBDA',
			'ENDIF',
			'IF (LAMBDA .EQ. 0 .AND. IPRT .EQ.0) THEN',
			'	IPRTR = -1000000000',
			'ENDIF',
			'IPRT = IPRTR',
			'Y = IPRT + ERR(1)*W',
			'IF(ICALL.EQ.4) Y=EXP(DV)',
			'$THETA   0.973255 ; IPRED 1',
			'$THETA  (0,1.37932) ; WA',
			'$THETA     0.001    ; lambda',
			'$THETA     0.001    ; zeta',
			'$OMEGA  0.0001',
			'$SIGMA  1  FIX',
			'$SIMULATION (1234)',
			'$ESTIMATION METHOD=1 INTER MAXEVALS=99999 PRINT=2 POSTHOC',
		],
        parameters => [
            { name => "lambda", parameter => "THETA3" },
            { name => "zeta", parameter => "THETA4" },
        ],
		use_base => 3,
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

	# Create the contr.txt and ccontra.txt needed for dtbs
	open my $fh_contr, '>', "contr.txt";
	print $fh_contr <<'END';
      subroutine contr (icall,cnt,ier1,ier2)
      double precision cnt
      call ncontr (cnt,ier1,ier2,l2r)
      return
      end
END
	close $fh_contr;

	open my $fh_ccontra, '>', "ccontra.txt";
	print $fh_ccontra <<'END';
      subroutine ccontr (icall,c1,c2,c3,ier1,ier2)
      USE ROCM_REAL,   ONLY: theta=>THETAC,y=>DV_ITM2
      USE NM_INTERFACE,ONLY: CELS
!      parameter (lth=40,lvr=30,no=50)
!      common /rocm0/ theta (lth)
!      common /rocm4/ y
!      double precision c1,c2,c3,theta,y,w,one,two
      double precision c1,c2,c3,w,one,two
      dimension c2(:),c3(:,:)
      data one,two/1.,2./
      if (icall.le.1) return
      w=y(1)

         if(theta(3).eq.0) y(1)=log(y(1))
         if(theta(3).ne.0) y(1)=(y(1)**theta(3)-one)/theta(3)


      call cels (c1,c2,c3,ier1,ier2)
      y(1)=w
      c1=c1-two*(theta(3)-one)*log(y(1))

      return
      end
END
	close $fh_ccontra;

    # Find a table with ID, TIME, CWRES and extra_input (IPRED)
    my @columns = ( 'ID', $self->idv, 'CWRES' );
    my $cwres_table = $self->model->problems->[0]->find_table(columns => \@columns, get_object => 1);
    my $cwres_table_name = $self->model->problems->[0]->find_table(columns => \@columns);
    if (not defined $cwres_table) {
        die "Error original model has no table containing ID, IDV and CWRES\n";
    }

    # Do we have IPRED, DVID or TAD?
    my $have_ipred = 0;
    my $have_dvid = 0;
	my $have_tad = 0;
    for my $option (@{$cwres_table->options}) {
        if ($option->name eq 'IPRED') {
            $have_ipred = 1;
            push @columns, 'IPRED';
        } elsif ($option->name eq 'DVID') {
            $have_dvid = 1;
            push @columns, 'DVID';
        } elsif ($option->name eq 'TAD') {
			$have_tad = 1;
			push @columns, 'TAD',
		}
    }

    my $table = nmtablefile->new(filename => "../$cwres_table_name"); 

    my $unique_dvid;
    my $number_of_dvid = 1;
    if ($have_dvid) {
        my $dvid_column = $table->tables->[0]->header->{'DVID'};
        $unique_dvid = array::unique($table->tables->[0]->columns->[$dvid_column]);
		$self->unique_dvid($unique_dvid);
        $number_of_dvid = scalar(@$unique_dvid);
    }
	$self->numdvid($number_of_dvid);

	my $idv_column = $self->idv;
	if ($have_tad) {
		$idv_column = 'TAD';
	}
    my @quartiles = _calculate_quartiles(table => $table->tables->[0], column => $idv_column);
	$self->quartiles(\@quartiles);

	my @models_to_run;
    for my $model_properties (@residual_models) {
		my $tad;
		if ($have_tad and $model_properties->{'name'} eq 'time_varying') {
			$tad = 1;
		}
    	my $input_columns = _create_input(table => $cwres_table, columns => \@columns, ipred => $model_properties->{'need_ipred'}, tad => $tad);
        next if ($model_properties->{'need_ipred'} and not $have_ipred);

        my $accept = "";
        for (my $i = 0; $i < $number_of_dvid; $i++) {
            if ($have_dvid) {
                $accept = "IGNORE=(DVID.NEN." . $unique_dvid->[$i] . ")";
            }

            my @prob_arr = @{$model_properties->{'prob_arr'}};
            for my $row (@prob_arr) {
                $row =~ s/<inputcolumns>/$input_columns/g;
                $row =~ s/<cwrestablename>/$cwres_table_name/g;
                $row =~ s/<dvidaccept>/$accept/g;
                $row =~ s/<q1>/$quartiles[0]/g;
                $row =~ s/<median>/$quartiles[1]/g;
                $row =~ s/<q3>/$quartiles[2]/g;
                my $idv = $self->idv;
				if ($have_tad and $model_properties->{'name'} eq 'time_varying') {
					$idv = 'TAD';
				} 
                $row =~ s/<idv>/$idv/g;
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
            $dvid_suffix = "_DVID" . int($unique_dvid->[$i]) if ($have_dvid);
            my $cwres_model = model->new(
				directory => 'm1/',
				filename => $model_properties->{'name'} . "$dvid_suffix.mod",
				problems => [ $cwres_problem ],
				extra_files => [ $self->directory . '/contr.txt', $self->directory . '/ccontra.txt' ],
			);
            $cwres_model->_write();
            push @{$self->model_names}, $model_properties->{'name'} . "$dvid_suffix";
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

	# Remove the extra files
	unlink('contr.txt', 'ccontra.txt');

    open my $fh, '>', 'results.csv';
    print $fh "\x{EF}\x{BB}\x{BF}Model,dOFV,Parameters\n";		# EFBBBF is the byte order mark for UTF-8 letting Excel understand that this csv if encoded in UTF-8
    my %base_models;        # Hash from basemodelno to base model OFV
	my $current_dvid;
	for (my $dvid_index = 0; $dvid_index < $self->numdvid; $dvid_index++) {
		if ($self->numdvid > 1) {
            print $fh "\n";
			print $fh "DVID=", int($self->unique_dvid->[$dvid_index]), "\n";
		}
		for (my $i = 0; $i < scalar(@residual_models); $i++) {
			my $model_index = $dvid_index + $i * $self->numdvid;
			my $model = $self->run_models->[$model_index];
			my $model_name = $self->model_names->[$model_index];
			$model_name =~ s/_DVID\d+//;
			my $ofv;
			if ($model->is_run()) {
				my $output = $model->outputs->[0];
				$ofv = $output->get_single_value(attribute => 'ofv');
			}
			if (not defined $ofv) {
				$ofv = 'NA';
			}
			if (exists $residual_models[$i]->{'base'}) {        # This is a base model
				$base_models{$residual_models[$i]->{'base'}} = $ofv;
				next;
			}

			my $base_ofv;
			$base_ofv = $base_models{$residual_models[$i]->{'use_base'}};
			if ($base_ofv eq 'NA' or $ofv eq 'NA') {        # Really skip parameters if no base ofv?
				print $fh $model_name, ",NA,NA\n";
				next;
			}
			my $delta_ofv = $ofv - $base_ofv;
			$delta_ofv = sprintf("%.2f", $delta_ofv);
			print $fh $model_name, ",", $delta_ofv, ",";
			if (exists $residual_models[$i]->{'parameters'}) {
				for my $parameter (@{$residual_models[$i]->{'parameters'}}) {
					if ($parameter->{'name'} eq "QUARTILES") {
						print $fh sprintf("t1=%.3f", $self->quartiles()->[0]);
						print $fh sprintf(" t2=%.3f", $self->quartiles()->[1]);
						print $fh sprintf(" t3=%.3f", $self->quartiles()->[2]);
						next;
					}
					print $fh $parameter->{'name'} . "=";
					my $coordval;
					my $param = $parameter->{'parameter'};
					my $paramhash;
					if ($param =~ /OMEGA/) {
						$paramhash = $model->outputs->[0]->omegacoordval()->[0]->[0];
					} elsif ($param =~ /SIGMA/) {
						$paramhash = $model->outputs->[0]->sigmacoordval()->[0]->[0];
					} elsif ($param =~ /THETA/) {
						$paramhash = $model->outputs->[0]->thetacoordval()->[0]->[0];
					}
                    my $param_value = $paramhash->{$param};
                    if (exists $parameter->{'recalc'}) {
                        $param_value = $parameter->{'recalc'}->($param_value);
                    }
					print $fh sprintf("%.3f", $param_value);
					print $fh " ";
				}
			}
			print $fh "\n";
		}
	}
	close $fh;
}

sub _calculate_quartiles
{
	my %parm = validated_hash(\@_,
		table => { isa => 'nmtable' },
		column => { isa => 'Str' },
	);
	my $table = $parm{'table'};
	my $column = $parm{'column'};

	my $column_no = $table->header->{$column};
	my @data = grep { $_ } @{$table->columns->[$column_no]};    # Filter out all 0 CWRES as non-observations

	my @quartiles = array::quartiles(\@data);

	return @quartiles;
}

sub _create_input
{
	# Create $INPUT string from table
	my %parm = validated_hash(\@_,
		table => { isa => 'model::problem::table' },
		columns => { isa => 'ArrayRef' },
		ipred => { isa => 'Bool', default => 1 },		# Should ipred be included if in columns?
		tad => { isa => 'Bool', default => 0 },			# Should TAD be used instead of $self->idv?
	);
	my $table = $parm{'table'};
	my @columns = @{$parm{'columns'}};
	my $ipred = $parm{'ipred'};
	my $tad = $parm{'tad'};

	my $input_columns;
	my @found_columns;
	for my $option (@{$table->options}) {
		my $found = 0; 
		for (my $i = 0; $i < scalar(@columns); $i++) {
            if ($option->name eq $columns[$i] and not $found_columns[$i]) {
                $found_columns[$i] = 1;
                my $name = $option->name;
                $name = 'DV' if ($name eq 'CWRES');
				$name = 'DROP' if ($name eq 'IPRED' and not $ipred);
				$name = 'DROP' if ($name eq 'TAD' and not $tad);
				$name = 'DROP' if ($name eq 'TIME' and $tad);
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
	
	return $input_columns;
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
