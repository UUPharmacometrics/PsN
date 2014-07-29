package model::cwres_module;

#use Carp;
use include_modules;
use Moose;
use MooseX::Params::Validate;

has 'enabled' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'cwtab_names' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['cwtab.est', 'cwtab'] } );
has 'sdno' => ( is => 'rw', isa => 'Int' );
has 'mirror_plots' => ( is => 'rw', isa => 'Maybe[Int]' );
has 'problem' => ( is => 'rw', required => 1, isa => 'model::problem' );

sub BUILD
{
	my $self = shift;
	my $mirror_name = $self->mirror_plots ? 'sim' : '';

#  if( $PsN::nm_major_version == '7' ){
#    croak("option -cwres is not supported for NONMEM7, ".
#		    "since CWRES can be requested directly in \$TABLE with this NONMEM version." );
#  }

	# Problem is the modelfile problem we are modifing to compute CWRES.

	my $prob = $self->problem;

	# Get number of etas and eps;
	my $nthetas = $prob -> record_count( record_name => 'theta' );
	my $netas = $prob -> nomegas();
	my $neps = $prob -> nsigmas();

	# Get current comres number
	my $comresno;
	my ( $crno_ref ) = $prob ->
		_option_val_pos( name        => 'COMRES',
						 record_name => 'abbreviated',
						 exact_match => 0 );
	if( defined $crno_ref ) {
		$comresno = $crno_ref -> [0];
	}
	
	# Add $ABBREVIATED if necessary
	if ( defined $comresno ) {
		$prob -> remove_option( record_name  => 'abbreviated',
								option_name  => 'COMRES' );
		$prob -> add_option( record_name  => 'abbreviated',
							 option_name  => 'COMRES',
							 option_value => ($netas+$neps+$comresno) );
	} else {  
		$prob -> add_records( type => 'abbreviated',
							  record_strings => [ "COMRES=".($netas+$neps) ] );
	}

	# get the table names. They are needed below and further down
	my @cwtab_names = @{$self -> cwtab_names};

	# Figure out if we have an sdtab and what number it has
	my ( $sd_ref ) = $prob ->		_option_val_pos( name        => 'FILE',
													 record_name => 'table',
													 exact_match => 0 );
	if( defined $sd_ref ) {
		foreach my $tabname ( @{$sd_ref} ) {
			if( $tabname =~ /[sd,pa]tab(\d+)/i ) {
				my $sdno = $1;
				if( $sdno eq '' ){
					$sdno = 1;
				}
				$self -> sdno($sdno);
				for( my $i = 0; $i <= $#cwtab_names; $i++ ) {

					# This regular expression is probably quite unneccessary. It
					# matches evertyhing before the first 'dot' in a filename,
					# the dot, and the rest of the name(dots included). We can
					# then inject a number before the first dot. It also handles
					# no dots, the number will then be injected at the end of
					# the filename.

					if( $cwtab_names[$i] =~ /([^\.]+)(\.{0,1})(.*)/ ) {
						$cwtab_names[$i] = $1.$sdno.$mirror_name.$2.$3;
					}
				}
				$self -> cwtab_names( \@cwtab_names);
				last;
			}
		}
	}


	# Figure out wheter we have and 'ADVAN' option. By not using
	# "exact_match" we can search for a prefix of the different ADVAN
	# options.

	my ($advan) = $prob -> _option_val_pos( record_name => 'subroutine',
											name => 'ADVAN',
											exact_match => 0);

	my $have_advan = scalar(@{$advan}) > 0;

	if( $PsN::nm_major_version == 5 ){
		if( $have_advan ){
			# infn.f will be written in "post_process"
		} else {
			my $code = $prob -> preds -> [0] -> verbatim_first;
			unless( defined $code ){
				$code = [];
				$prob -> preds -> [0] -> verbatim_first($code);
			}
			unshift(@{$code},
					# fortan code
					('"      COMMON /ROCM6/ THETAF(40),OMEGAF(30,30),SIGMAF(30,30)',
					 '"      COMMON /ROCM7/ SETH(40),SEOM(30,30),SESIG(30,30)',
					 '"      COMMON /ROCM8/ OBJECT ',
					 '"      DOUBLE PRECISION THETAF, OMEGAF, SIGMAF ',
					 '"      DOUBLE PRECISION OBJECT ',
					 '"      REAL SETH,SEOM,SESIG ',
					 '"      INTEGER J,I ',
					 '"      INTEGER MODE ',
					 '"      INTEGER NTH,NETA,NEPS ',
					 "\"      DATA NTH,NETA,NEPS/$nthetas,$netas,$neps/ ")
				);

			# Abbrev code 
			$code = $prob -> preds -> [0] -> code;
			
			# fortran code
			push( @{$code},
				  ('"      IF (ICALL.EQ.0) THEN',
				   '"C     open files here, if necessary',
				   '"         OPEN(50,FILE=\'cwtab'.$self -> sdno().$mirror_name.'.est\')') );

			push( @{$code},
				  # fortan code
				  ('"      ENDIF',
				   '"      IF (ICALL.EQ.3) THEN',
				   '"         MODE=0',
				   '"         CALL PASS(MODE)',
				   '"         MODE=1',
				   '"         WRITE (50,*) \'ETAS\'',
				   '" 20      CALL PASS(MODE)',
				   '"         IF (MODE.EQ.0) GO TO 30',
				   '"         IF (NEWIND.NE.2) THEN',
				   '"            CALL GETETA(ETA)',
				   '"            WRITE (50,97) (ETA(I),I=1,NETA)',
				   '"         ENDIF',
				   '"         GO TO 20',
				   '" 30      CONTINUE',
				   '"         WRITE (50,*) \'THETAS\'',
				   '"         WRITE (50,99) (THETAF(J),J=1,NTH)',
				   '"         WRITE (50,*) \'OMEGAS\'',
				   '"         DO 7000 I=1,NETA',
				   '" 7000       WRITE (50,99) (OMEGAF(I,J),J=1,NETA)',
				   '"            WRITE (50,*) \'SIGMAS\'',
				   '"            DO 7999 I=1,NEPS',
				   '" 7999          WRITE (50,99) (SIGMAF(I,J),J=1,NEPS)',
				   '"            ENDIF',
				   '" 99         FORMAT (20E15.7)',
				   '" 98         FORMAT (2I8)',
				   '" 97         FORMAT (10E15.7)')
				);
			
		}
#	} elsif ( $PsN::nm_major_version == 6 ) {
	} else {

		my $code;

		if( $have_advan ){
			unless( $prob -> infns ){
				$prob -> add_records( type => 'infn',
									  record_strings => [] );
			}
			
			$code = $prob -> infns -> [0] -> code;

		} else {

			$code = $prob -> preds -> [0] -> code;
			
		}
		
		push( @{$code}, 
			  'IF (ICALL.EQ.3) THEN',
			  '  OPEN(50,FILE=\'cwtab'.$self -> sdno().$mirror_name.'.est\')',
			  '  WRITE (50,*) \'ETAS\'',
			  '  DO WHILE(DATA)',
			  '    IF (NEWIND.LE.1) WRITE (50,*) ETA',
			  '  ENDDO',                                
			  '  WRITE (50,*) \'THETAS\'',
			  '  WRITE (50,*) THETA',
			  '  WRITE (50,*) \'OMEGAS\'',
			  '  WRITE (50,*) OMEGA(BLOCK)',
			  '  WRITE (50,*) \'SIGMAS\'',
			  '  WRITE (50,*) SIGMA(BLOCK)',
			  'ENDIF' );
	}


	my $code_records;
	if( $have_advan ){
		# We have and ADVAN option in $SUBROUTINE, get $ERROR code
		$code_records = $prob -> errors;
		
		# If we also use version 5, we must include "infn.f" in $SUBROUTINE
		if( $PsN::nm_major_version == 5 ){
			$prob -> add_option( record_name => 'subroutine',
								 option_name => 'INFN',
								 option_value=> 'infn.f' );
		}
		
	} else {
		# No ADVAN subroutine, we should modify $PRED code
		$code_records = $prob -> preds;
	}

	# Get code array reference, so we can update the code inplace.
	my $code = $code_records -> [0] -> verbatim_last;

	unless( defined $code ){
		$code = [];
		$code_records -> [0] -> verbatim_last($code);
	}

	my $com = defined $comresno ? $comresno + 1 : 1;

	my @table_row;

	for( 1..$netas ){
		push( @{$code},"\"  COM($com)=G($_,1)" );
		push( @table_row, "COM($com)=G$_"."1");
		$com++;
	}
	
	for( 1..$neps ){
		if( $have_advan ){
			push( @{$code},"\"  COM($com)=HH($_,1)" );
			push( @table_row, "COM($com)=H$_"."1" );
		} else {
			push( @{$code},"\"  COM($com)=H($_,1)" );
			push( @table_row, "COM($com)=H$_"."1" );
		}
		$com++;
	}

	my ($mdv) = $prob -> _option_val_pos( record_name => 'input',
										  name => 'MDV' );

	if( defined $prob -> preds and scalar(@{$mdv}) == 0  ){
		$mdv = '';
	} else {
		$mdv = 'MDV';
	}

	$prob -> add_records( type => 'table',
						  record_strings => ['ID ',
											 join(' ',@table_row),
											 "IPRED DV $mdv NOPRINT ".
											 "ONEHEADER FILE=cwtab".$self -> sdno().'.deriv'] );

}

sub post_process
{
	my $self = shift;

	my ($advan) = $self->problem -> _option_val_pos( record_name => 'subroutine',
													 name => 'ADVAN',
													 exact_match => 0);

	my $mirror_name = $self->mirror_plots ? 'sim' : '';

	if( $PsN::nm_major_version == 5 and scalar(@{$advan}) > 0 ){

		my $ntheta = $self->problem->record_count( record_name => 'theta' );
		my $neta = $self->problem->nomegas;
		my $neps = $self->problem->nsigmas;
		
		open(INFN, ">infn.f");
		
print INFN << "EOF";
		SUBROUTINE INFN(ICALL,THETA,DATREC,INDXS,NEWIND)
			DIMENSION THETA(*),DATREC(*),INDXS(*)
			DOUBLE PRECISION THETA
			COMMON /ROCM6/ THETAF(40),OMEGAF(30,30),SIGMAF(30,30)
			COMMON /ROCM7/ SETH(40),SEOM(30,30),SESIG(30,30)
			COMMON /ROCM8/ OBJECT
			COMMON /ROCM9/ IERE,IERC
			DOUBLE PRECISION THETAF, OMEGAF, SIGMAF
			DOUBLE PRECISION OBJECT
			REAL SETH,SEOM,SESIG
			DOUBLE PRECISION ETA(10)
			INTEGER J,I
			INTEGER IERE,IERC
			INTEGER MODE
			INTEGER NTH,NETA,NEPS
EOF

			print INFN "      DATA NTH,NETA,NEPS/$ntheta,$neta,$neps/\n";

		print INFN "      IF (ICALL.EQ.0) THEN\nC     open files here, if necessary\n";
		print INFN "      OPEN(50,FILE='cwtab".$self -> sdno().$mirror_name.".est')\n";

print INFN << "EOF";
		ENDIF
			IF (ICALL.EQ.3) THEN
			MODE=0
			CALL PASS(MODE)
			MODE=1
			WRITE (50,*) 'ETAS'
			20      CALL PASS(MODE)
			IF (MODE.EQ.0) GO TO 30
			IF (NEWIND.NE.2) THEN
            CALL GETETA(ETA)
            WRITE (50,97) (ETA(I),I=1,NETA)
			ENDIF
			GO TO 20
			30      CONTINUE
			WRITE (50,*) 'THETAS'
			WRITE (50,99) (THETAF(J),J=1,NTH)
			WRITE (50,*) 'OMEGAS'
			DO 7000 I=1,NETA
			7000       WRITE (50,99) (OMEGAF(I,J),J=1,NETA)
            WRITE (50,*) 'SIGMAS'
            DO 7999 I=1,NEPS
			7999          WRITE (50,99) (SIGMAF(I,J),J=1,NEPS)
            ENDIF
			99         FORMAT (20E15.7)
			98         FORMAT (2I8)
			97         FORMAT (10E15.7)
            RETURN
            END
EOF

			close INFN;
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
