use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::iofv_module;
use Carp;
use Config;
use debug;


sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'enabled' => 'SCALAR', 'base_model' => 'm_model' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::iofv_module->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::iofv_module->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::iofv_module->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::iofv_module->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::iofv_module->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'enabled'} = defined $parm{'enabled'} ? $parm{'enabled'} : 0 unless defined $this -> {'enabled'};
	$this -> {'base_model'} = defined $parm{'base_model'} ? $parm{'base_model'} : 0 unless defined $this -> {'base_model'};

	bless $this, $class;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 107 "lib/model/iofv_module_subs.pm" 
{
  my $base_model = $this->base_model;
  
  if( $base_model -> is_option_set( record => 'subroutine',
				    name => 'CONTR' ) ){

    croak('CONTR in $SUBROUTINE is already set, iofv cannot be computed' );

  }

  $base_model -> add_records( type => 'subroutine',
			      record_strings => ['CONTR=iofvcont.f'] );

  $base_model -> set_records( type => 'contr',
			      record_strings => ['DATA=(ID)'] );
}
# line 74 libgen/model/iofv_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub enabled {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'enabled'} = $parm;
	} else {
		return $self -> {'enabled'};
	}
}

sub base_model {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'base_model'} = $parm;
	} else {
		return $self -> {'base_model'};
	}
}

sub post_process {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> post_process');
# line 130 "lib/model/iofv_module_subs.pm" 
{

  if( $PsN::nm_major_version == '7' ){
    croak("option -iofv is not yet supported for NONMEM7");
  }


  my $sizes_dir = $PsN::nmdir;
  $sizes_dir .= '/SIZES';
  if( $Config{osname} eq 'MSWin32' ){
    $sizes_dir =~ s/([^\\])\\([^\\])/$1\\\\$2/g;
    $sizes_dir =~ s/\//\\\\/g;
  }

  my $base_model = $self->base_model;

  # Figure out if we have an sdtab and what number it has
  my ( $sd_ref, $junk ) = $base_model -> problems -> [0] -> 
      _option_val_pos( name        => 'FILE',
		       record_name => 'table',
		       exact_match => 0 );

  my $sdno = '1';

  if( defined $sd_ref ) {
    foreach my $tabname ( @{$sd_ref} ) {
      if( $tabname =~ /[sd,pa]tab(\d+)/ ) {
	$sdno= $1;
      }
    }
  }

  open(CONTR,">iofvcont.f");
  print CONTR << "EOF";
      subroutine contr (icall,cnt,ier1,ier2)
      INCLUDE "$sizes_dir"
C     parameter (no=50)
      common /rocm1/ y(no),data(no,3),nobs
      integer nobs, un
      double precision cnt,y
      OPEN(80,FILE=\'iotab$sdno\')
      if (icall.le.1) return
      call ncontr (cnt,ier1,ier2,l2r)
C     individual obj. funct. value for indiv. jj = cnt
      write(80,10) data(1,1),cnt
   10 FORMAT(1E12.4E2,1E12.4E2) 
      return
      end

EOF
 
    close CONTR;

}
# line 169 libgen/model/iofv_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> post_process');
	# End of Non-Dia code #

}

sub post_run_process {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> post_run_process');
# line 190 "lib/model/iofv_module_subs.pm" 
{
  my $base_model = $self->base_model;

  # Figure out if we have an sdtab and what number it has
  my ( $sd_ref, $junk ) = $base_model -> problems -> [0] -> 
      _option_val_pos( name        => 'FILE',
		       record_name => 'table',
		       exact_match => 0 );

  my $sdno = '1';

  if( defined $sd_ref ) {
    foreach my $tabname ( @{$sd_ref} ) {
      if( $tabname =~ /[sd,pa]tab(\d+)/ ) {
	$sdno= $1;
      }
    }
  }

  my $data = $base_model -> datas -> [0];
  
  my $ids = $data -> column_to_array( column => $data -> idcolumn -1 );

  my ($first_id, $last_id) = ($ids -> [0],$ids -> [$#{$ids}]);

  if( -e "iotab$sdno" ){
    open( IOTAB, "<iotab$sdno" );
    my @iotab = <IOTAB>;
    my @values;
    my @starting_points;

    my $previous_id = $last_id;

    for( my $i = 0;$i < scalar @iotab; $i++ ){
      my @line = split( ' ',$iotab[$i] );
      
      push( @values, $line[1] );

      if( $previous_id == $last_id and $line[0] == $first_id ){
	push( @starting_points, $i );
      }

      $previous_id = $line[0];
    }

    if( defined $base_model -> covariance ){

      @iotab = @iotab[$starting_points[$#starting_points-1] .. $starting_points[$#starting_points]-1];

    } else {

      @iotab = @iotab[$starting_points[$#starting_points-1] .. $starting_points[$#starting_points]-1];

    }
    close(IOTAB);

    open IOTAB, ">iotab$sdno";
    print( IOTAB @iotab );
    close IOTAB;

    my @eo;
    if ( defined $base_model -> extra_output() ) {
      @eo = @{$base_model -> extra_output()};
    }
    
    push( @eo, "iotab$sdno" );
    $base_model -> extra_output( \@eo );
    
  } else {
    croak("Unable to open iotab: iotab$sdno ." );
  }
}
# line 253 libgen/model/iofv_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> post_run_process');
	# End of Non-Dia code #

}

1;

