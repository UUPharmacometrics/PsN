use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::problem::data;


#---------------------------------------------------------------------
#         Inherited Class Packages
#---------------------------------------------------------------------
use base qw(model::problem::record);

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my %superParms;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'ignoresign' => 'SCALAR', 'ignore_list' => 'ARRAY' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
	}
	foreach my $givenp ( keys %parm ) {
		$superParms{$givenp} = $parm{$givenp} and next unless( defined $valid_parm{$givenp});

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::problem::data->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::problem::data->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::data->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::problem::data->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}


	bless $this, $class;
	model::problem::record::new($this,%superParms);

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($this). '-> new');
# line 2 "lib/model/problem/data_subs.pm" 
{
	foreach my $option ( @{$this->options} ) {
		if ( defined $option and $option -> name eq 'IGNORE') {
			my $value = $option -> value;
      chomp( $value );
      if ( $value =~ /\(*.\)/ ) {
				$value =~ s/\(//g;
				$value =~ s/\)//g;
				my @raw_list = split(',',$value);
				$this->ignore_list([]) unless defined $this->ignore_list;		# In case the reference is undef
				push( @{$this->ignore_list}, @raw_list );
      } else {
				$this->ignoresign($value);
      }
    }
  }
}
# line 75 libgen/model/problem/data.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($this). '-> new');
	# End of Non-Dia code #

	return $this;
};

sub ignoresign {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ignoresign'} = $parm;
	} else {
		return $self -> {'ignoresign'};
	}
}

sub ignore_list {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'ignore_list'} = $parm;
	} else {
		return $self -> {'ignore_list'};
	}
}

1;

