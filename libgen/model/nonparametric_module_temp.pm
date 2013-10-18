use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::nonparametric_module;

use Data::Dumper;

use debug;


sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my $this = ref($type) ? $type : {};
	my %parm  = @_;
	my %valid_parm = ( 'enabled' => 'SCALAR', 'model' => 'model', 'problem' => 'model::problem',
			'temp_problem_number' => 'SCALAR' );

	if( defined $parm{'reference_object'} ){
		foreach my $possible_parm( keys %valid_parm ){
			if( not exists $parm{$possible_parm} and not exists $this -> {$possible_parm} and exists $parm{'reference_object'} -> {$possible_parm} ){
				$parm{$possible_parm} = $parm{'reference_object'} -> {$possible_parm};
			}
		}
		$parm{'reference_object'} = undef;
	}
	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::nonparametric_module->new: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->new: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp} or defined $this -> {$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->new: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->new: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
		$this -> {$givenp} = $parm{$givenp} unless defined $this -> {$givenp};
	}

	$this -> {'enabled'} = defined $parm{'enabled'} ? $parm{'enabled'} : 0 unless defined $this -> {'enabled'};

	bless $this, $class;

	# Start of Non-Dia code #
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

sub model {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'model'} = $parm;
	} else {
		return $self -> {'model'};
	}
}

sub problem {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'problem'} = $parm;
	} else {
		return $self -> {'problem'};
	}
}

sub temp_problem_number {
	my $self = shift;
	my $parm = shift;

	# Start of Non-Dia code #
	# End of Non-Dia code #

	if( defined($parm) ){
		$self -> {'temp_problem_number'} = $parm;
	} else {
		return $self -> {'temp_problem_number'};
	}
}

sub etas_tablename {
	my $self = shift;
	my $filename;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> etas_tablename');
# line 126 "lib/model/nonparametric_module_subs.pm" 

my $modname = $self->model->filename;
my $probnum = $self->problem_number;

$filename = $modname.'_'.$probnum.'.psn_nonp_etas';

# line 131 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> etas_tablename');
	# End of Non-Dia code #

	return $filename;
}

sub marginals_tablename {
	my $self = shift;
	my $filename;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> marginals_tablename');
# line 139 "lib/model/nonparametric_module_subs.pm" 

my $modname = $self->model->filename;
my $probnum = $self->problem_number;

$filename = $modname.'_'.$probnum.'.psn_marginals';

# line 151 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> marginals_tablename');
	# End of Non-Dia code #

	return $filename;
}

sub format_etas_table {
	my $self = shift;
	my @formatted;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> format_etas_table');
# line 48 "lib/model/nonparametric_module_subs.pm" 

@formatted = @{$self -> format_table( type => 'etas' )} ;

# line 168 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> format_etas_table');
	# End of Non-Dia code #

	return \@formatted;
}

sub format_marginals_table {
	my $self = shift;
	my @formatted;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> format_marginals_table');
# line 58 "lib/model/nonparametric_module_subs.pm" 

@formatted = @{$self -> format_table( type => 'marginals' )} ;

# line 185 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> format_marginals_table');
	# End of Non-Dia code #

	return \@formatted;
}

sub format_table {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::nonparametric_module->format_table: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_table: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_table: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_table: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_table: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type = $parm{'type'};
	my @formatted;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> format_table');
# line 68 "lib/model/nonparametric_module_subs.pm" 

my $nomegas = $self->problem->nomegas;
	
my $table_str = 'ID';
for( my $j = 1; $j <= $nomegas; $j++ ) {
  $table_str = $table_str.' ETA'.$j;
}
my $accessor = $type.'_tablename';
$table_str = $table_str.' FILE='.$self -> $accessor.
    ' NOAPPEND ONEHEADER NOPRINT FIRSTONLY';

my $table = model::problem::table -> new ( record_arr => [$table_str] );

@formatted = @{$table -> _format_record};

# line 240 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> format_table');
	# End of Non-Dia code #

	return \@formatted;
}

sub problem_number {
	my $self = shift;
	my $problem_number;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> problem_number');
# line 202 "lib/model/nonparametric_module_subs.pm" 

if ( defined $self->model and defined $self->model->problems ) {
  # This is the default
  my @modprobs = @{$self->model->problems};
  for( my $i = 0; $i <= $#modprobs; $i++ ) {
    $problem_number = ($i+1) if ( $modprobs[$i] eq $self->problem );
  }
} else {
  # This happens when the problems are not yet set in the model
  $problem_number = $self->temp_problem_number;
}

# line 266 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> problem_number');
	# End of Non-Dia code #

	return $problem_number;
}

sub enable {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> enable');
# line 14 "lib/model/nonparametric_module_subs.pm" 

$self->enabled(1);

# line 282 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> enable');
	# End of Non-Dia code #

}

sub disable {
	my $self = shift;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> disable');
# line 24 "lib/model/nonparametric_module_subs.pm" 

$self->enabled(0);

# line 297 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> disable');
	# End of Non-Dia code #

}

sub etas_table_exists {
	my $self = shift;
	my $exists = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> etas_table_exists');
# line 176 "lib/model/nonparametric_module_subs.pm" 

my $directory = $self->model->directory;
my $filename = $self->etas_tablename;

$exists = -e $directory.$filename ? 1 : 0;

# line 316 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> etas_table_exists');
	# End of Non-Dia code #

	return $exists;
}

sub marginals_table_exists {
	my $self = shift;
	my $exists = 0;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> marginals_table_exists');
# line 189 "lib/model/nonparametric_module_subs.pm" 

my $directory = $self->model->directory;
my $filename = $self->marginals_tablename;

$exists = -e $directory.$filename ? 1 : 0;

# line 336 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> marginals_table_exists');
	# End of Non-Dia code #

	return $exists;
}

sub format_nonparametric {
	my $self = shift;
	my %parm  = @_;
	my %valid_parm = ( 'type' => 'SCALAR' );

	foreach my $givenp ( keys %parm ) {
		'debug' -> die( message => "ERROR in model::nonparametric_module->format_nonparametric: Parameter \'$givenp\' is not valid" )
			unless( defined $valid_parm{$givenp} );

		if( $valid_parm{$givenp} =~ /^m_(.*)/ ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_nonparametric: You need to specify a \'$givenp\'!" )
				unless(defined $parm{$givenp});
			$valid_parm{$givenp} = $1;
		}

		if( $valid_parm{$givenp} eq 'SCALAR' or $valid_parm{$givenp} eq 'REF' ){
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_nonparametric: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref(\$parm{$givenp}))
				if( defined $parm{$givenp} and ref(\$parm{$givenp}) ne $valid_parm{$givenp} );
		} elsif( $parm{$givenp} =~ /=HASH\(/ and $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_nonparametric: " . lc($valid_parm{$givenp}) . " parameter '$givenp' is of wrong type:" . lc(ref($parm{$givenp})) )
				if( defined $parm{$givenp} and lc(ref($parm{$givenp})) ne lc($valid_parm{$givenp}));
		} elsif( $valid_parm{$givenp} ne '' ) {
			'debug' -> die( message => "ERROR in model::nonparametric_module->format_nonparametric: " . $valid_parm{$givenp} . " parameter '$givenp' is of wrong type:" . ref($parm{$givenp}))
				if( defined $parm{$givenp} and ref($parm{$givenp}) ne $valid_parm{$givenp} );
		}
	}

	my $type = $parm{'type'};
	my @formatted;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> format_nonparametric');
# line 110 "lib/model/nonparametric_module_subs.pm" 

my $accessor = $type.'_msfoname';
my $nonp_str = uc($type).' UNCONDITIONAL MSFO='.$self -> $accessor.
    ' NOAPPEND ONEHEADER NOPRINT FIRSTONLY';

my $nonp = model::problem::nonparametric -> new ( record_arr => [$nonp_str] );

@formatted = @{$nonp -> _format_record};

# line 385 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> format_nonparametric');
	# End of Non-Dia code #

	return \@formatted;
}

sub format_etas_nonparametric {
	my $self = shift;
	my @formatted;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> format_etas_nonparametric');
# line 90 "lib/model/nonparametric_module_subs.pm" 

@formatted = @{$self -> format_nonparametric( type => 'etas' )};

# line 402 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> format_etas_nonparametric');
	# End of Non-Dia code #

	return \@formatted;
}

sub format_marginals_nonparametric {
	my $self = shift;
	my @formatted;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> format_marginals_nonparametric');
# line 100 "lib/model/nonparametric_module_subs.pm" 

@formatted = @{$self -> format_nonparametric( type => 'marginals' )};

# line 419 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> format_marginals_nonparametric');
	# End of Non-Dia code #

	return \@formatted;
}

sub format_etas_msfi {
	my $self = shift;
	my @formatted;

	# Start of Non-Dia code #
        'debug' -> warn(level => 3, message => "Entering \t" . ref($self). '-> format_etas_msfi');
# line 34 "lib/model/nonparametric_module_subs.pm" 

my $msfi_str = $self -> marginals_msfoname;

my $msfi = model::problem::msfi -> new ( record_arr => [$msfi_str] );

@formatted = @{$msfi -> _format_record};

# line 440 libgen/model/nonparametric_module.pm 
        'debug' -> warn(level => 3, message => "Leaving \t" . ref($self). '-> format_etas_msfi');
	# End of Non-Dia code #

	return \@formatted;
}

1;

