package model::problem::theta;
#use Carp;
use include_modules;
use model::problem::record::theta_option;
use Math::Random;
use Moose;
use MooseX::Params::Validate;

extends 'model::problem::init_record';


sub set_random_inits
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 degree => { isa => 'Num', default => 0.1, optional => 1 },
		 bound_record => { isa => 'model::problem::theta', optional => 1 }
	);
	#this is a overloaded set_random_inits , this is only for theta
	my $degree = $parm{'degree'};
	my $bound_record = $parm{'bound_record'};
	if (($degree >= 1) or ($degree <= 0)){
		croak("Illegal input to theta->set_random_inits, degree $degree is not between 0 and 1");
	}

	return if ($self->fix or $self->prior);

	unless (defined $bound_record){
		$bound_record = $self;
	}

	my $nopt = scalar(@{$bound_record->options});
	unless (defined $self->options and scalar(@{$self->options})==$nopt){
		croak("bug in theta->set_random_inits: bound_record does not match self" );
	}
	for (my $j=0; $j< $nopt; $j++){
		my $option = $bound_record->options->[$j];
		next if ($option->fix or $option->prior or ($option->init == 0));
		my $range = $option->get_range(degree => $degree);

		my $val;
		for (my $k=0; $k<1000; $k++){
			$val = random_uniform(1, $range->[0], $range->[1] );
			last unless ($val == 0);
		}
		$self->options->[$j]->check_and_set_init(new_value=>$val);
	}#end loop over options
}


sub _read_options
{
	my $self = shift;

      {
	$self->same(0);
	my @row = ();
	$self->comment([]);
	my $global_index= $self->n_previous_rows()+1;
	if ( defined $self->record_arr ) {
	  for ( @{$self->record_arr} ) {
	    chomp;
	    s/^\s+//;
	    s/\s+$//;
	    # get rid of $THETA
	    s/^\s*\$\w+//;
	    next unless( length($_) > 0 );

	    if ( /^\s*\;/ ) {
	      # This is a comment row
	      push( @{$self->comment}, $_."\n" );
	    } else {
	      # Make sure that the labels and units are in one string
	      s/\;\s+/\;/g;

	      # Get rid of unwanted spaces
	      s/\s*\,\s*/\,/g;
	      s/\s+FIX/FIX/g;
	      s/\s+\)/\)/g;
	      s/\(\s+/\(/g;
	      #if there is no space between ) and ( then add one
	      s/\)\(/\) \(/g;

	      # Split thetas and labels/units
	      my ( $line, $comment ) = split( ";", $_, 2 );
	      
	      if ($line =~ /\)(x\d+)/){
					croak("Model parsing error: PsN does not support ".$1." notation in " . '$THETA');
	      }

	      # commas are optional in ([low,] init [,up] [FIXED])
	      #if there are spaces inside parentheses, they need to be replaced
	      #by commas. Optional for NONMEM but necessary for PsN
	      #remove all spaces before closing parentheses
	      #any number then space should be replaced by match without space with,
	      $line =~ s/(\([0-9.\-+eE]+)\s+/$1,/g; #first space
	      $line =~ s/(\([0-9.\-+eE,]+)\s+/$1,/g; #second
	      # Split the theta string to see if we have more than one theta.
	      @row = split( " ",$line );
	      if ( $#row <=0 ) {
		# If we only have one theta, send the whole row to option
		#comment will be label/unit
		#FIXED are attached to init
		$self->options([]) unless defined $self->options;
		push( @{$self->options},
		      model::problem::record::theta_option ->
		      new ( option_string => $line.';'.$comment,
			    coordinate_string => 'THETA'.$global_index));
		$global_index++;
	      } else {
		# If we have more than one theta, send one theta at a time to option
		#add comment to global comment
		push( @{$self->comment}, ';'.$comment."\n" ) if ($comment =~ /[^\s]+/);
		$self->options([]) unless defined $self->options;
		for my $li ( @row ) {
		  push( @{$self->options},
			model::problem::record::theta_option ->
			new ( option_string => $li ,
			      coordinate_string => 'THETA'.$global_index));
		  $global_index++;
		}
	      }
	    }
	  }
	}
      }

}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
