package model::problem::code_record;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record';

has 'code' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'pre_verbatim' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'verbatim_last' => ( is => 'rw', isa => 'ArrayRef[Str]' );
has 'verbatim_first' => ( is => 'rw', isa => 'ArrayRef[Str]' );

sub _format_record
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 number_format => { isa => 'Maybe[Int]', optional => 1 },
	);
	my $number_format = $parm{'number_format'};
	my @formatted;

	  my $fname;
	  if ( defined $self->verbatim_first
	       or defined $self->pre_verbatim
	       or defined $self->code
	       or defined $self->verbatim_last ) {
	      my @class_names = split('::',ref($self));
	      $fname = uc(pop(@class_names));
	      $fname = "\$".$fname; #and then prepend to $formatted[0] at the very end so that do not get line break
	  }

	  if ( defined $self->pre_verbatim ) {
	      push( @formatted, @{$self->pre_verbatim} );
	  }
	  if ( defined $self->verbatim_first ) {
	      push( @formatted, '"FIRST' );
	      push( @formatted, @{$self->verbatim_first} );
	  }
	  if ( defined $self->code ) {
	      push( @formatted, @{$self->code} );
	  }
	  if ( defined $self->verbatim_last ) {
	      push( @formatted, '"LAST' );
	      push( @formatted, @{$self->verbatim_last} );
	  }
	  if (scalar(@formatted)>0){
	      if ($formatted[0] =~/^\s*;/){ #first code line is a comment
		  unshift(@formatted,$fname);
	      }else{
		  $formatted[0] = $fname.' '.$formatted[0]; 
	      }
	  }

	return \@formatted;
}

sub _read_options
{
	my $self = shift;

	my $in = 0;
	if ( defined $self->record_arr ) {
	  $self->code([]);
	  my ( $first, $last, $have_first ) = ( 0, 0, 0 );
	  my @pre_verbatim = ();
	  for ( @{$self->record_arr} ) {
	    # Get rid of $RECORD and unwanted spaces
	    s/^\s*\$\w+//;
	    if ( /\" (\w+) = EVTREC\((\d+),(\d+)\)/ ) {
	      next;
	    }
	    if( /^\"\s*FIRST/ ) {
	      $first = 1;
	      $have_first = 1;
	      next;
	    }
	    if( /^\"\s*LAST/ ) {
	      $first = 0;
	      $last  = 1;
	      next;
	    }
	    if( $first or $last ) {
	      if( /^\"/ ) {
		if( $first ) {
			$self->verbatim_first([]) unless defined $self->verbatim_first;
		  push( @{$self->verbatim_first}, $_ );
		} else {
			$self->verbatim_last([]) unless defined $self->verbatim_last;
		  push( @{$self->verbatim_last}, $_ );
		}		  
	      } else {
		$first = 0;
		$last  = 0;
		push @{$self->code}, $_;
	      }
	    } else {
	      if ($have_first){
		push @{$self->code}, $_;
	      }else{
		push (@pre_verbatim,$_);
	      }
	    }
	  }
	  if ($have_first){
			$self->pre_verbatim([]) unless defined $self->pre_verbatim;
	    push( @{$self->pre_verbatim}, @pre_verbatim);
	  }else{
	    unshift @{$self->code}, @pre_verbatim;
	  }
	}
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
