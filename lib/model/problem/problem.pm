package model::problem::problem;

use Moose;
use MooseX::Params::Validate;

extends 'model::problem::record';

sub update_runrecord_tags{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  based_on => { isa => 'Maybe[Str]', optional => 1 },
							  new_comment  => { isa => 'Maybe[Str]', optional => 1 },
							  add_tags => { isa => 'Bool', optional => 0 }
		);
	my $based_on = $parm{'based_on'};
	my $new_comment = $parm{'new_comment'};
	my $add_tags = $parm{'add_tags'};

	#double ;;, may have space and number. 'Based on'is handled separately below
	my @tags=('2. Description:','3. Label:','4. Structural model:','5. Covariate model:');
	push (@tags,('6. Inter-individual variability:','7. Inter-occasion variability:','8. Residual variability:'));
	push (@tags,'9. Estimation:');


	my @print_order = defined $self->print_order ? @{$self->print_order} : ();	
	my @comments = defined($self->comment) ? @{$self->comment} : ();    
	my @options = defined($self->options) ? @{$self->options} : ();

	my $numopts = scalar(@options);

	#if have no original opts, add a dummy one to get new comment/tags after $PROB instead of before
	if (((defined $new_comment) or $add_tags) and ($numopts == 0)){
		$self -> _add_option( option_string => 'UPDATED' );
		$numopts=1;
	}

	if (defined $new_comment){
		push(@comments, "\n".';'.$new_comment);
		push( @print_order, $numopts );
	}
	if ($add_tags){
		#add all, set based on
		#start with Based on
		my $line = "\n".';; 1. Based on: '.$based_on;
		push(@comments, $line);
		push( @print_order, $numopts );
		foreach my $tag (@tags){
			push(@comments, ";; $tag");
			push( @print_order, $numopts );
		}
	}else{
		for (my $j=0; $j< scalar(@comments); $j++){
			if ($comments[$j] =~ /Based on:/ ){
				my $line = ';; 1. Based on: '.$based_on."\n";
				$comments[$j] = $line;
			}
		}
	}
	if ((defined $new_comment) or $add_tags){
		$comments[$#comments] .= "\n";
	}
	$self->comment(\@comments);
	$self->print_order(\@print_order);

}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
