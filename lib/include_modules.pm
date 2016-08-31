package include_modules;
use Moose::Util::TypeConstraints;
use ext::Carp;
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(cluck longmess shortmess carp croak debugmessage);

our $debuglevel=0;
sub debugmessage{
	my $messagelevel=shift;
	my $message = shift;
	if ($messagelevel <= $debuglevel){
		carp($message);
	} 
}

subtype 'PositiveInt',
	as 'Int',
	where { $_ > 0 },
	message { "This number ($_) is not a positive integer" };
no Moose::Util::TypeConstraints;
1;
