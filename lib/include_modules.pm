package include_modules;
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

1;
