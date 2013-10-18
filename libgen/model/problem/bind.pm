use strict;
#---------------------------------------------------------------------
#         Perl Class Package
#---------------------------------------------------------------------
package model::problem::bind;

#---------------------------------------------------------------------
#         Inherited Class Packages
#---------------------------------------------------------------------
use base qw(model::problem::record);

sub new {
	my $type  = shift;
	my $class = ref($type) || $type;
	my %superParms;
	my $this = ref($type) ? $type : {};
	%superParms = @_;

	bless $this, $class;
	model::problem::record::new($this,%superParms);

	# Start of Non-Dia code #
	# End of Non-Dia code #

	return $this;
};

1;

