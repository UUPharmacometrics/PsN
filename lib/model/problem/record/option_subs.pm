start include statements
end include statements

start new
{
	if ( defined $this->option_string ) {
	  $this -> _read_option;
	  delete $this -> {'option_string'};		# FIXME: Must be fixed with Moose
	}
}
end new

start _read_option
{
	#this gets strange for $PRIOR which has  NWPRI NTHETA=4,NETA=4,NTHP=4,NETP=4
	my $line = $self->option_string;
	chomp( $line );
	$line =~ s/^\s+//;
	$line =~ s/\s+$//;
	my @option = split( "=", $line ); # NTHETA    4,NETA    4,NTHP   4,NETP   4
	$self->name(shift( @option ));
	$self->value(join( "=", @option )); #4,NETA=4,NTHP=4,NETP=4

}
end _read_option

start _format_option
{
	$formatted = $self->name;
	if ( defined $self->value and $self->value ne '' ) {
	    $formatted = $formatted . '=' . $self->value; #NTHETA=4,NETA=4,NTHP=4,NETP=4
	}
}
end _format_option
