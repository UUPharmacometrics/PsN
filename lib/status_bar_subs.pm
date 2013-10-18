start tick
	$self->sofar($self->sofar + 1);

	if ( $self->sofar >= $self->steps ) {
		return 1;
  }

	if ( $self->steps / $self->width < 1 ) {
		$return = 1;
  } elsif ( $self->sofar % int($self->steps / $self->width) ) {
		$return = 0;
	} else {
		$return = 1;
	}
end tick

start print_step
	if ( $self->sofar == 0 ) {
		$output = '|';
	} else {
		if (( $self->steps / $self->width < 1 ) and ($self->steps > 0)) {
			$output = '.' x int($self->width / $self->steps);
		} else {
	  	$output = '.';
		}
	}

	if ( $self->sofar >= $self->steps ) {
		$output .= '|';
	}
end print_step
