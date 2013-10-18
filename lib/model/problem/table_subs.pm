# {{{ new

start new

end new

# }}} new

# {{{ contify

start contify

my @options = @{$self -> options};

for( my $j = 1; $j <= scalar @options; $j++ ) {
  $cont_column = $j if( $options[$j-1] -> name eq 'CONT' );
}

if( not defined $cont_column ) {
  $self -> options( [] );
  $self -> add_option( init_data => { name => 'CONT' } );
  unshift( @options, $self -> options -> [0] );
  $self -> options( \@options );
  $cont_column = 1;
}

end contify

# }}} contify
