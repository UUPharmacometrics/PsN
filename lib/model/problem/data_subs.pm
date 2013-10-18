start new
{
	foreach my $option ( @{$this->options} ) {
		if ( defined $option and $option -> name eq 'IGNORE') {
			my $value = $option -> value;
      chomp( $value );
      if ( $value =~ /\(*.\)/ ) {
				$value =~ s/\(//g;
				$value =~ s/\)//g;
				my @raw_list = split(',',$value);
				$this->ignore_list([]) unless defined $this->ignore_list;		# In case the reference is undef
				push( @{$this->ignore_list}, @raw_list );
      } else {
				$this->ignoresign($value);
      }
    }
  }
}
end new
