# {{{ new
start new
      {
	my @mo_bins = ();
	foreach my $model ( @{$this->models} ) {
	  my @pr_bins = ();
	  foreach my $data ( @{$model->datas}  ) {
	    push( @pr_bins, $data -> count_ind );
	  }
	  push( @mo_bins, \@pr_bins );
	}
	$this->bins(\@mo_bins);
      }
end new
# }}}

# {{{ modelfit_analyze
start modelfit_analyze
end modelfit_analyze
# }}}

