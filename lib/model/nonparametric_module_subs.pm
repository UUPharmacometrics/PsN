# {{{ include

start include statements

use Data::Dumper;

end include statements

# }}} include

# {{{ enable

start enable

$self->enabled(1);

end enable

# }}} enable

# {{{ disable

start disable

$self->enabled(0);

end disable

# }}} disable

# {{{ format_etas_msfi

start format_etas_msfi

my $msfi_str = $self -> marginals_msfoname;

my $msfi = model::problem::msfi -> new ( record_arr => [$msfi_str] );

@formatted = @{$msfi -> _format_record};

end format_etas_msfi

# }}} format_etas_msfi

# {{{ format_etas_table

start format_etas_table

@formatted = @{$self -> format_table( type => 'etas' )} ;

end format_etas_table

# }}} format_etas_table

# {{{ format_marginals_table

start format_marginals_table

@formatted = @{$self -> format_table( type => 'marginals' )} ;

end format_marginals_table

# }}} format_marginals_table

# {{{ format_table

start format_table

my $nomegas = $self->problem->nomegas;
	
my $table_str = 'ID';
for( my $j = 1; $j <= $nomegas; $j++ ) {
  $table_str = $table_str.' ETA'.$j;
}
my $accessor = $type.'_tablename';
$table_str = $table_str.' FILE='.$self -> $accessor.
    ' NOAPPEND ONEHEADER NOPRINT FIRSTONLY';

my $table = model::problem::table -> new ( record_arr => [$table_str] );

@formatted = @{$table -> _format_record};

end format_table

# }}} format_table

# {{{ format_etas_nonparametric

start format_etas_nonparametric

@formatted = @{$self -> format_nonparametric( type => 'etas' )};

end format_etas_nonparametric

# }}} format_etas_nonparametric

# {{{ format_marginals_nonparametric

start format_marginals_nonparametric

@formatted = @{$self -> format_nonparametric( type => 'marginals' )};

end format_marginals_nonparametric

# }}} format_marginals_nonparametric

# {{{ format_nonparametric

start format_nonparametric

my $accessor = $type.'_msfoname';
my $nonp_str = uc($type).' UNCONDITIONAL MSFO='.$self -> $accessor.
    ' NOAPPEND ONEHEADER NOPRINT FIRSTONLY';

my $nonp = model::problem::nonparametric -> new ( record_arr => [$nonp_str] );

@formatted = @{$nonp -> _format_record};

end format_nonparametric

# }}} format_nonparametric

# {{{ etas_tablename

start etas_tablename

my $modname = $self->model->filename;
my $probnum = $self->problem_number;

$filename = $modname.'_'.$probnum.'.psn_nonp_etas';

end etas_tablename

# }}} etas_tablename

# {{{ marginals_tablename

start marginals_tablename

my $modname = $self->model->filename;
my $probnum = $self->problem_number;

$filename = $modname.'_'.$probnum.'.psn_marginals';

end marginals_tablename

# }}} marginals_tablename

# {{{ etas_msfoname

start etas_msfoname

my $probnum = $self -> problem_number;

$filename = 'etasmsfo'.$probnum;

end etas_msfoname

# }}} etas_msfoname

# {{{ marginals_msfoname

start marginals_msfoname

my $probnum = $self -> problem_number;

$filename = 'marginalsmsfo'.$probnum;

end marginals_msfoname

# }}} marginals_msfoname

# {{{ etas_table_exists

start etas_table_exists

my $directory = $self->model->directory;
my $filename = $self->etas_tablename;

$exists = -e $directory.$filename ? 1 : 0;

end etas_table_exists

# }}} etas_table_exists

# {{{ marginals_table_exists

start marginals_table_exists

my $directory = $self->model->directory;
my $filename = $self->marginals_tablename;

$exists = -e $directory.$filename ? 1 : 0;

end marginals_table_exists

# }}} marginals_table_exists

# {{{ problem_number

start problem_number

if ( defined $self->model and defined $self->model->problems ) {
  # This is the default
  my @modprobs = @{$self->model->problems};
  for( my $i = 0; $i <= $#modprobs; $i++ ) {
    $problem_number = ($i+1) if ( $modprobs[$i] eq $self->problem );
  }
} else {
  # This happens when the problems are not yet set in the model
  $problem_number = $self->temp_problem_number;
}

end problem_number

# }}} problem_number
