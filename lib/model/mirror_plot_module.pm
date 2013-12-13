package model::mirror_plot_module;

# The mirror plot module will add an extra problem that simulates from
# the original model. We assume here that there is only one problem in
# the original model.

# The parameters are the number of mirror plots to obtain (which is the
# number of simulations we must perform).

# The tables will be renamed from *tab1 to *tab1sim



use Carp;
use Math::Random;
use Moose;
use MooseX::Params::Validate;

has 'enabled' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'cwres' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'mirror_from_lst' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'nr_of_mirrors' => ( is => 'rw', isa => 'Int' );
has 'base_model' => ( is => 'rw', required => 1, isa => 'model' );
has 'last_est_complete' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'niter_eonly' => ( is => 'rw', isa => 'Maybe[Int]' );

sub BUILD
{
	my $this  = shift;

  my $base_model = $this->base_model;
  
  my $prob_num;

  if( $this -> cwres and not $base_model->is_run ){
    croak('To create mirror plots for cwres tables, you must have run the model with cwres separately' );
  }

  if( $this->cwres and not $this->mirror_from_lst ) {
    carp('MSFO computation method cannot be used with cwres. mirror_from_lst enabled' );
    $this->mirror_from_lst(1);
  }

  my $msfo_names = $base_model -> msfo_names( problem_numbers => [1] );
  my $msfi_names = $base_model -> msfi_names( );

  if( $base_model -> is_run and $this->mirror_from_lst ) {

    # 1. Update initial estimates from file.

    $base_model -> update_inits( from_model => $base_model );

    my $table_file_names = $base_model -> table_names( problem_numbers => [1] );    

    for( my $i; $i < @{$table_file_names -> [0]}; $i++ ){
      $table_file_names -> [0] -> [$i] =~ s/(.*)(\d+)(.*)/$1$2sim$3/;
    }
    
    $base_model -> table_names( new_names => $table_file_names,
				problem_numbers => [1] );
    
    $base_model -> remove_records( type => 'covariance' );
    
    $base_model -> remove_option( record_name => 'estimation',
				  option_name => 'MSFO',
				  problem_numbers => [1]);
    
    $prob_num = 1;
    
  } elsif( ( defined $msfo_names -> [0] and -e $msfo_names -> [0][0])
	   or
	   ( defined $msfi_names -> [0] and -e $msfi_names -> [0][0]) ){
    
    if( defined $msfo_names -> [0] and -e $msfo_names -> [0][0] ){
    }

    my $have_msfi;

    if( defined $msfi_names -> [0] and -e $msfi_names -> [0][0] ){
      $have_msfi = 1;
    }    

    unless( $have_msfi ){

      # If we end up here, we know we have msfo file and we need to
      # set the $MSFI record, remove inits and add mirror code like
      # above. And remove msfo?

      my $msfo_name = $msfo_names -> [0][0];

      $base_model -> set_records( type => 'msfi',
				  record_strings => [$msfo_name] );

      $base_model -> remove_records( type => 'theta' );
      $base_model -> remove_records( type => 'omega' );
      $base_model -> remove_records( type => 'sigma' );

      my $table_file_names = $base_model -> table_names( problem_numbers => [1] );    

      for( my $i; $i < @{$table_file_names -> [0]}; $i++ ){
				$table_file_names -> [0] -> [$i] =~ s/(.*)(\d+)(.*)/$1$2sim$3/;
      }
      
      $base_model -> table_names( new_names => $table_file_names,
				  problem_numbers => [1] );
      
      $base_model -> remove_records( type => 'covariance' );
      
      $base_model -> remove_option( record_name => 'estimation',
				    option_name => 'MSFO',
				    problem_numbers => [1]);      
      
    }
    
    $prob_num = 1;
    
  } else {
    
    my $problems = $#{$base_model->problems};
    
    my $sh_mod = model::shrinkage_module -> new ( nomegas => $base_model -> nomegas -> [0],
						  directory => $base_model -> directory(),
						  problem_number => ($problems+2) );


    $sh_mod->disable();

    $base_model->add_problem( init_data => { prob_arr => ['$PROB'], shrinkage_module => $sh_mod });

		$base_model->active_problems([]) unless defined $base_model->active_problems;
    push( @{$base_model->active_problems}, 1 );

		$base_model->datas([]) unless defined $base_model->datas;
    push( @{$base_model->datas}, $base_model->datas->[0] );

    # 1. Add msfo to $estimation

    my $estimation = $base_model -> record( record_name => 'estimation' );

    $base_model -> add_records( type => 'estimation',
				problem_numbers => [2],
				record_strings => $estimation -> [0] ); #will this add all $EST?

    $base_model -> remove_option( record_name => 'estimation',
				  option_name => 'MSFO',
				  problem_numbers => [2]);

    $base_model -> set_option( record_name => 'estimation',
			       option_name => 'MSFO',
			       option_value => 'msfo',
			       problem_numbers => [1] );
    
    # 2. Add $PROBLEM with copy of $INPUT
    
    my $input = $base_model -> record( record_name => 'input' );
    
    $base_model -> set_records( type => 'input',
				problem_numbers => [2],
				record_strings => $input -> [0] );
    
    my $data = $base_model -> record( record_name => 'data' );
    
    $base_model -> set_records( type => 'data',
				problem_numbers => [2],
				record_strings => $data -> [0] );
    
    $base_model -> set_option( record_name => 'data',
			       option_name => 'REWIND',
			       problem_numbers => [2] );
    
    # 3. Add $MSFI 
    
    $base_model -> set_records( type => 'msfi',
				problem_numbers => [2],
				record_strings => ['msfo'] );

    my $tables = $base_model -> record( record_name => 'table',
					problem_number => 1 );
    
    foreach my $table ( @{$tables} ){
      $base_model -> add_records( type => 'table',
				  problem_numbers => [2],
				  record_strings => $table );
    }

    my $table_file_names = $base_model -> table_names( problem_numbers => [2] );

    for( my $i; $i < @{$table_file_names -> [0]}; $i++ ){
      $table_file_names -> [0] -> [$i] =~ s/(.*)(\d+)(.*)/$1$2sim$3/;
    }
    
    my @dummy_array;
    $dummy_array[1] = $table_file_names -> [0];
    
    # The call to table_names below is quite horrid. To change the
    # table names of the second problem we must provide an array of
    # values for the first problem. Fortunately the
    # model::record::_option_val method accepts an empty array
    # because it shifts values from the array(which will be undef in
    # this case) and passes them to model::record::option -> value,
    # which then performs a no-op.
    #
    # Should any of that behaviour change, this brakes.
    #
    # Phew... glad to get that out of my system!
    
    $base_model -> table_names( new_names => [[],$table_file_names -> [0]],
				problem_numbers => [1,2] );
    
    $prob_num = 2;
    
  }

  # Below is common code for all three cases. The $prob_num variable
  # controls where modification is done.
  
  my $seed = random_uniform_integer(1,1,99999999);
  my $nr_of_mirrors = $this->nr_of_mirrors;
  
  if( $nr_of_mirrors < 2 ){
    ui->print( category => 'all',
	       message => 'Number of mirrorplots must be at least two, will run with two.' );
		$nr_of_mirrors = 2;
    $this->nr_of_mirrors($nr_of_mirrors);
  }
  
  $base_model -> set_records( type => 'simulation',
			      problem_numbers => [$prob_num],
			      record_strings => ["($seed) NSUB=$nr_of_mirrors"] );

  my $ok = $base_model -> set_maxeval_zero(problem_number => $prob_num,
					   need_ofv => 0,
					   print_warning => 1,
					   niter_eonly => $this->niter_eonly,
					   last_est_complete => $this->last_est_complete);
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
