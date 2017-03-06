package tool::linearize;

use Moose;
use MooseX::Params::Validate;
use File::Path;
use File::Copy 'cp';
use tool;
use tool::modelfit;
use tool::scm;

has 'epsilon' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'foce' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'error' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'full_block' => ( is => 'rw', isa => 'Bool' );      # Set to also run with full block OMEGA

has 'full_block_model' => ( is => 'rw', isa => 'model' );

extends 'tool';

sub BUILD
{
}

sub modelfit_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	my $model = $self->models->[$model_number - 1];

    my $lstfile;
    if (-e $model->outputs->[0]->full_name()){
        $lstfile = $model->outputs->[0]->full_name();
    }

    if ($model->is_option_set(record => 'abbreviated', name => 'REPLACE')) {
        print "\nWARNING: Option REPLACE used in \$ABBREVIATED. This can lead to serious errors.\n\n";
    }

    my @keep;
    foreach my $option (@{$model->problems->[0]->inputs->[0]->options()}){
        push (@keep, $option->name() ) if ( not ($option -> value eq 'DROP' or $option -> value eq 'SKIP'
                    or $option -> name eq 'DROP' or $option -> name eq 'SKIP'
                    or $option->name eq 'ID' or $option->name eq 'DV' 
                    or $option->name eq 'MDV'));
    }
    #set do not drop to everything undropped in model

    my $scm = tool::scm->new(
		%{common_options::restore_options(@common_options::tool_options)},
        clean => 2,
        models => [ $model ],
        epsilon => $self->epsilon,
        foce => $self->foce,
        do_not_drop => \@keep,
        lst_file => $lstfile,
        error => $self->error,
        search_direction => 'forward',
        linearize => 1,
        max_steps => 0,
        test_relations => {},
        categorical_covariates => [],
        continuous_covariates  => [],
        both_directions => 0,
        logfile => ['linlog.txt'],
    );

    $scm->run;

    #cleanup
    rmtree([ $scm->directory . 'm1' ]);
    rmtree([ $scm->directory . 'final_models' ]);
    unlink($scm->directory . 'covariate_statistics.txt');
    unlink($scm->directory . 'relations.txt');
    unlink($scm->directory . 'short_scmlog.txt');
    unlink($scm->directory . 'original.mod');
    unlink($scm->directory . 'base_model.mod');
    my @files = glob($scm->directory . $scm->basename . '*');
    for my $file (@files) {
        cp($file, '.');
    }

    cp($scm->basename . '.dta', '../' . $scm->basename . '.dta');
    cp($scm->basename . '.mod', '../' . $scm->basename . '.mod');

    if ($self->full_block) {
        my $name = $scm->basename;
        $name =~ s/linbase/full_block.mod/;
        my $full_block_model = $self->_create_full_block(input_model_name => $scm->basename . '.mod', output_model_name => "$name");
        $self->full_block_model($full_block_model);

        my $modelfit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
		    models => [ $full_block_model ], 
            base_dir => $self->directory . 'm1/',
            directory => undef,
            top_tool => 0,
            copy_data => 0,
        );
        $self->tools([]) unless defined $self->tools;
        push(@{$self->tools}, $modelfit);
    }
}

sub modelfit_analyze
{
    my $self = shift;

    # Print the ofv of the full block model
    if (defined $self->full_block_model) {
        my $ofv;
        if ($self->full_block_model->is_run()) {
            my $output = $self->full_block_model->outputs->[0];
            $ofv = $output->get_single_value(attribute => 'ofv');
        }
        if (defined $ofv) {
            $ofv = sprintf('%.5f', $ofv);
        } else {
            $ofv = 'NA';
        }
        print "\nThe ofv of the full omega block model:   $ofv   " . $self->full_block_model->filename . "\n";
        cp($self->full_block_model->full_name(), '..');
    }
}

sub _create_full_block
{
    my $self = shift;
    my %parm = validated_hash(\@_,
		input_model_name => { isa => 'Str' },
		output_model_name => { isa => 'Str' },
	);
	my $input_model_name = $parm{'input_model_name'};
	my $output_model_name = $parm{'output_model_name'};

    my $base = model->new(filename => $input_model_name);
    my $model = $base->copy(filename => "m1/$output_model_name");

    my $omega_matrix = $model->problems->[0]->get_filled_omega_matrix(start_eta => 1);
    my $size = @{$omega_matrix};
    my @record_arr = ( "\$OMEGA BLOCK($size)" );
    for (my $i = 0; $i < $size; $i++) {
        my $row = "";
        for (my $j = 0; $j <= $i; $j++) {
            $row .= $omega_matrix->[$i]->[$j] . ' ';
        }
        push @record_arr, "$row\n";
    }

    my $omega = model::problem::omega->new(record_arr => \@record_arr);
    $model->problems->[0]->omegas([ $omega ]);

    $model->_write;

    return $model;
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
