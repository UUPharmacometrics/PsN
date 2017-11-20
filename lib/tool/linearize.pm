package tool::linearize;

use Moose;
use MooseX::Params::Validate;
use File::Path;
use File::Copy 'cp';
use tool;
use tool::modelfit;
use tool::scm;
use model_approximations;

has 'epsilon' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'foce' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'error' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'keep_covariance' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'estimate_fo' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'extra_table_columns' => ( is => 'rw', isa => 'ArrayRef[Str]' );    # Set to array of colnames to add to an extra data table output by derivatives.mod
has 'lst_file' => ( is => 'rw', isa => 'Str' );
has 'nointer' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'second_order' => ( is => 'rw', isa => 'Bool', default => 0 );

has 'dataname' => ( is => 'rw', isa => 'Str' );

extends 'tool';

sub modelfit_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
		model_number => { isa => 'Int', optional => 1 }
	);
	my $model_number = $parm{'model_number'};

	my $model = $self->models->[$model_number - 1];

    if (not $self->second_order) {
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
            lst_file => $self->lst_file,
            error => $self->error,
            search_direction => 'forward',
            linearize => 1,
            max_steps => 0,
            test_relations => {},
            categorical_covariates => [],
            continuous_covariates  => [],
            both_directions => 0,
            logfile => ['linlog.txt'],
            from_linearize => 1,
            keep_covariance => $self->keep_covariance,
            estimate_fo => $self->estimate_fo,
            directory => 'scm_dir1',
            extra_table_columns => $self->extra_table_columns,
            nointer => $self->nointer,
            nm_output => $self->nm_output,
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
        cp($scm->basename . '.lst', '../' . $scm->basename . '.lst');
        cp($scm->basename . '.phi', '../' . $scm->basename . '.phi');
        cp($scm->basename . '.ext', '../' . $scm->basename . '.ext');
        cp($scm->basename . '.cov', '../' . $scm->basename . '.cov');
        cp($scm->basename . '.coi', '../' . $scm->basename . '.coi');
    } else {
        my $derivatives_model = model_approximations::second_order_derivatives_model(model => $model);
        my $derivatives_fit = tool::modelfit -> new(
            %{common_options::restore_options(@common_options::tool_options)},
            base_directory => $self->directory,
            directory => $self->directory.'/derivatives_modelfit_dir/',
            models => [ $derivatives_model ],
            top_tool => 0);
        $derivatives_fit->run();
    }
}

sub modelfit_analyze
{
    my $self = shift;
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
