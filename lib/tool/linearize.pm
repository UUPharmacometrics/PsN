package tool::linearize;

use Mouse;
use MouseX::Params::Validate;
use File::Path;
use File::Copy 'copy';
use tool;
use tool::modelfit;
use tool::scm;
use model_approximations;

has 'epsilon' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'foce' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'error' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'keep_covariance' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'extra_table_columns' => ( is => 'rw', isa => 'ArrayRef[Str]' );    # Set to array of colnames to add to an extra data table output by derivatives.mod
has 'lst_file' => ( is => 'rw', isa => 'Str' );
has 'nointer' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'second_order' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'extra_data_columns' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );  # Columns to add to linbase.dta and $INPUT for linbase.mod
has 'estimation_options' => ( is => 'rw', isa => 'Str' );

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
            if (not ($option->value eq 'DROP' or $option->value eq 'SKIP'
                        or $option->name eq 'DROP' or $option->name eq 'SKIP'
                        or $option->name eq 'ID' or $option->name eq 'DV'
                        or $option->name eq 'MDV')) {
                if ($option->value eq 'TIME' or $option->name eq 'TIME') {      # Special case for TIME. Keep it instead of synonym
                    push @keep, 'TIME';
                } elsif ($option->value eq 'DATE' or $option->name eq 'DATE' or $option->value eq 'DAT1' or $option->name eq 'DAT1' or
                         $option->value eq 'DAT2' or $option->name eq 'DAT2' or $option->value eq 'DAT3' or $option->name eq 'DAT3') {
                    ;       # Don't add since NM-TRAN will aggregate DATE into TIME
                } else {
                    push @keep, $option->name;
                }
            }
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
            directory => 'scm_dir1',
            extra_table_columns => $self->extra_table_columns,
            nointer => $self->nointer,
            nm_output => $self->nm_output,
            use_data_format => 0,
            extra_data_columns => $self->extra_data_columns,
            estimation_options => $self->estimation_options,
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
            copy($file, '.');
        }

        copy($scm->basename . '.dta', '../' . $scm->basename . '.dta');
        copy($scm->basename . '.mod', '../' . $scm->basename . '.mod');
        copy($scm->basename . '.lst', '../' . $scm->basename . '.lst');
        copy($scm->basename . '.phi', '../' . $scm->basename . '.phi');
        copy($scm->basename . '.ext', '../' . $scm->basename . '.ext');
        copy($scm->basename . '.cov', '../' . $scm->basename . '.cov');
        copy($scm->basename . '.coi', '../' . $scm->basename . '.coi');
    } else {
        my $derivatives_model = model_approximations::second_order_derivatives_model(model => $model);
        if ($model->is_run()) {
            $derivatives_model->update_inits(from_output => $model->outputs->[0]);
        }
        my $derivatives_fit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
            base_directory => $self->directory,
            directory => $self->directory . '/derivatives_modelfit_dir/',
            models => [ $derivatives_model ],
            top_tool => 0,
        );
        $derivatives_fit->run();
        if (not $derivatives_model->have_output()) {
            croak("Derivatives model has no output. Terminating.");
        }
        my $derivatives_ofv = $derivatives_model->outputs->[0]->get_single_value(attribute => 'ofv');
        my $approximation_model = model_approximations::second_order_approximation_model(model => $model);
        my $approximation_fit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
            base_directory => $self->directory,
            directory => $self->directory . '/approximation_modelfit_dir/',
            models => [ $approximation_model ],
            top_tool => 0,
        );
        $approximation_fit->run();
        my $approximation_ofv = $approximation_model->outputs->[0]->get_single_value(attribute => 'ofv');

        my $initial_ofv;
        my $ofv_path = $approximation_model->outputs->[0]->get_single_value(attribute => 'ofvpath');
        if (defined $ofv_path) {
            $initial_ofv = $ofv_path->[0];
        }
        if (defined $initial_ofv) {
            $initial_ofv = sprintf("%12.5f", $initial_ofv);
        }

        print "\n";
        print "Base model OFV: " . $derivatives_ofv . "\n";
        print "Approximation model initial OFV: " . $initial_ofv . "\n";
        print "Approximation model estimated OFV: " . $approximation_ofv . "\n";
    }
}

1;
