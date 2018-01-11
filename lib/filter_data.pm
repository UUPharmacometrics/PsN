package filter_data;

# Package for filtering a NONMEM dataset according to model IGNORE

use strict;
use warnings;
use MooseX::Params::Validate;
use data;
use model;
use tool::modelfit;
use include_modules;

require Exporter;
our @ISA = qw(Exporter);
our %EXPORT_TAGS = ('all' => [ qw(filter_dataset) ]);
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

sub filter_dataset
{
	my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        force => { isa => 'bool', default => 0 },
	);
	my $model = $parm{'model'};
	my $force = $parm{'force'};

    if ($force or $model->need_data_filtering()) {

        my $preprocessdir = 'preprocess_data_dir';
        if (not -d $preprocessdir) {
            mkdir($preprocessdir);
        }

        my $filtered_data_model = $model->copy(
            filename => 'filter_data.mod',
            directory => $preprocessdir, 
            copy_datafile => 0,
            copy_output => 0,
            write_copy => 0,
            output_same_directory => 1,
        );
        my @filter_table_header;
        if (defined $filtered_data_model->problems->[0]->inputs and 
            defined $filtered_data_model->problems->[0]->inputs->[0]->options) {
            my ($arr, $time_added) = $filtered_data_model->problems->[0]->inputs->[0]->get_filter_table_names(); 
            croak("found no undropped data column in \$INPUT ") unless (defined $arr);
            croak("automatic filtering cannot yet handle \$INPUT with DATX but without TIME") if ($time_added);
            @filter_table_header = @{$arr};
        } else {
            die("Trying to construct table for filtering data".
                " but no headers were found in \$INPUT");
        }

        foreach my $remove_rec ('abbreviated', 'msfi', 'contr', 'subroutine', 'prior', 'model', 'tol', 'infn', 'omega',
            'pk','aesinitial','aes','des','error','pred','mix','theta','sigma','simulation','estimation','covariance',
            'nonparametric','table','scatter') {
            $filtered_data_model->remove_records(type => $remove_rec);
        }

        $filtered_data_model->add_records(type => 'pred', record_strings => ['Y=THETA(1)+ETA(1)+EPS(1)']);
        $filtered_data_model->add_records(type => 'theta', record_strings => ['1']);
        $filtered_data_model->add_records(type => 'omega', record_strings => ['1']);
        $filtered_data_model->add_records(type => 'sigma', record_strings => ['1']);
        $filtered_data_model->add_records(type => 'estimation', record_strings => ['MAXEVALS=0 METHOD=ZERO']);

        my $datafile = 'filtered.dta';
        $filtered_data_model->add_records( 
            type => 'table',
            record_strings => [ join(' ', @filter_table_header) .  ' NOAPPEND NOPRINT ONEHEADER FILE=' . $datafile ]
        );

        $filtered_data_model->_write();

        my $filter_fit = tool::modelfit->new(
            %{common_options::restore_options(@common_options::tool_options)},
            base_directory => $preprocessdir,
            directory => $preprocessdir.'/data_filtering_dir/',
            models => [ $filtered_data_model ],
            top_tool => 0,
            clean => 2);
        $filter_fit->run;

        my $adjusted_model = $model->copy(
            filename => 'adjusted_model.mod',
            directory => '.', 
            copy_datafile => 0,
            copy_output => 0,
            write_copy => 0,
            output_same_directory => 1,
        );

        $adjusted_model->remove_option(record_name => 'data', option_name => 'IGNORE', fuzzy_match => 1); 
        $adjusted_model->remove_option(record_name => 'data', option_name => 'ACCEPT', fuzzy_match => 1); 
        $adjusted_model->problems->[0]->datas->[0]->set_filename(filename => $datafile);
        $adjusted_model->problems->[0]->datas->[0]->ignoresign('@');

        return $adjusted_model;

    } else {
        return $model;
    }
}

1;
