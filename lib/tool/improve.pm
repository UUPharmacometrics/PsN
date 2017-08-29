package tool::improve;

use strict;
use Moose;
use MooseX::Params::Validate;
use File::Copy 'cp';
use include_modules;
use log;
use tool::qa;
use PsN;
use YAML;
use model_transformations;

extends 'tool';

has 'model' => ( is => 'rw', isa => 'model' );
has 'groups' => ( is => 'rw', isa => 'Int', default => 10 );       # The number of groups to use for quantiles in the time_varying model
has 'idv' => ( is => 'rw', isa => 'Str', default => 'TIME' );
has 'dv' => ( is => 'rw', isa => 'Str', default => 'CWRES' );
has 'dvid' => ( is => 'rw', isa => 'Str', default => 'DVID' );
has 'occ' => ( is => 'rw', isa => 'Str', default => 'OCC' );
has 'covariates' => ( is => 'rw', isa => 'Str' );       # A comma separated list of continuous covariate symbols
has 'categorical' => ( is => 'rw', isa => 'Str' );       # A comma separated list of categorical covariate symbols
has 'parameters' => ( is => 'rw', isa => 'Str' );       # A comma separated list of parameter symbols
has 'fo' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'lst_file' => ( is => 'rw', isa => 'Str' );
has 'cmd_line' => ( is => 'rw', isa => 'Str' );         # Used as a work around for calling scm via system
has 'nointer' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'nonlinear' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'skip' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'only' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );    # Will be transformed into skip in BUILD


sub BUILD
{
    my $self = shift;

}

sub modelfit_setup
{
	my $self = shift;

    my $qa = tool::qa->new(
        eval( $common_options::parameters ),
        models => $self->models,
        dvid => $self->dvid,
        idv => $self->idv,
        dv => $self->dv,
        occ => $self->occ,
        groups => $self->groups,
        covariates => $self->covariates,
        categorical => $self->categorical,
        parameters => $self->parameters,
        cmd_line => $self->cmd_line,
        fo => $self->fo,
        lst_file => $self->lst_file,
        nointer => $self->nointer,
        nonlinear => $self->nonlinear,
        skip => $self->skip,
        only => $self->only,
        template_file_rplots => 'improve_qa.R',
        template_directory_rplots => $PsN::Rscripts_dir,
        top_tool => 1,
		rplots => 1,
    );

    $qa->run();
    $qa->print_results();

	my $results = YAML::LoadFile('qa_dir1/results.yaml');

	my $choice = $results->{'choice'};
	if ($choice eq 'boxcox') {
		model_transformations::boxcox_etas(model => $self->models->[0]);
	} else {
		model_transformations::full_omega_block(model => $self->models->[0]);
	}

    my $final_qa = tool::qa->new(
        eval( $common_options::parameters ),
        models => $self->models,
        dvid => $self->dvid,
        idv => $self->idv,
        dv => $self->dv,
        occ => $self->occ,
        groups => $self->groups,
        covariates => $self->covariates,
        categorical => $self->categorical,
        parameters => $self->parameters,
        cmd_line => $self->cmd_line,
        fo => $self->fo,
        lst_file => $self->lst_file,
        nointer => $self->nointer,
        nonlinear => $self->nonlinear,
        skip => $self->skip,
        only => $self->only,
        template_file_rplots => 'qa_default.Rmd',
        template_directory_rplots => $PsN::Rscripts_dir,
        top_tool => 1,
		rplots => 1,
		directory => 'final_qa',
    );

    $final_qa->run();
    $final_qa->print_results();
}

sub modelfit_analyze
{
    my $self = shift;
}

sub create_R_plots_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
        rplot => { isa => 'rplots', optional => 0 }
    );
	my $rplot = $parm{'rplot'};

	$rplot->pdf_title('Improve');

    $rplot->add_preamble(
        code => [
            '# improve specific preamble',
        ]
    );
}


no Moose;
__PACKAGE__->meta->make_immutable;
1;
