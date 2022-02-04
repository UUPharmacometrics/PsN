package tool::xv;

use include_modules;
use tool::xv_step;
use Mouse;
use MouseX::Params::Validate;

extends 'tool';

has 'xv_steps' => ( is => 'rw', isa => 'ArrayRef[xv_step]' );
has 'subtools' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['xv_step'] } );
has 'warnings' => ( is => 'rw', isa => 'Int', default => 0 );
has 'step_counter' => ( is => 'rw', isa => 'Int', default => 1 );

sub BUILD
{
    my $self  = shift;

    my $model;
    $model = $self->models->[0];

}

sub add_xv_step
{
    my ($self, %parm) = validated_hash(@_,
        init_data => {isa => 'Any', optional => 0}
    );
    $self->xv_steps([]) unless defined $self->xv_steps;
    push( @{$self->xv_steps}, xv_step->new( %{$parm{'init_data'}} ) );
}

sub xv_step_pre_fork_setup
{
    my $self = shift;

    my $subtools = undef;
    if( scalar @{$self -> subtools} > 1 ){
        my @subtools = @{$self -> subtools};
        shift( @subtools );
        $subtools = \@subtools;
    }

    my %step_args;
    if (defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'xv_step'}){
        %step_args = %{$self -> subtool_arguments -> {'xv_step'}};
    }

    my $dir = 'xv_step_dir'.$self->step_counter;
    $self->step_counter($self->step_counter+1);
    my $xv_step = tool::xv_step -> new( models => [$self -> models -> [0]],
                                        subtools => $subtools,
                                        %step_args,
                                        directory => $dir,
                                        subtool_arguments => $self->subtool_arguments);

    $xv_step -> create_data_sets;
    $self -> xv_steps([]) unless (defined $self -> xv_steps);
    push( @{$self -> xv_steps}, $xv_step );
}

sub xv_step_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 0 }
    );
    my $model_number = $parm{'model_number'};

    unless( $model_number == 1 ){
        my $subtools = undef;
        if( scalar @{$self -> subtools} > 1 ){
            my @subtools = @{$self -> subtools};
            shift( @subtools );
            $subtools = \@subtools;
        }

        my %step_args;
        if (defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'xv_step'}){
            %step_args = %{$self -> subtool_arguments -> {'xv_step'}};
        }

        my $first_xv_step = $self -> xv_steps -> [0];
        my $xv_step = tool::xv_step -> new( models => [$self -> models -> [$model_number - 1]],
            prediction_data => $first_xv_step -> prediction_data,
            estimation_data => $first_xv_step -> estimation_data,
            stratify_on => $first_xv_step -> stratify_on,
            subtools => $subtools,
            %step_args,
            subtool_arguments => $self -> subtool_arguments);

        $self -> xv_steps([]) unless (defined $self -> xv_steps);
        push( @{$self -> xv_steps}, $xv_step );
    }

    $self -> tools([$self -> xv_steps -> [$model_number-1]]);
}

sub xv_step_post_subtool_analyze
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Maybe[Int]', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    $model_number=0 unless (defined $model_number); #FIXME! do not call without model number

    my $subtools = undef;
    if( scalar @{$self -> subtools} > 1 ){
        my @subtools = @{$self -> subtools};
        shift( @subtools );
        $subtools = \@subtools;
    }
    my $newwarn = $self->warnings() + $self -> xv_steps -> [$model_number - 1] ->warnings;
    $self->warnings($newwarn);
    my $first_xv_step = $self -> xv_steps -> [0];
    if( $self -> xv_steps -> [$model_number - 1] -> cont ){

        my %step_args;
        if (defined $self -> subtool_arguments and defined $self -> subtool_arguments -> {'xv_step'}){
            %step_args = %{$self -> subtool_arguments -> {'xv_step'}};
        }
        my $dir = 'xv_step_dir'.$self->step_counter;
        $self->step_counter($self->step_counter+1);

        $self -> xv_steps -> [$model_number -1] =
            tool::xv_step -> new( models => [$self -> models -> [$model_number - 1]],
                                  prediction_data => $first_xv_step -> prediction_data,
                                  estimation_data => $first_xv_step -> estimation_data,
                                  stratify_on => $first_xv_step -> stratify_on,
                                  subtools => $subtools,
                                  %step_args,
                                  directory => $dir,
                                  subtool_arguments => $self->subtool_arguments );

        $self->tools([]) unless (defined $self->tools);
        push( @{$self -> tools}, $self -> xv_steps -> [$model_number-1] );
    }
}

1;
