package scm_util;

use strict;
use MouseX::Params::Validate;
require tool::scm;
require tool::scm::config_file;
require model;
use File::Copy qw(move mv copy);
use warnings;
require OSspecific;


sub setup
{
    # must be able to call from scmplus.pm using PsN and scm 4.7.0 with
    #scm_util::setup( common_options_hashref => $common_options_hashref,
    #                            config_file => $config_file,
    #                            options => $options,
    #                            model => $model,
    #                            scm_options_hashref => {both_directions => 0,
    #                                                   directory => $self->scm_top_directory,
    #                                                   logfile => [$self->directory().'scmlog.txt']},)

    my %parm = validated_hash(\@_,
                              config_file => {isa => 'tool::scm::config_file', optional => 0},
                              options => {isa => 'HashRef', optional => 0},
                              model => { isa => 'model',optional => 0},
                              common_options_hashref => {isa => 'HashRef', optional => 0},
                              scm_options_hashref => {isa => 'HashRef', optional => 1, default => {}},
        );
    my $config_file = $parm{'config_file'};
    my $options = $parm{'options'};
    my $model = $parm{'model'};
    my $common_options_hashref = $parm{'common_options_hashref'};
    my $scm_options_hashref = $parm{'scm_options_hashref'};

    my $direction = $config_file -> search_direction;
    die "You need to specify a search direction (forward/backward/both)\n" unless (defined $direction );

    my $scm;
    if( $direction eq 'forward' or $direction eq 'both' ){

        my ($orig_ofv,$orig_p_value,$ofv_backward,$p_backward) = setup_config_forward(config_file => $config_file,
                                                                                      options => $options);
        $scm = tool::scm->new ( %{$common_options_hashref},
                                models    => [$model],
                                base_criteria_values => get_base_criteria_values(options => $options),
                                config_file => $config_file,
                                both_directions => ($direction eq 'both')? 1 : 0,
                                p_backward => $p_backward,
                                ofv_backward => $ofv_backward,
                                %{$scm_options_hashref}
            );
    }else{

        setup_config_backward(config_file => $config_file,
                              options => $options);

        $scm = tool::scm->new ( %{$common_options_hashref},
                                    base_criteria_values => get_base_criteria_values(options => $options),
                                    models    => [$model],
                                    config_file => $config_file,
                                    %{$scm_options_hashref}
            );
    }

    return $scm;
}

sub copy_config_file{
    #call from scmplus bin script with PsN 4.7.0 and
    #scm_util::copy_config_file(options => \%options, directory => $asr->directory);

    my %parm = validated_hash(\@_,
                              options => {isa => 'HashRef', optional => 0},
                              directory => {isa => 'Str', optional => 0},
        );
    my $options = $parm{'options'};
    my $directory = $parm{'directory'};

    my ( $dir, $file ) = OSspecific::absolute_path('',$options->{'config_file'});
    copy($dir.$file,$directory.$file);
}

sub setup_model{
    # must be able to call from scmplus bin script using PsN 4.7.0 with
    #my $model = scm_util::setup_model(filename => $config_file->model,
    #                                options => \%options,
    #                                model_parameter_hashref => {eval common_options::model_parameters(\%options)});
    my %parm = validated_hash(\@_,
                              filename => {isa => 'Str', optional => 0},
                              options => {isa => 'HashRef', optional => 0},
                              model_parameter_hashref => {isa => 'HashRef', optional => 0},
        );
    my $filename = $parm{'filename'};
    my $options = $parm{'options'};
    my $model_parameter_hashref = $parm{'model_parameter_hashref'};

    my $model = model -> new ( %{$model_parameter_hashref},
                               filename => $filename);

    if( $options->{'shrinkage'} ) {
        $model-> shrinkage_stats( enabled => 1 );
    }

    if( $model-> is_option_set( record => 'abbreviated', name => 'REPLACE' ) ){
        print "\nWARNING: Option REPLACE used in \$ABBREVIATED. This can lead to serious errors.\n\n";
    }

    return $model;
}

sub check_options{
    # must be able to call from scmplus bin script using PsN 4.7.0 with
    # scm_util::check_options(scriptname => 'scmplus',
    #                                options => \%options,
    #                                config_file => $config_file,
    #                                require_model => 1)

    my %parm = validated_hash(\@_,
                              scriptname => {isa => 'Str', optional => 0},
                              options => {isa => 'HashRef', optional => 0},
                              require_model => {isa => 'Bool', default => 1},
                              config_file => {isa => 'tool::scm::config_file', optional => 0},
        );
    my $scriptname = $parm{'scriptname'};
    my $require_model = $parm{'require_model'};
    my $options = $parm{'options'};
    my $config_file = $parm{'config_file'};

    if (($scriptname eq 'scm') or ($scriptname eq 'scmplus')){
        if (defined $options->{'directory'} and -e $options->{'directory'}) {
            die "$scriptname cannot resume a previous run. Please change your -directory.\n";
        }
    }

    my $direction = $config_file -> search_direction;
    die "You need to specify a search direction (forward/backward/both)\n" unless (defined $direction );

    if ($scriptname eq 'scmplus'){
        unless (($direction eq 'forward') or ($direction eq 'both')){
            die(" scmplus does not support search_direction $direction ");
        }
        if ((defined $options->{'linearize'} and $options->{'linearize'}==1) ||
            (defined $config_file->linearize() and $config_file->linearize()==1)){
            die "$scriptname does not support option -linearize";
        }
    }


    if (defined $options->{'model'}) {    # Special case for model if not in config file. Are there others?
        $config_file->model($options->{'model'});
    }

    if ($require_model){
        if (not defined $config_file->model) {
            die "Error: No model specified in config file\n";
        }
    }

    if ($config_file->linearize){
        die "Option -second_order is currently broken"
            if $config_file->second_order();
        die "Cannot set option foce to 0, will crash.\n"
            if (defined $config_file->foce() and (not $config_file->foce));

        #two new options, linearize and lst-file

        if ($config_file->second_order()){
            print "Warning: Option -second_order is intended for use together with option -foce\n"
                unless $config_file->foce();
        }

        die "option -linearize only works with NONMEM7" unless ($PsN::nm_major_version == 7);

        if ($config_file->derivatives_data()){
            my ( $dir, $file ) = OSspecific::absolute_path('',$config_file->derivatives_data());
            $config_file->derivatives_data($dir . $file);
        }
        if ($config_file->lst_file()){
            my ( $dir, $file ) = OSspecific::absolute_path('',$config_file->lst_file());
            $config_file->lst_file($dir . $file);
        }

    }else{
        die "Option -foce is only allowed together with option -linearize"
            if $config_file->foce();
        die "Option -second_order is only allowed together with option -linearize"
            if $config_file->second_order();
        die "Option -lst_file is only allowed together with option -linearize"
            if $config_file->lst_file();
        die "Option -update_derivatives is only allowed together with option -linearize"
            if $config_file->update_derivatives();
        die "Option -error is only allowed together with option -linearize"
            if $config_file->error();
        die "Option -error_code is only allowed together with option -linearize"
            if $config_file->error_code();
        die "Option -derivatives_data is only allowed together with option -linearize"
            if $config_file->derivatives_data();
    }




}

sub get_base_criteria_values
{
    my %parm = validated_hash(\@_,
                              options => {isa => 'HashRef', optional => 0},
        );
    my $options = $parm{'options'};

    my %base_criteria_values=();
    if (defined $options->{'base_ofv'}){
        $base_criteria_values{'ofv'}=$options->{'base_ofv'};
    }
    return \%base_criteria_values;
}

sub setup_config_backward
{
    my %parm = validated_hash(\@_,
                              config_file => {isa => 'tool::scm::config_file', optional => 0},
                              options => {isa => 'HashRef', optional => 0}
        );
    my $config_file = $parm{'config_file'};
    my $options = $parm{'options'};

    if( defined $config_file -> ofv_backward ){
        $config_file -> ofv_change( $config_file -> ofv_backward );
    }

    if( defined $config_file -> p_backward ){
        $config_file -> p_value( $config_file -> p_backward );
    }

    $config_file -> search_direction( 'backward' );

}

sub setup_config_forward
{
    my %parm = validated_hash(\@_,
                              config_file => {isa => 'tool::scm::config_file', optional => 0},
                              options => {isa => 'HashRef', optional => 0}
        );
    my $config_file = $parm{'config_file'};
    my $options = $parm{'options'};

    $config_file -> search_direction( 'forward' );

    my ($orig_ofv,$orig_p_value);
    my $ofv_backward = undef;
    my $p_backward = undef;


    if( defined $config_file -> ofv_forward ){
        $orig_ofv = $config_file -> ofv_change;
        $config_file -> ofv_change($config_file -> ofv_forward);
    }

    if( defined $config_file -> p_forward ){
        $orig_p_value = $config_file -> p_value;
        $config_file -> p_value( $config_file -> p_forward );
    }

    if( defined $config_file -> ofv_backward ){
        $ofv_backward = $config_file -> ofv_backward ;
    }elsif (defined $orig_ofv){
        $ofv_backward = $orig_ofv;
    }

    if( defined $config_file -> p_backward ){
        $p_backward = $config_file -> p_backward;
    }elsif (defined $orig_p_value){
        $p_backward = $orig_p_value;
    }


    return ($orig_ofv,$orig_p_value,$ofv_backward,$p_backward);


}

sub get_config_object
{
    # must be able to call from scmplus bin script using PsN 4.7.0 with
    #my $config_file = scm_util::get_config_object(options => \%options,common_tool_options =>\@common_options::tool_options);

    my %parm = validated_hash(\@_,
                              options => {isa => 'HashRef', optional => 0},
                              common_tool_options => {isa => 'ArrayRef', optional => 0},
        );
    my $options = $parm{'options'};
    my $common_tool_options = $parm{'common_tool_options'};

    unless (defined $options->{'config_file'}){
        print "Please specify a config file \n";
        die;
    }

    if ( $options->{'config_file'} eq ''){
        print "Please specify a config file \n";
        die;
    }

    unless( -e $options->{'config_file'} ){
        print "Error: config file ".$options->{'config_file'}." is missing.\n" ;
        die;
    }
    my $file = file -> new( name => $options->{'config_file'}, path => '.' );
    my $config_file = 'tool::scm::config_file' -> new ( file => $file );

    foreach my $option ( keys %{$config_file -> valid_scalar_options} ){
        if( defined $options->{$option} ) {
            $config_file -> $option($options->{$option});
        }elsif (defined $config_file -> $option){
            #store tool_options so that can use common_options::restore in scm
            foreach my $opt (@{$common_tool_options}){
                $opt =~ s/[!:|].*//g; #get rid of :s |? :i etcetera
                if ($opt eq $option){
                    $options->{$option} = $config_file -> $option;
                    last;
                }
            }
        }
    }

    foreach my $option ( keys %{$config_file -> valid_code_options} ){
        if( $options->{$option} ){
            $config_file -> $option(eval($options->{$option}));
        }
    }

    foreach my $option ( keys %{$config_file -> valid_array_options} ){
        if( $options->{$option} ){
            my @arr = split( /,/ , $options->{$option});
            $config_file -> $option(\@arr);
        }
    }


    return $config_file;
}

1;
