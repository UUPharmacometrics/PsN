package tool::benchmark;

use include_modules;
use strict;
use data;
use nonmemrun;
use OSspecific;
use tool::modelfit;
use Mouse;
use MouseX::Params::Validate;
use output;
use utils::file;
use array qw(get_array_positions);
use math qw(round);

extends 'tool';

has 'benchmark_raw_results' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['benchmarklog.csv'] } );
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'benchmark_results.csv' );

has 'merge_rawresults' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'alt_nonmem' => ( is => 'rw', isa => 'ArrayRef' , default => sub { [] });
has 'reference_lst' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] });
has 'record_change_list' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'theta_change_list' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'record_options' => ( is => 'rw', isa => 'Str' );
has 'theta_inits' => ( is => 'rw', isa => 'Str' );
has 'dofv_threshold' => ( is => 'rw', isa => 'Num', default => 1 );
has 'replicates' => ( is => 'rw', isa => 'Int', default => 1 );
has 'parameter_threshold' => ( is => 'rw', isa => 'Num', default => 5 );
has 'nonmem_paths' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'full_rawres_headers' => ( is => 'rw', isa => 'ArrayRef' , default => sub { [] });
has 'settings_header' => ( is => 'rw', isa => 'ArrayRef' , default => sub { [] });
has 'copy_data' => ( is => 'rw', isa => 'Bool', default => 1 );
has '_reference_raw_results' => ( is => 'rw', isa => 'ArrayRef' );

sub BUILD
{
    my $self  = shift;

    for my $accessor ('logfile','raw_nonp_file'){
        my @new_files=();
        my @old_files = @{$self->$accessor};
        for (my $i=0; $i < scalar(@old_files); $i++){
            my $name;
            my $ldir;
            ( $ldir, $name ) =
                OSspecific::absolute_path( $self ->directory(), $old_files[$i] );
            push(@new_files,$ldir.$name) ;
        }
        $self->$accessor(\@new_files);
    }

    croak("dofv_threshold must not be negative") if ($self->dofv_threshold < 0);
    croak("parameter_threshold must not be negative") if ($self->parameter_threshold < 0);
    croak("replicates must be larger than 0") unless ($self->replicates > 0);

    foreach my $version (@{$self->alt_nonmem}){
        #this will croak in nonmemrun if version is not valid
        #each item is hashref with keys 'full_path_runscript','full_path_nmtran'

        push(@{$self->nonmem_paths},nonmemrun::setup_paths(nm_version => $version));
    }

    if (defined $self->record_options){
        $self->record_change_list(parse_record_options(record_options => $self->record_options));
    }
    if (defined $self->theta_inits){
        $self->theta_change_list(parse_theta_inits(theta_inits => $self->theta_inits, models => $self->models));
    }

    my $ref = $self->parse_reference_lst();
    $self->_reference_raw_results($ref);
}

sub parse_reference_lst
{
    my $self = shift;
    my @raw_results = ();

    for my $lst_name (@{$self->reference_lst}) {
        my $output = output->new(filename => $lst_name);
        my $model = $output->lst_model;
        $model->outputs->[0] = $output;
        my $modelfit = tool::modelfit->new(models => [ $model]);
        my ($raw_results_row, $nonp_row) = $modelfit->create_raw_results_rows(
            max_hash => $modelfit->max_hash,
            model => $modelfit->models->[0],
            model_number => 1,
            raw_line_structure => $modelfit->raw_line_structure
        );
        push @raw_results, $raw_results_row->[0];
    }

    return \@raw_results;
}


sub parse_theta_inits
{
    my %parm = validated_hash(\@_,
                              theta_inits => { isa => 'Str', optional => 0 },
                              models => { isa => 'ArrayRef', optional => 0 },
    );
    my $theta_inits = $parm{'theta_inits'};
    my $models = $parm{'models'};

    my @list=();
    my @items = split(',,',$theta_inits); #first split on double comma

    my @thetacounts=();
    foreach my $model (@{$models}){
        croak("model ".$model->filename." has no problems") unless (defined $model->problems);
        my $count = 0;
        foreach my $prob (@{$model->problems}){
            if (defined $prob->thetas and scalar(@{$prob->thetas})>0){
                $count = $prob->record_count(record_name => 'theta');
                last;
            }

        }
        push(@thetacounts,$count);
    }


    foreach my $item (@items){
        unless ($item =~ /:/){
            croak("Error parsing record_options item $item (no colon : found)");
        }
        #then split on :
        my @leftright = split(':',$item);
        if (scalar(@leftright) < 2){
            croak("Error parsing theta_inits item $item (missing theta identifier or inits list?)");
        }
        if (scalar(@leftright) > 2){
            croak("Error parsing theta_inits item $item (too many colon : found. Did you forget to use double comma ,, between items?)");
        }
        if (length($leftright[0])<1){
            croak("Error parsing theta_inits item $item (missing theta identifier?)");
        }
        if (length($leftright[1])<1){
            croak("Error parsing theta_inits item $item (missing inits list?)");
        }
        #if left hand is a number, check that models have that many thetas
        if ($leftright[0] =~ /^(\d+)$/){
            my $num = $1;
            if ($num < 1){
                croak ("Error parsing theta_inits item $item: Theta number must be larger than 0 but is $num");
            }
            for (my $i=0; $i< scalar(@{$models}); $i++){
                if ($num > $thetacounts[$i]){
                    croak("Error parsing theta_inits item $item: Cannot set theta number $num because model ".
                          $models->[$i]->filename." only has ".$thetacounts[$i]." thetas. Use labels to add new thetas to models");
                }
            }
        }

        #split right hand on comma, check that at least one element
        #check each item in right hand that there are no duplicates
        my @opts = split(',',$leftright[1]);
        unless (scalar(@opts)>0){
            croak("Error parsing theta_inits item $item: no inits found");
        }

        my %defined;
        foreach my $opt (@opts){
            if (defined $defined{$opt} and ($defined{$opt} ==1)){
                croak("duplicate setting of $opt in $item");
            }
            $defined{$opt} =1;
        }
        #left hand is key of hash
        #value in hash is arrayref
        #push hashref to @list
        push (@list,{$leftright[0] => \@opts});
    }
    return \@list;

}

sub parse_record_options
{
    my %parm = validated_hash(\@_,
        record_options => { isa => 'Str', optional => 0 }
    );
    my $record_options = $parm{'record_options'};

    my @list=();
    my @items = split(',,',$record_options); #first split on double comma
    my $testprob = model::problem->new(ignore_missing_files=> 1,
                                       prob_arr       => ['$PROB','$INPUT ID','$DATA dummy.txt']);

    foreach my $item (@items){
        unless ($item =~ /:/){
            croak("Error parsing record_options item $item (no colon : found)");
        }
        #then split on :
        my @leftright = split(':',$item);
        if (scalar(@leftright) < 2){
            croak("Error parsing record_options item $item (missing record name or options list?)");
        }
        if (scalar(@leftright) > 2){
            croak("Error parsing record_options item $item (too many colon : found. Did you forget to use double comma ,, between items?)");
        }
        #check that left hand is a record. problem _normalize_record_name will croak if it is not
        my $recname = $testprob -> _normalize_record_name(record_name => $leftright[0]);

        #split right hand on comma, check that at least one element
        #check each item in right hand that there are no duplicates
        my @opts = split(',',$leftright[1]);
        unless (scalar(@opts)>0){
            croak("Error parsing record_options item $item: no options found");
        }

        my %defined;
        foreach my $opt (@opts){
            if ((defined $defined{$opt}) and $defined{$opt} ==1){
                croak("duplicate setting of $opt in $item");
            }
            $defined{$opt} =1;
        }
        #left hand is key of hash
        #value in hash is arrayref
        #push hashref to @list
        push (@list,{$recname => \@opts});
    }
    return \@list;

}

sub modify_model
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              record => { isa => 'Str', optional => 0 },
                              record_number => { isa => 'Int', optional => 1, default => 1 },
                              option => { isa => 'Str', optional => 0 },
    );
    my $model = $parm{'model'};
    my $record = $parm{'record'};
    my $record_number = $parm{'record_number'};
    my $option = $parm{'option'};

    #record number is default 1. Assumption is that if want to vary e.g. settings in $EST then
    #it will be first $EST, while second $EST if any is just EONLY or similar


    return if ($option eq 'none');
    my $optionvalue=undef;
    if ($option =~ /=/){
        $option =~s/=(.*)$//;
        $optionvalue = $1;
    }
    my $accessor = $record.'s';
    if ((defined $model->problems->[0]->$accessor) and
        scalar(@{$model->problems->[0]->$accessor})>0){
        $model->set_option(record_name => $record,
                           record_number => $record_number,
                           option_name => $option,
                           option_value => $optionvalue,
                           fuzzy_match => 1);
    }else{
        $model->add_option(record_name => $record,
                           record_number => $record_number,
                           option_name => $option,
                           option_value => $optionvalue,
                           add_record => 1);

    }

}

sub modify_model_theta
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              theta => { isa => 'Str', optional => 0 },
                              init => { isa => 'Str', optional => 0 },
    );
    my $model = $parm{'model'};
    my $theta = $parm{'theta'};
    my $init = $parm{'init'};

    return if ($init eq 'none');
    for (my $prob=0; $prob< scalar(@{$model->problems}); $prob++){
        #modify first prob that has any thetas
        if ((defined $model->problems->[$prob]->thetas) and
            scalar(@{$model->problems->[$prob]->thetas})>0){
            if ($theta =~ /^(\d+)$/){
                #theta number
                my $num = $1;
                my $count = 0;
                for (my $j=0; $j< scalar(@{$model->problems->[$prob]->thetas}); $j++){
                    for (my $i=0; $i< scalar(@{$model->problems->[$prob]->thetas->[$j]->options}); $i++){
                        $count++;
                        if ($num == $count){
                            my ($success,$dirt1,$dirt2)=
                                $model->problems->[$prob]->thetas->[$j]->options->[$i]->check_and_set_init (new_value => $init);
                            croak("Could not set init $init in theta $theta of ".$model->filename) unless ($success);
                            last;
                        }
                    }
                    last if ($num == $count);
                }
                croak("could not find theta $num") unless ($num == $count);
            }else{
                #theta label
                my $found = 0;
                for (my $j=0; $j< scalar(@{$model->problems->[$prob]->thetas}); $j++){
                    foreach my $opt (@{$model->problems->[$prob]->thetas->[$j]->options}){
                        if (defined $opt->label and ($opt->label eq $theta)){
                            my ($success,$dirt1,$dirt2)= $opt -> check_and_set_init(new_value=>$init);
                            croak("Could not set init $init in theta $theta of ".$model->filename) unless ($success);
                            $found =1;
                            last;
                        }
                    }
                    last if ($found);
                }
                unless ($found){
                    #add a theta
                    $model->problems->[$prob]->add_records( type => 'theta',
                                                            record_strings => [$init.' ; '.$theta] );

                }

            }
            last; #found prob with thetas
        }
    }

}

sub create_nonmem_alt_models
{
    my %parm = validated_hash(\@_,
                              model_lists => { isa => 'ArrayRef', optional => 0 },
                              alt_nonmem => { isa => 'ArrayRef', optional => 0 },
                              nm_version => { isa => 'Str', optional => 0 },
    );
    my $model_lists = $parm{'model_lists'};
    my $alt_nonmem = $parm{'alt_nonmem'};
    my $nm_version = $parm{'nm_version'};

    my $listcount = scalar(@{$model_lists});
    my $mcount = scalar(@{$model_lists->[0]});

    for (my $i=0; $i< scalar(@{$alt_nonmem}); $i++){
        #loop over model lists mi
        for (my $mi=0; $mi< $listcount; $mi++){
            push(@{$model_lists},[]);
            for (my $j=0; $j< $mcount; $j++){
                my $newname = get_modified_filename(model => $model_lists->[$mi]->[$j],
                                                    nm_version => $alt_nonmem->[$i]);
                push(@{$model_lists->[-1]},$model_lists->[$mi]->[$j]->copy( filename => $newname,
                                                                            directory=> $model_lists->[$mi]->[$j]->directory,
                                                                            write_copy => 0,
                                                                            output_same_directory => 1,
                                                                            copy_output => 0));
            }
        }
    }
    #finally modify model names for original nm_version
    for (my $mi=0; $mi< $listcount; $mi++){
        for (my $j=0; $j< $mcount; $j++){
            my $newname = get_modified_filename(model => $model_lists->[$mi]->[$j],
                                                nm_version => $nm_version);
            $model_lists->[$mi]->[$j]->filename($newname);
        }
    }

}

sub create_replicate_models
{
    my %parm = validated_hash(\@_,
                              model_lists => { isa => 'ArrayRef', optional => 0 },
                              replicates => { isa => 'Int', optional => 0 },
    );
    my $model_lists = $parm{'model_lists'};
    my $replicates = $parm{'replicates'};

    my $listcount = scalar(@{$model_lists}); #this will not change, but all will be extended to length*replicates
    my $mcount = scalar(@{$model_lists->[0]});

    for (my $i=0; $i< $listcount; $i++){
        for (my $j=2; $j<= $replicates; $j++){
            for (my $mi=0; $mi< $mcount; $mi++){
                my $newname = get_modified_filename(model => $model_lists->[$i]->[$mi],
                                                    replicate => $j);
                push(@{$model_lists->[$i]},$model_lists->[$i]->[$mi]->copy( filename => $newname,
                                                                            directory=> $model_lists->[$i]->[$mi]->directory,
                                                                            write_copy => 0,
                                                                            output_same_directory => 1,
                                                                            copy_output => 0));
            }
        }
        #finally modify names for replicate 1
        for (my $mi=0; $mi< $mcount; $mi++){
            my $newname = get_modified_filename(model => $model_lists->[$i]->[$mi],
                                                replicate => 1);
            $model_lists->[$i]->[$mi]->filename($newname);

        }

    }

}

sub create_record_variant_models
{
    my %parm = validated_hash(\@_,
                              model_lists => { isa => 'ArrayRef', optional => 0 },
                              change_list => { isa => 'ArrayRef', optional => 0 },
    );
    my $model_lists = $parm{'model_lists'};
    my $change_list = $parm{'change_list'};

    #loop over model lists mi
    for (my $mi=0; $mi< scalar(@{$model_lists}); $mi++){
        #loop over change list ci
        for (my $ci=0; $ci < scalar(@{$change_list}); $ci++){
            #count models mcount in list mi before making change ci
            my $mcount = scalar(@{$model_lists->[$mi]});
            my @keys = keys %{$change_list->[$ci]}; #always only one key
            my $recordname = $keys[0];

            my $optcount = scalar(@{$change_list->[$ci]->{$recordname}}); #ref of array of opts
            for (my $op=1; $op < $optcount; $op++){
                #for each option *in addition to* the first required one in the option list, make a copy of the
                #mcount first models in list mi at the start of this iteration. push copies to
                #model_list mi, write_copy is false, output_same_directory true
                for (my $i=0; $i< $mcount; $i++){
                    my $newname = get_modified_filename(model => $model_lists->[$mi]->[$i],
                                                        option => $change_list->[$ci]->{$recordname}->[$op]);
                    push(@{$model_lists->[$mi]},$model_lists->[$mi]->[$i]->copy( filename => $newname,
                                                                                 directory => $model_lists->[$mi]->[$i]->directory,
                                                                                 write_copy => 0,
                                                                                 output_same_directory => 1,
                                                                                 copy_output => 0));
                    modify_model(model => $model_lists->[$mi]->[-1],
                                 record => $recordname,
                                 option => $change_list->[$ci]->{$recordname}->[$op]);
                }
            }
            #now we only have the first option in list to set in original models
            my $op = 0;
            #set option op in mcount first models
            for (my $i=0; $i< $mcount; $i++){
                my $newname = get_modified_filename(model => $model_lists->[$mi]->[$i],
                                                    option => $change_list->[$ci]->{$recordname}->[$op]);
                $model_lists->[$mi]->[$i]->filename($newname);
                modify_model(model => $model_lists->[$mi]->[$i],
                             record => $recordname,
                             option => $change_list->[$ci]->{$recordname}->[$op]);

            }
        }
    }

}

sub create_theta_variant_models
{
    my %parm = validated_hash(\@_,
                              model_lists => { isa => 'ArrayRef', optional => 0 },
                              theta_list => { isa => 'ArrayRef', optional => 0 },
    );
    my $model_lists = $parm{'model_lists'};
    my $theta_list = $parm{'theta_list'};

    #loop over model lists mi
    for (my $mi=0; $mi< scalar(@{$model_lists}); $mi++){
        #loop over theta list ci
        for (my $ci=0; $ci < scalar(@{$theta_list}); $ci++){
            #count models mcount in list mi before making change ci
            my $mcount = scalar(@{$model_lists->[$mi]});
            my @keys = keys %{$theta_list->[$ci]}; #always only one key
            my $thetaname = $keys[0];

            my $optcount = scalar(@{$theta_list->[$ci]->{$thetaname}}); #ref of array of values
            for (my $op=1; $op < $optcount; $op++){
                #for each option *in addition to* the first required one in the option list, make a copy of the
                #mcount first models in list mi at the start of this iteration. push copies to
                #model_list mi, write_copy is false, output_same_directory true
                for (my $i=0; $i< $mcount; $i++){
                    my $newname = get_modified_filename(model => $model_lists->[$mi]->[$i],
                                                        option => $theta_list->[$ci]->{$thetaname}->[$op]);
                    push(@{$model_lists->[$mi]},$model_lists->[$mi]->[$i]->copy( filename => $newname,
                                                                                 directory => $model_lists->[$mi]->[$i]->directory,
                                                                                 write_copy => 0,
                                                                                 output_same_directory => 1,
                                                                                 copy_output => 0));
                    modify_model_theta(model => $model_lists->[$mi]->[-1],
                                       theta => $thetaname,
                                       init => $theta_list->[$ci]->{$thetaname}->[$op]);
                }
            }
            #now we only have the first option in list to set in original models
            my $op = 0;
            #set option op in mcount first models
            for (my $i=0; $i< $mcount; $i++){
                my $newname = get_modified_filename(model => $model_lists->[$mi]->[$i],
                                                    option => $theta_list->[$ci]->{$thetaname}->[$op]);
                $model_lists->[$mi]->[$i]->filename($newname);
                modify_model_theta(model => $model_lists->[$mi]->[$i],
                                   theta => $thetaname,
                                   init => $theta_list->[$ci]->{$thetaname}->[$op]);

            }
        }
    }

}

sub get_modified_filename
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              clean => { isa => 'Bool', optional => 1,default=>0  },
                              option => { isa => 'Str', optional => 1 },
                              nm_version => { isa => 'Str', optional => 1 },
                              replicate => { isa => 'Int', optional => 1 },
    );
    my $model = $parm{'model'};
    my $clean = $parm{'clean'};
    my $option = $parm{'option'};
    my $nm_version = $parm{'nm_version'};
    my $replicate = $parm{'replicate'};

    my $basename = $model->filename;
    #this regex must be the same as used in modelfit.pm, for consistency
    $basename =~ s/\.([^.]+)$//; #last dot and extension
    my $extension = $1;
    unless (defined $extension and (length($extension)>0)){
        croak("cannot handle model filename ".$model->filename." without extension");
    }
    if ($clean){
        croak("must not set clean in combination with nm_version") if (defined $nm_version);
        croak("must not set clean in combination with record or option") if (defined $option);
        croak("must not set clean in combination with replicate") if (defined $replicate);
        #replace remaining dots with underscore
        $basename =~ s/\./_/g;
        return $basename.'.'.$extension;
    }
    if (defined $nm_version){
        croak("must not set nm_version in combination with record or option") if (defined $option);
        croak("must not set nm_version in combination with replicate") if (defined $replicate);
        croak("empty nm_version") unless (length($nm_version)>0);
        $nm_version =~ s/\./_/g; #remove risky signs
        return $basename.".$nm_version.".$extension;
    }elsif (defined $replicate){
        croak("must not set replicate in combination with record or option") if (defined $option);
        return $basename.".$replicate.".$extension;
    }else{
        croak("must set both record and option when not set nm_version or replicate") unless (defined $option);
    }
    #continue with record option variant


    #remove all risky signs from option: dots, equal, parentheses, space , comma
    #replace with underscore
    $option =~ s/\./_/g;
    $option =~ s/=/_/g;
    $option =~ s/\(/_/g;
    $option =~ s/\)/_/g;
    $option =~ s/\s+/_/g;
    $option =~ s/,/_/g;

    return $basename.".$option.".$extension;


}

sub modelfit_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    #array of arrays
    #default is to have one list per input model in this array, but if merge_rawresults is set then have only one list
    my $model_lists = [];

    #TODO remove $tables. optional keep_tables

    my @raw_results_files=();
    if ($self->merge_rawresults){
        push(@{$model_lists},[]);
        foreach my $model (@{$self->models}){
            #if more than one dot then remove all but last
            my $newname = get_modified_filename(model => $model,
                                                clean => 1);
            push(@{$model_lists->[0]},$model ->copy( filename    => $self -> directory.'m1/'.$newname,
                                                     write_copy => 0,
                                                     output_same_directory => 1,
                                                     copy_output => 0));
            $model_lists->[0]->[-1]->clear_outputs();
        }
        my ( $ldir, $name ) =    OSspecific::absolute_path( $self ->directory(), 'raw_results_benchmark.csv' );
        push(@raw_results_files,$ldir.$name) ;

    }else{
        for (my $i=0; $i< scalar(@{$self->models}); $i++){
            #make sure mX exists. There is code in tool.pm for this, but it does not work
            if ($i>0){
                my ($newdir, $newfile) = OSspecific::absolute_path( $self->directory .  '/m'.($i+1), '' );
                mkdir( $newdir );
            }
            #if more than one dot then remove all but last
            my $newname = get_modified_filename(model => $self->models->[$i],
                                                clean => 1);
            push(@{$model_lists},[$self->models->[$i]->copy( filename => $self -> directory.'m'.($i+1).'/'.$newname,
                                                             write_copy => 0,
                                                             output_same_directory => 1,
                                                             copy_output => 0) ]);

            $model_lists->[-1]->[0]->clear_outputs();
            my $filename = $self->models->[$i]->filename;
            $filename =~ s/\.[^.]+$//; #remove last dot and extension
            my ( $ldir, $name ) =    OSspecific::absolute_path( $self ->directory(), 'raw_results_'.$filename.'.csv' );
            push(@raw_results_files,$ldir.$name) ;
        }
    }
    $self->raw_results_file(\@raw_results_files);
    push(@{$self->settings_header},'base');



    if (scalar(@{$self->record_change_list})>0){
        #model_lists is modified, extended with copies, write_copy false
        create_record_variant_models(model_lists => $model_lists,
                                     change_list => $self->record_change_list);
        #TODO make subroutine for this
        my %rechash;
        for (my $ci=0; $ci < scalar(@{$self->record_change_list}); $ci++){
            my @keys = keys %{$self->record_change_list->[$ci]}; #always only one key
            my $recordname = $keys[0];
            my $short_name = lc(model::problem::_get_uc_short_type($recordname)); #lower case
            if (defined $rechash{$short_name}){
                $rechash{$short_name} = $rechash{$short_name}+1;
            }else{
                $rechash{$short_name} = 1;
            }
            push(@{$self->settings_header},$short_name.'.'.$rechash{$short_name});
        }
    }
    if (scalar(@{$self->theta_change_list})>0){
        #model_lists is modified, extended with copies, write_copy false
        create_theta_variant_models(model_lists => $model_lists,
                                    theta_list => $self->theta_change_list);
        #TODO make subroutine for this
        for (my $ci=0; $ci < scalar(@{$self->theta_change_list}); $ci++){
            my @keys = keys %{$self->theta_change_list->[$ci]}; #always only one key
            my $name = $keys[0];
            if ($name =~ /^\d+$/){
                #numeric
                $name = 'TH_'.$name;
            }
            push(@{$self->settings_header},$name);
        }
    }


    if (scalar(@{$self->alt_nonmem})>0){
        #model list is modified (file names changed) and extended
        push(@{$self->settings_header},'nm.version');
        create_nonmem_alt_models(model_lists => $model_lists,
                                 alt_nonmem => $self->alt_nonmem,
                                 nm_version => $self->nm_version);
    }

    #if do replicates first then will naturally appear together in rawres. Disadvantage is that transient system issues will affect same item
    if ($self->replicates>1){
        #model list is modified (file names changed) and individuals lists extended
        push(@{$self->settings_header},'replicate');
        create_replicate_models(model_lists => $model_lists,
                                 replicates => $self->replicates);
    }



    for (my $j=0; $j< scalar(@{$model_lists}); $j++){
        for (my $i=0; $i< scalar(@{$model_lists->[$j]}); $i++){
            my $filename = $model_lists->[$j]->[$i]->create_output_filename();
            $model_lists->[$j]->[$i]->outputfile($model_lists->[$j]->[$i]->directory.$filename);
            $model_lists->[$j]->[$i]->set_outputfile();
            $model_lists->[$j]->[$i]->_write();
            if (not $self->copy_data) {
                if (not $model_lists->[$j]->[$i]->copy_data_setting_ok(copy_data => $self->copy_data)){
                    croak("Cannot set -no-copy_data, absolute data file path is too long.");
                }
                $model_lists->[$j]->[$i]->relative_data_path(0);
            }
        }
    }

    $self->tools([]) unless (defined $self->tools());

    my @nonmems = ($self->nm_version);
    push(@nonmems,@{$self->alt_nonmem});
    for (my $i=0; $i< scalar(@nonmems); $i++){
        for (my $j=0; $j< scalar(@{$self->raw_results_file}); $j++){
            my $message = "Running models from input model ".($j+1)." with nm_version ".$nonmems[$i];
            my $dir='run_model_'.($j+1).'_nm_'.$nonmems[$i];
            if ($self->merge_rawresults){
                $message = "Running all models with nm_version ".$nonmems[$i];
                $dir='run_all_models_nm_'.$nonmems[$i];
            }
            my $offset = scalar(@{$self->raw_results_file})*$i;
            my @model_names = ();
            foreach my $mod (@{$model_lists->[$offset+$j]}){
                my $name = $mod->filename;
                $name =~ s/\.[^.]+$//; #remove last dot and extension
                my @parts = split(/\./,$name);
                push(@model_names,\@parts);
            }

            #name modelfit dir after nm-version
            my $evaluation =
                tool::modelfit ->new( %{common_options::restore_options(@common_options::tool_options)},
                                      nm_version => $nonmems[$i],
                                      models         => $model_lists->[$offset + $j],
                                      base_directory   => $self->directory,
                                      raw_results_file => [$self->raw_results_file->[$j]],
                                      raw_results_append => ($i > 0), #when writing rawres file
                                      directory             => $dir,
                                      _raw_results_callback =>
                                      $self->_modelfit_raw_results_callback(model_names => \@model_names,
                                                                            offset => scalar(@model_names) * $i,
                                                                            settings_header => $self->settings_header,
                                                                            replicates => $self->replicates),
                                      raw_results           => undef,
                                      prepared_models       => undef,
                                      top_tool              => 0,
                                      copy_data => $self->copy_data,
                );
            push(@{$self->tools}, $evaluation);

        }
    }


}

sub modelfit_analyze
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    if (defined $self->tools){
        for (my $i=0; $i< scalar(@{$self->raw_results_file}); $i++){
            if (defined $self->tools()->[$i]){
                push(@{$self->full_rawres_headers},$self->tools()->[$i]->raw_results_header);
            }
        }
    }
}

sub prepare_results
{
    my $self = shift;

}

sub _modelfit_raw_results_callback
{
    #replace model column with name without extension
    #leave sorting to write function? compute deltas here?
    my $self = shift;
    my %parm = validated_hash(\@_,
                              model_names =>{ isa => 'ArrayRef', optional => 0 },
                              offset =>{ isa => 'Int', optional => 0 },
                              settings_header =>{ isa => 'ArrayRef', optional => 0 },
                              replicates =>{ isa => 'Int', optional => 0 },
        );
    my $model_names = $parm{'model_names'}; #array of arrays
    my $offset = $parm{'offset'};
    my $settings_header = $parm{'settings_header'};
    my $replicates = $parm{'replicates'};
    my $subroutine;

    $subroutine = sub {
        my $modelfit = shift;
        my $mh_ref   = shift;
        my %max_hash = %{$mh_ref};

        my $base = round(scalar(@{$model_names}) / $replicates);
        my @newarr = ();
        if ($replicates > 1) {
            #sort so that replicates together
            for (my $i = 0; $i < $base; $i++) {
                push(@newarr, []);
            }
        }

        foreach my $row ( @{$modelfit -> raw_results()} ) {
            #modify model column
            my $nameline = $row->[0];
            my @oldrow = @{$row};
            #insert settings after model name
            $row = [$oldrow[0], @{$model_names->[$nameline-1]}, @oldrow[1 .. $#oldrow]];
            if ($replicates > 1){
                my $modulo = $nameline % $base; #modulo division
                push(@{$newarr[$modulo - 1]}, [$oldrow[0], @{$model_names->[$nameline-1]}, @oldrow[1 .. $#oldrow]]);
            }
        }
        if ($replicates > 1) {
            $modelfit->raw_results([]);
            for (my $i = 0; $i < $base; $i++) {
                push(@{$modelfit->raw_results}, @{$newarr[$i]});#multiple array refs
            }
        }

        if (not $modelfit->raw_results_append) {        # Is this the first call?
            if (scalar(@{$self->reference_lst}) > 0) {
                my $model_index = 0;
                my $ref_index = 1;
                for my $row (@{$self->_reference_raw_results}) {
                    my $nameline = $row->[0];
                    my @oldrow = @$row;
                    my $new_row = [ $model_index, utils::file::get_file_stem($self->reference_lst->[$ref_index - 1]), ("ref$ref_index") x (scalar(@{$model_names->[$nameline - 1]}) - 1), @oldrow[1 .. $#oldrow] ];
                    $ref_index++;
                    $model_index--;
                    unshift @{$modelfit->raw_results}, $new_row;
                }
            }
        }

        #TODO raw_line_structure
        my @old_header = @{$modelfit -> raw_results_header()};
        $modelfit -> raw_results_header([$old_header[0],@{$settings_header},@old_header[1 .. $#old_header]]);
    };

    return $subroutine;
}

sub create_R_plots_code{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              rplot => { isa => 'rplots', optional => 0 }
        );
    my $rplot = $parm{'rplot'};


    my @lines=();
    my @paramcounts=();
    my $subprob_est_index;
    my $subprob_cov_index;
    my $ofv_index;
    for (my $i=0; $i< scalar(@{$self->raw_results_file}); $i++){
        my @cols=();
        my $paramcount = 0;
        my $labelref = $self->models->[$i]->problems->[0]->get_estimated_attributes(parameter => 'all',
                                                                                    attribute => 'labels');
        #cannot use raw_results_header, has only placeholders for sigma etc
        my $headerref = $self->full_rawres_headers->[$i]; #ref of array of strings, no quotes
        $subprob_est_index = get_array_positions(target => $headerref,keys => ['subprob_est_time'])->[0];
        $subprob_cov_index = get_array_positions(target => $headerref,keys => ['subprob_cov_time'])->[0];
        $ofv_index = get_array_positions(target => $headerref,keys => ['ofv'])->[0];

        if (defined $labelref){
            $paramcount = scalar(@{$labelref});
            #        print join(' ',@{$headerref})."\n";
            @cols = @{get_array_positions(target => $headerref,keys => $labelref)};
        }
        my $colstring = ($i+1).'=c('.join(',',@cols).')';
        push(@lines,$colstring);
        push(@paramcounts,$paramcount);
    }


    $rplot->add_preamble(code => [
                             '#benchmark-specific preamble',
                             'OFV.COL              <- '.$ofv_index,
                             'OFV.THRESHOLD        <- '.$self->dofv_threshold,
                             'PARAMETER.THRESHOLD  <- '.$self->parameter_threshold,
                             'EST.TIME.COL         <- '.$subprob_est_index,
                             'COV.TIME.COL         <- '.$subprob_cov_index,
                             'RAWRES.COUNT         <- '.scalar(@{$self->raw_results_file}),
                             'ALL.PARAMCOUNT       <-c('.join(',',@paramcounts).')',
                             "ALL.RAWRESFILES      <-c('".join("','",@{$self->raw_results_file})."')",
                             'COL.ESTIMATED.PARAMS <-list('.join(",\n                            ",@lines).') #indices in rawres file'
                         ]);

}

1;
