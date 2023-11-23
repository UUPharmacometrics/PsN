package tool::lasso;

use include_modules;
use tool::xv;
use tool::modelfit;
use Data::Dumper;
use Cwd;
use OSspecific;
use Mouse;
use MouseX::Params::Validate;

extends 'tool';

has 'relations' => ( is => 'rw', required => 1, isa => 'Str' );
has 'covariate_statistics_file' => ( is => 'rw', isa => 'Str', default => 'covariate_statistics.txt' );
has 'lasso_model_file' => ( is => 'rw', isa => 'Str', default => 'lasso_start_model.mod' );
has 'logfile' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { ['lasso.log'] } );
has 'model_optimal' => ( is => 'rw', isa => 'ArrayRef' , default => sub { [] } );
has 'log_scale' => ( is => 'rw', isa => 'Bool', default=> 0 );
has 'NOABORT_added' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'run_final_model' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'adaptive' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'adjusted' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'normalize' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'use_pred' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'convergence' => ( is => 'rw', isa => 'Str', default => 'FIRSTMIN' );
has 'al_coefficients' => ( is => 'rw', isa => 'Str');
has 'statistics' => ( is => 'rw', isa => 'HashRef', default => sub { {} } );
has 'cutoff' => ( is => 'rw', isa => 'Num', default => 0.005 );
has 'stratify_on' => ( is => 'rw', isa => 'Str' );
has 'external_data' => ( is => 'rw', isa => 'Str' );
has 'warnings' => ( is => 'rw', isa => 'Int', default => 0 );
has 'step_t' => ( is => 'rw', isa => 'Num', default => 0.05 );
has 'start_t' => ( is => 'rw', isa => 'Num', default => 0 );
has 'stop_t' => ( is => 'rw', isa => 'Num', default => 1 );
has 'pred_ofv_start_t' => ( is => 'rw', isa => 'Num' );
has 'groups' => ( is => 'rw', isa => 'Int', default => 5 );
has 't_thetanumber' => ( is => 'rw', isa => 'Int', default => 0);
has 'evaluation_number' => ( is => 'rw', isa => 'Int');
has 'lambda_thetanumber' => ( is => 'rw', isa => 'Maybe[Int]');
has 'results_file' => ( is => 'rw', isa => 'Str', default => 'lasso_results.csv' );
has 'cutoff_thetas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'cutoff_thetas_estimates' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'cutoff_thetas_labels' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'factor_estimate' => ( is => 'rw', isa => 'Num' );
has 'weight_thetas' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );


our $blank = "    ";
our $row_length = 80;
our $decimal_points =5;
our $dec_str = "%.$decimal_points". "f";
our    $sign_dec_str = "%+.$decimal_points". "f";
our $theta_initial_value = 0.0001;


sub BUILD
{
    my $self  = shift;

    if ( scalar (@{$self -> models->[0]-> problems}) != 1 ){
        croak('The input model must contain exactly one problem.');
    }


    if ($self->groups()<2){
        croak("groups must be at least 2");
    }
    if ($self->step_t()==0) {
        croak("step_t cannot be 0");
    }
    unless ($self->start_t()>=0) {
        croak("start_t must not be smaller than 0");
    }
    unless ($self->stop_t()>=0) {
        croak("stop_t must not be smaller than 0");
    }

    if ($self->step_t()>0) {
        if ($self->stop_t()< $self->start_t()) {
            croak("stop_t cannot be smaller than start_t when step_t is positive");
        }
    }else{
        if ($self->stop_t()> $self->start_t()) {
            croak("stop_t cannot be larger than start_t when step_t is negative");
        }
    }

    unless ($self->convergence() =~ /^(REACHMAX|FIRSTMIN|HALT)$/){
        croak("convergence criterion must be either REACHMAX,".
            " FIRSTMIN or HALT.");
    }

    if ($self->adjusted){
        $self->adaptive(1);
    }

    foreach my $model ( @{$self -> models} ) {
        foreach my $problem (@{$model->problems()}){
            if (defined $problem->nwpri_ntheta()){
                ui -> print( category => 'all',
                    message => "Warning: lasso does not support \$PRIOR NWPRI.",
                    newline => 1);
                last;
            }
        }
    }

    if ($self->models->[0]->filename =~ /^sample(\d+)\.mod/){
        my $num = $1;
        $self->evaluation_number($1);
    }

    {
        my $ldir;
        my $name;

        ($ldir, $name) = OSspecific::absolute_path($self->directory, $self->logfile->[0]);
        $self->logfile->[0] = $ldir.$name;
        ($ldir, $name) = OSspecific::absolute_path($self->directory, $self->raw_results_file->[0]);
        $self->raw_results_file->[0] = $ldir.$name;
    }

    foreach my $attribute ( 'covariate_statistics_file', 'lasso_model_file') {
        my $name = $self -> {$attribute};
        my $ldir;
        ( $ldir, $name ) = OSspecific::absolute_path( $self -> directory, $name );
        $self -> {$attribute} = $ldir.$name;
    }
}


sub parse_row
{
    #sub for splitting too long lines of newly generated code
    my %parm = validated_hash(\@_,
        parse_str => { isa => 'Str', optional => 0 },
        parse_operator => { isa => 'Str', optional => 0 },
        max_length => { isa => 'Int', optional => 0 }
    );
    my $parse_str = $parm{'parse_str'};
    my $parse_operator = $parm{'parse_operator'};
    my $max_length = $parm{'max_length'};
    my @rows;

    my $variable_name;
    my $len;
    my $tmp_str;
    my $done = 0;
    if (length($parse_str)<$max_length) {
        push(@rows,$parse_str . "\n");
    }elsif (index($parse_str,"=")==-1){
        push(@rows,$parse_str . "\n");
    }else{
        $variable_name = substr($parse_str,0,index($parse_str,"="));
        $variable_name  =~ s/^\s+//;
        $variable_name   =~ s/\s+$//;

        my $op_index = index($parse_str,$parse_operator);
        if ($op_index==-1) {
            push(@rows,$parse_str . "\n");
        }else{
            while (length($parse_str)>$max_length)
            {
                my $i = $op_index+1;
                $len=length($parse_str)-1;
                while (index($parse_str,$parse_operator,$i)!=-1
                    && $i<=$max_length && $i<=$len)    {
                    if (index($parse_str,$parse_operator,$i)<=$max_length) {
                        $op_index = index($parse_str,$parse_operator,$i);
                    } else{
                        last;
                    }
                    $i = $op_index+1;
                }
                $tmp_str = substr($parse_str,0,$op_index);
                push @rows, $tmp_str ."\n";
                $parse_str = substr($parse_str,0,index($parse_str,"=")+1) . " " .
                $variable_name .$parse_operator .
                substr($parse_str,$op_index+length($parse_operator));
                $op_index = index($parse_str,$parse_operator);
                if ($op_index==-1){
                    $done = 1;
                }

            }
            push (@rows, $parse_str ."\n") unless ($done);

        }
    }

    return \@rows;
}

sub factor_string
{
    #have unit tests
    my %parm = validated_hash(\@_,
                              parameter => { isa => 'Str', optional => 0 },
                              covariate => { isa => 'Str', optional => 0 },
                              thetanumber => {isa => 'Int', optional => 0},
                              adaptive=> {isa => 'Bool', optional => 0},
                              normalize=> {isa => 'Bool', optional => 0},
                              mean => {isa => 'Num', optional => 0},
                              sd => { isa => 'Num', optional => 0 },
        );
    my $parameter = $parm{'parameter'};
    my $covariate = $parm{'covariate'};
    my $thetanumber = $parm{'thetanumber'};
    my $adaptive = $parm{'adaptive'};
    my $normalize = $parm{'normalize'};
    my $mean = $parm{'mean'};
    my $sd = $parm{'sd'};

    my $name = $parameter.$covariate;
    my $adstring='';
    if ($adaptive){
        $adstring = '*AL_'.$name;
    }

    my $string;

    if ($normalize){
        $string = $name." = THETA(" .$thetanumber.")".$adstring."*($covariate".
            sprintf($sign_dec_str,-$mean).")/".sprintf($dec_str,$sd)."*FACTOR";
    }else{
        $string = $name." = THETA(" .$thetanumber.")".$adstring."*$covariate"."*FACTOR";
    }
    return $string;
}

sub add_adaptive_lasso_theta
{
    #have unit tests
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              parameter => { isa => 'Str', optional => 0 },
                              covariate => { isa => 'Str', optional => 0 },
                              thetanumber => {isa => 'Int', optional => 0},
        );
    my $model = $parm{'model'};
    my $parameter = $parm{'parameter'};
    my $covariate = $parm{'covariate'};
    my $thetanumber = $parm{'thetanumber'};

    my $name = "AL_$parameter$covariate";
    my $code = $name.' = THETA('.$thetanumber.') ; FIXED';

    $model->initial_values(parameter_type => 'theta',
                           parameter_numbers => [[$thetanumber]],
                           new_values =>[[1]],
                           add_if_absent => 1);

    $model->fixed(parameter_type => 'theta',
                  parameter_numbers => [[$thetanumber]],
                  new_values => [[1]] );
    $model -> lower_bounds(parameter_type =>  'theta',
                           parameter_numbers =>[[$thetanumber]],
                           new_values => [[undef]] );
    $model -> upper_bounds(parameter_type =>  'theta',
                           parameter_numbers =>[[$thetanumber]],
                           new_values => [[undef]] );

    $model -> labels( parameter_type     => 'theta',
                      parameter_numbers => [[$thetanumber]],
                      new_values        => [['TH'.($thetanumber)." $name"]] );

    return $code;
}

sub update_adaptive_theta
{
    #have unit tests
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              coefficient => { isa => 'Num', optional => 0 },
                              thetanumber => {isa => 'Int', optional => 0},
                              al_thetanumber => {isa => 'Int', optional => 0},
        );
    my $model = $parm{'model'};
    my $coefficient = $parm{'coefficient'};
    my $thetanumber = $parm{'thetanumber'};
    my $al_thetanumber = $parm{'al_thetanumber'};

#    $coefficient = sprintf($dec_str,abs($coefficient)); then lasso_coeff.csv is wrong
    $coefficient = abs($coefficient);

    if ($coefficient == 0){
        #set both thetas to 0 FIX
        $model->initial_values(parameter_type => 'theta',
                               parameter_numbers => [[$thetanumber]],
                               new_values =>[[0]]);
        $model -> lower_bounds(parameter_type =>  'theta',
                               parameter_numbers =>[[$thetanumber]],
                               new_values => [[undef]] );
        $model -> upper_bounds(parameter_type =>  'theta',
                               parameter_numbers =>[[$thetanumber]],
                               new_values => [[undef]] );

        $model->fixed(parameter_type => 'theta',
                      parameter_numbers => [[$thetanumber]],
                      new_values => [[1]] );

        $model->initial_values(parameter_type => 'theta',
                               parameter_numbers => [[$al_thetanumber]],
                               new_values =>[[0]]);

        #al already FIX
    }else{
        #update al_thetanumber to coefficient, still fix
        #reset thetanumber to small init, since starting new xv with small t
        #update bounds based on coefficient

        my $init=$theta_initial_value;

        #FIXME if many iterations pick up old value of coefficient, now it is 1



        my $lower_val = $model ->lower_bounds( parameter_type    => 'theta',
                                               parameter_numbers => [[$thetanumber]])->[0][0];
        my $upper_val = $model ->upper_bounds( parameter_type    => 'theta',
                                               parameter_numbers => [[$thetanumber]])->[0][0];

        if (defined $upper_val or defined $lower_val){
            #will be undef if log scale
            #FIXME this only works one iteration, otherwise need sd and mean
            $upper_val=sprintf($dec_str,$upper_val/$coefficient);
            $lower_val=sprintf($dec_str,$lower_val/$coefficient);

            $model -> lower_bounds(parameter_type =>  'theta',
                                   parameter_numbers =>[[$thetanumber]],
                                   new_values => [[$lower_val]] );
            $model -> upper_bounds(parameter_type =>  'theta',
                                   parameter_numbers =>[[$thetanumber]],
                                   new_values => [[$upper_val]] );

            unless (($init > $lower_val) and ($init < $upper_val)){
                $init = $lower_val + ($upper_val-$lower_val)*(0.05);
            }
        }
        $model->initial_values(parameter_type => 'theta',
                               parameter_numbers => [[$thetanumber]],
                               new_values =>[[$init]]);


        $model->initial_values(parameter_type => 'theta',
                               parameter_numbers => [[$al_thetanumber]],
                               new_values =>[[$coefficient]]);

    }

}


sub final_theta_string
{
    #have unit tests
    my %parm = validated_hash(\@_,
                              parameter => { isa => 'Str', optional => 0 },
                              covariate => { isa => 'Str', optional => 0 },
                              thetanumber => {isa => 'Int', optional => 0},
                              normalize=> {isa => 'Bool', optional => 0},
                              mean => {isa => 'Num', optional => 0},
        );
    my $parameter = $parm{'parameter'};
    my $covariate = $parm{'covariate'};
    my $thetanumber = $parm{'thetanumber'};
    my $mean = $parm{'mean'};
    my $normalize = $parm{'normalize'};

    my $string;
    if ($normalize){
        $string = $parameter.$covariate. " = THETA(" .$thetanumber.")*($covariate".
            sprintf($sign_dec_str,-$mean).")";
    }else{
        $string = $parameter.$covariate. " = THETA(" .$thetanumber.")*$covariate";
    }

    return $string;
}

sub add_lasso_theta
{
    #have unit tests
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              parameter => { isa => 'Str', optional => 0 },
                              covariate => { isa => 'Str', optional => 0 },
                              normalize=> {isa => 'Bool', optional => 0},
                              log_scale => {isa => 'Bool', optional => 0},
                              thetanumber => {isa => 'Int', optional => 0},
                              mean => {isa => 'Num', optional => 0},
                              sd => { isa => 'Num', optional => 0 },
                              max => { isa => 'Num', optional => 0 },
                              min => { isa => 'Num', optional => 0 },
        );
    my $model = $parm{'model'};
    my $parameter = $parm{'parameter'};
    my $covariate = $parm{'covariate'};
    my $thetanumber = $parm{'thetanumber'};
    my $mean = $parm{'mean'};
    my $sd = $parm{'sd'};
    my $max = $parm{'max'};
    my $min = $parm{'min'};
    my $normalize = $parm{'normalize'};
    my $log_scale = $parm{'log_scale'};

    $model->initial_values(parameter_type => 'theta',
                           parameter_numbers => [[$thetanumber]],
                           new_values =>[[$theta_initial_value]],
                           add_if_absent => 1);

    my $upper_val=undef;
    my $lower_val=undef;

    unless ($log_scale){
        if ($normalize){
            $upper_val=sprintf($dec_str,-1/(($min-$mean)/$sd));
            $lower_val=sprintf($dec_str,-1/(($max-$mean)/$sd));
        }else{
            if ($min == 0){
                $upper_val=undef;
            }else{
                $upper_val=sprintf($dec_str,-1/$min);
            }
            $lower_val=sprintf($dec_str,-1/$max);
        }
    }
    $model -> lower_bounds(parameter_type =>  'theta',
                           parameter_numbers =>[[$thetanumber]],
                           new_values => [[$lower_val]] );
    $model -> upper_bounds(parameter_type =>  'theta',
                           parameter_numbers =>[[$thetanumber]],
                           new_values => [[$upper_val]] );
    $model -> labels( parameter_type     => 'theta',
                      parameter_numbers => [[$thetanumber]],
                      new_values        => [['TH'.($thetanumber)." $parameter$covariate"]] );


}

sub add_optimal_theta
{
    #have unit tests
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              parameter => { isa => 'Str', optional => 0 },
                              covariate => { isa => 'Str', optional => 0 },
                              thetanumber => {isa => 'Int', optional => 0},
                              normalize=> {isa => 'Bool', optional => 0},
                              coefficient => {isa => 'Num', optional => 0},
                              sd => { isa => 'Num', optional => 0 },
        );
    my $model = $parm{'model'};
    my $parameter = $parm{'parameter'};
    my $covariate = $parm{'covariate'};
    my $thetanumber = $parm{'thetanumber'};
    my $coefficient = $parm{'coefficient'};
    my $sd = $parm{'sd'};
    my $normalize = $parm{'normalize'};

    #rounding as in model code
    $sd =sprintf($dec_str,$sd);

    my $theta_initial_value;
    if ($normalize){
        $theta_initial_value = $coefficient/($sd);
    }else{
        $theta_initial_value = $coefficient;
    }

    $model->initial_values(parameter_type => 'theta',
                           parameter_numbers => [[$thetanumber]],
                           new_values =>[[$theta_initial_value]],
                           add_if_absent => 1);

    $model -> lower_bounds(parameter_type =>  'theta',
                           parameter_numbers =>[[$thetanumber]],
                           new_values => [[undef]] );
    $model -> upper_bounds(parameter_type =>  'theta',
                           parameter_numbers =>[[$thetanumber]],
                           new_values => [[undef]] );
    $model -> labels( parameter_type     => 'theta',
                      parameter_numbers => [[$thetanumber]],
                      new_values        => [['TH'.($thetanumber)." $parameter$covariate"]] );

    $model->fixed(parameter_type => 'theta',
                  parameter_numbers => [[$thetanumber]],
                  new_values => [[1]] );

}

sub check_name
{
    my %parm = validated_hash(\@_,
                              parameter => { isa => 'Str', optional => 0 },
                              covariate => { isa => 'Str', optional => 0 },
                              factor => {isa => 'Str', optional => 1, default => ''},
                              H => {isa => 'Str', optional => 1, default => ''},
                              version => {isa => 'Maybe[Int]', optional => 0},
        );
    my $parameter = $parm{'parameter'};
    my $covariate = $parm{'covariate'};
    my $factor = $parm{'factor'};
    my $H = $parm{'H'};
    my $version = $parm{'version'};

    unless (defined $version){
        #assume 7
        $version=7;
    }

    my $name = $parameter.$H.$covariate.$factor;

    if ($version < 7 and length($name)>6){
        my $shorten = (length($covariate)>= length($parameter))? $covariate : $parameter;
        croak("Lasso parameter name ".$name.' is '.
              "longer than 6 characters, not allowed by NONMEM".$version.
              ". Shorten the name for $shorten or use NONMEM7.");
    }

}

sub get_abssum
{
    my %parm = validated_hash(\@_,
                              cutoff_thetas => {isa => 'ArrayRef', optional => 0},
        );
    my $cutoff_thetas = $parm{'cutoff_thetas'};

    my $tmpstr = "ABSSUM = ";
    my $first = 1;
    foreach my $i (@{$cutoff_thetas}){
        if ($first) {
            $tmpstr = $tmpstr . "ABS(THETA($i))";
            $first=0;
        } else {
            $tmpstr = $tmpstr . "+ABS(THETA($i))";
        }
    }
    return $tmpstr;
}

sub setup_full_model
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              use_pred =>{isa => 'Bool', optional => 0},
                              covariance => { isa => 'ArrayRef', optional => 0 },
        );
    my $model = $parm{'model'};
    my $use_pred = $parm{'use_pred'};
    my $covariance = $parm{'covariance'};

    $model->problems->[0] -> add_records( record_strings => $covariance,
                                          type => 'covariance' );

    remove_lasso_constraint(model => $model,use_pred => $use_pred);
}

sub remove_lasso_constraint
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              use_pred =>{isa => 'Bool', optional => 0},
                              delete_factor => {isa => 'Bool', optional => 1, default=> 0},
        );
    my $model = $parm{'model'};
    my $use_pred = $parm{'use_pred'};
    my $delete_factor = $parm{'delete_factor'};

    my @old_code;
    if ($use_pred){
        @old_code = @{$model->get_code(record => 'pred')};
    }else{
        @old_code = @{$model->get_code(record => 'pk')};
    }

    #remove everything betw
    # "TVALUE  = THETA(".$nthetas.")\n";
    #and "FACTOR = EXP(1-RATIO)\n";
    my $copy = 1;
    my @new_code=();
    foreach my $line (@old_code){
        if ($line =~ /^\s*TVALUE  = THETA/){
            push(@new_code,$line) unless ($delete_factor);
            $copy=0;
        }elsif($line =~ /^\s*FACTOR = EXP/){
            push(@new_code,$blank.'FACTOR = 1') unless ($delete_factor);
            $copy=1;
        }elsif($line =~ /\s*;;; LASSO/){ #begin or end
            next;
        }elsif($copy){
            push(@new_code,$line);
        }
    }
#    print join("\n",@new_code)."\n";
    if ($use_pred) {
        $model->set_code(record => 'pred', code => \@new_code);
    } else {
        $model->set_code(record => 'pk', code => \@new_code);
    }

}

sub remove_covariate_normalization
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              use_pred =>{isa => 'Bool', optional => 0},
                              adaptive =>{isa => 'Bool', optional => 0},
                              theta_labels =>{isa => 'ArrayRef', optional => 0},
        );
    my $model = $parm{'model'};
    my $use_pred = $parm{'use_pred'};
    my $theta_labels = $parm{'theta_labels'};
    my $adaptive = $parm{'adaptive'};

    my @old_code;
    if ($use_pred){
        @old_code = @{$model->get_code(record => 'pred')};
    }else{
        @old_code = @{$model->get_code(record => 'pk')};
    }

    my @new_code=();
    my @labels = @{$theta_labels};
    my $count = scalar(@labels);
    my $nextlabel = shift(@labels);
    my $index = 0;
    my $nextal = 'AL_'.$theta_labels->[$index];
    my @sd = ();

    foreach my $line (@old_code){
        if ($adaptive and ($line =~ /^\s*$nextal/)){
            $index++;
            $nextal = 'AL_'.$theta_labels->[$index] if ($index < $count);
            next;
        }elsif ($line =~ /^\s*$nextlabel/){
            $line =~ s/\*FACTOR//;
            if ($adaptive){
                $line =~ s/\*AL_$nextlabel//;
            }
            $line =~ s/\/(\d+\.\d+)//; #remove sd and store it
            push(@sd,$1);
            push(@new_code,$line);
            if (scalar(@labels)> 0){
                $nextlabel = shift(@labels);
            }else{
                $nextlabel = 'done';
            }
        }else{
            push(@new_code,$line);
        }
    }
#    print join("\n",@new_code)."\n";
    if ($use_pred) {
        $model->set_code(record => 'pred', code => \@new_code);
    } else {
        $model->set_code(record => 'pk', code => \@new_code);
    }
    return \@sd;
}


sub setup_regular_model
{
    my %parm = validated_hash(\@_,
                              lasso_model => { isa => 'model', optional => 0 },
                              use_pred =>{isa => 'Bool', optional => 0},
                              adaptive =>{isa => 'Bool', optional => 0},
                              adjusted =>{isa => 'Bool', optional => 0},
                              iteration =>{isa => 'Int', optional => 0},
                              number =>{isa => 'Maybe[Int]', optional => 1},
                              normalize=> {isa => 'Bool', optional => 0},
                              external_data =>{isa => 'Str', optional => 1},
                              directory =>{isa => 'Str', optional => 0},
                              cutoff_thetas => { isa => 'ArrayRef', optional => 0 },
                              cutoff_thetas_labels => { isa => 'ArrayRef', optional => 0 },
                              lasso_coefficients => { isa => 'ArrayRef', optional => 0 },
        );
    my $lasso_model = $parm{'lasso_model'};
    my $use_pred = $parm{'use_pred'};
    my $adaptive = $parm{'adaptive'};
    my $adjusted = $parm{'adjusted'};
    my $iteration = $parm{'iteration'};
    my $number = $parm{'number'};
    my $external_data = $parm{'external_data'};
    my $directory = $parm{'directory'};
    my $cutoff_thetas = $parm{'cutoff_thetas'};
    my $cutoff_thetas_labels = $parm{'cutoff_thetas_labels'};
    my $lasso_coefficients = $parm{'lasso_coefficients'};
    my $normalize = $parm{'normalize'};

    my $for_evaluation = 0;
    if (defined $external_data){
        $for_evaluation = 1;
    }

    my $filename;
    my $tablename;
    unless (defined $number){
        $number = 0;
    }

    if ($for_evaluation){
        if ($adjusted){
            if ($iteration == 1){
                $filename = 'evaluation_type_aal_sample_'.$number.'.mod';
                $tablename = 'dvpred_type_aal_sample_'.$number.'.tab';
            }else{
                croak("only one iteration with adjusted or regular");
            }
        }else{
            #adaptive
            if ($iteration == 1){
                $filename = 'evaluation_type_l_sample_'.$number.'.mod';
                $tablename = 'dvpred_type_l_sample_'.$number.'.tab';
            }else{
                unless ($adaptive){
                    croak("only one iteration with adjusted or regular");
                }
                $filename = 'evaluation_type_al_sample_'.$number.'.mod';
                $tablename = 'dvpred_type_al_sample_'.$number.'.tab';
            }
        }
    }else{
        my $temp;
        if ($number == 0){
            #regular lasso, no workflow
            $temp = '';
        }else{
            $temp = '_sample_'.$number;
        }
        if ($adjusted){
            if ($iteration == 1){
                $filename = 'optimal_type_aal'.$temp.'.mod';
            }else{
                croak("only one iteration with adjusted or regular");
            }
        }else{
            #adaptive
            if ($iteration == 1){
                $filename = 'optimal_type_l'.$temp.'.mod';
            }else{
                unless ($adaptive){
                    croak("only one iteration with adjusted or regular");
                }
                $filename = 'optimal_type_al'.$temp.'.mod';
            }
        }
    }

    my $model = $lasso_model -> copy(filename => $directory.$filename,
                                     copy_datafile => 0,
                                     output_same_directory => 1,
                                     write_copy => 0,
                                     copy_output => 0);

    remove_lasso_constraint(model => $model,
                            use_pred => $use_pred,
                            delete_factor => 1);

    my $count = scalar(@{$cutoff_thetas});
    my $sd;
    if ($normalize){
        #lasso was run with normalization
        $sd = remove_covariate_normalization(model => $model,
                                             use_pred => $use_pred,
                                             theta_labels => $cutoff_thetas_labels,
                                             adaptive => $adaptive);
    }

    if ($for_evaluation){
        #For mae
        $model -> add_records(type => 'table',
                              record_strings => ['ID EVID DV PRED NOAPPEND NOPRINT ONEHEADER FILE='.$tablename],
                              problem_numbers => [1]);

        $model->set_maxeval_zero(need_ofv=> 1);

        $model->ignore_missing_data(1);
        $model->datafiles(new_names =>[$external_data],problem_numbers => [1]);
        $model->relative_data_path(0);
    }


    if ($adaptive){
        update_al_coefficients(model => $model,
                               cutoff_thetas => $cutoff_thetas,
                               coefficients => [(1) x $count]); #set all 1 FIX
    }
    for (my $i=0; $i<$count; $i++){
        my $thetanumber = $cutoff_thetas->[$i];
        my $value = $lasso_coefficients->[$i];
        if ($normalize and ($sd->[$i] != 0)){
            $value = $value/($sd->[$i]);
        }
        #change bounds first so that init update will not be rejected
        $model -> lower_bounds(parameter_type =>  'theta',
                               parameter_numbers =>[[$thetanumber]],
                               new_values => [[undef]] );
        $model -> upper_bounds(parameter_type =>  'theta',
                               parameter_numbers =>[[$thetanumber]],
                               new_values => [[undef]] );

        $model->initial_values(parameter_type => 'theta',
                               parameter_numbers => [[$thetanumber]],
                               new_values =>[[$value]]);

        $model->fixed(parameter_type => 'theta',
                      parameter_numbers => [[$thetanumber]],
                      new_values => [[1]] );
    }
    return $model;
}


sub setup_lasso_model
{
    my %parm = validated_hash(\@_,
                              lasso_model => { isa => 'model', optional => 0 },
                              parameter_covariate_form => { isa => 'HashRef', optional => 0 },
                              t_value =>{isa => 'Num', optional => 0},
                              statistics => { isa => 'HashRef', optional => 0 },
                              normalize=> {isa => 'Bool', optional => 0},
                              log_scale=> {isa => 'Bool', optional => 0},
                              missing_data_token => {isa => 'Str', optional => 0},
                              adaptive => {isa => 'Bool', optional => 0},
        );
    my $lasso_model = $parm{'lasso_model'};
    my %parameter_covariate_form = %{$parm{'parameter_covariate_form'}};
    my $t_value = $parm{'t_value'};
    my $statistics = $parm{'statistics'};
    my $missing_data_token = $parm{'missing_data_token'};
    my $adaptive = $parm{'adaptive'};
    my $normalize = $parm{'normalize'};
    my $log_scale = $parm{'log_scale'};

    my @cutoff_thetas=();
    my @cutoff_thetas_labels=();
    my @weight_thetas=();
    my $lambda_theta;

    my @old_code;
    @old_code = @{$lasso_model->get_code(record => 'pk')};
    my $use_pred = 0;
    unless ($#old_code > 0) {
        @old_code = @{$lasso_model->get_code(record => 'pred')};
        $use_pred = 1;
    }
    if ( $#old_code <= 0 ) {
        croak("Neither PK or PRED defined in " .
            $lasso_model -> filename . "\n" );
    }

    my @new_code;
    my @if_statements; #only once for each covariate, not for each param.
    my @zero_statements; #only once for each covariate, not for each param.
    my @lasso_coefficient_code =(); #adaptive only
    my %if_printed;
    my @factor_code;
    my $nthetas =  $lasso_model->nthetas();
    my $old_thetas = $nthetas;
    unshift @new_code, ";;; LASSO-END\n";

    my $sign = '*';
    my $plus1 = '+1';
    if ($log_scale){
        $sign = '+' ;
        $plus1 = '';
    }

    my $tmpstr;

    foreach my $par (sort {lc($a) cmp lc($b)} keys %parameter_covariate_form){
        $tmpstr = $par . "COV = ";
        my $first = 1;
        foreach my $covariate (sort {lc($a) cmp lc($b)} keys %{$parameter_covariate_form{$par}}){
            if ($first == 1){
                $first=0;
            } else {
                $tmpstr = $tmpstr . $sign;
            }
            if ($parameter_covariate_form{$par}{$covariate}{'form'} == 2){
                check_name(parameter => $par,covariate=>$covariate,version => $PsN::nm_major_version);
                $tmpstr = $tmpstr . "($par" ."$covariate$plus1)";
                $nthetas++;
                $parameter_covariate_form{$par}{$covariate}{'theta'}=$nthetas;
                $parameter_covariate_form{$par}{$covariate}{'lasso_coefficient'}=1;
                push(@cutoff_thetas,$nthetas);
                push(@cutoff_thetas_labels,$par.$covariate);
                push (@factor_code,$blank.factor_string(parameter => $par,
                                                        covariate => $covariate,
                                                        thetanumber => $nthetas,
                                                        adaptive => $adaptive,
                                                        normalize => $normalize,
                                                        mean => $statistics->{$covariate}{2}{'mean'},
                                                        sd => $statistics->{$covariate}{2}{'sd'}));

                add_lasso_theta(model => $lasso_model,
                                parameter => $par,
                                covariate => $covariate,
                                thetanumber => $nthetas,
                                normalize => $normalize,
                                log_scale => $log_scale,
                                mean => $statistics->{$covariate}{2}{'mean'},
                                sd => $statistics->{$covariate}{2}{'sd'},
                                max => $statistics->{$covariate}{2}{'max'},
                                min => $statistics->{$covariate}{2}{'min'});
                if ($adaptive){
                    $nthetas++;
                    $parameter_covariate_form{$par}{$covariate}{'al_theta'}=$nthetas;
                    push(@lasso_coefficient_code,$blank.add_adaptive_lasso_theta(model => $lasso_model,
                                                                                 parameter => $par,
                                                                                 covariate => $covariate,
                                                                                 thetanumber => $nthetas));
                }

            }elsif ($parameter_covariate_form{$par}{$covariate}{'form'} == 3){
                check_name(parameter => $par,covariate=>$covariate,version => $PsN::nm_major_version, H => 'H');
                $tmpstr = $tmpstr . "($par" ."$covariate$plus1)".$sign."("."$par"."H"."$covariate"."$plus1)";
                $nthetas++;
                $parameter_covariate_form{$par}{$covariate}{'theta'}=$nthetas;
                $parameter_covariate_form{$par}{$covariate}{'lasso_coefficient'}=1;
                push(@cutoff_thetas,$nthetas);
                push(@cutoff_thetas_labels,$par.$covariate);
                push (@factor_code,$blank.factor_string(parameter => $par,
                                                        covariate => $covariate,
                                                        thetanumber => $nthetas,
                                                        adaptive => $adaptive,
                                                        normalize => $normalize,
                                                        mean => $statistics->{$covariate}{3}{'mean'},
                                                        sd => $statistics->{$covariate}{3}{'sd'}));
                add_lasso_theta(model => $lasso_model,
                                parameter => $par,
                                covariate => $covariate,
                                thetanumber => $nthetas,
                                normalize => $normalize,
                                log_scale => $log_scale,
                                mean => $statistics->{$covariate}{3}{'mean'},
                                sd => $statistics->{$covariate}{3}{'sd'},
                                max => $statistics->{$covariate}{3}{'max'},
                                min => $statistics->{$covariate}{3}{'min'});

                if ($adaptive){
                    $nthetas++;
                    $parameter_covariate_form{$par}{$covariate}{'al_theta'}=$nthetas;
                    push(@lasso_coefficient_code,$blank.add_adaptive_lasso_theta(model => $lasso_model,
                                                                                 parameter => $par,
                                                                                 covariate => $covariate,
                                                                                 thetanumber => $nthetas));
                }

                $nthetas++;
                $parameter_covariate_form{$par}{$covariate}{'Htheta'}=$nthetas;
                $parameter_covariate_form{$par}{$covariate}{'Hlasso_coefficient'}=1;
                push(@cutoff_thetas,$nthetas);
                push(@cutoff_thetas_labels,$par.'H'.$covariate);
                push (@factor_code,$blank.factor_string(parameter => $par,
                                                        covariate => 'H'.$covariate,
                                                        thetanumber => $nthetas,
                                                        adaptive => $adaptive,
                                                        normalize => $normalize,
                                                        mean => $statistics->{$covariate}{3}{'H-mean'},
                                                        sd => $statistics->{$covariate}{3}{'H-sd'}));

                add_lasso_theta(model => $lasso_model,
                                parameter => $par,
                                covariate => 'H'.$covariate,
                                thetanumber => $nthetas,
                                normalize => $normalize,
                                log_scale => $log_scale,
                                mean => $statistics->{$covariate}{3}{'H-mean'},
                                sd => $statistics->{$covariate}{3}{'H-sd'},
                                max => ($statistics->{$covariate}{3}{'max'}-$statistics->{$covariate}{3}{'breakpoint'}),
                                min => 0);

                if ($adaptive){
                    $nthetas++;
                    $parameter_covariate_form{$par}{$covariate}{'Hal_theta'}=$nthetas;
                    push(@lasso_coefficient_code,$blank.add_adaptive_lasso_theta(model => $lasso_model,
                                                                                 parameter => $par,
                                                                                 covariate => 'H'.$covariate,
                                                                                 thetanumber => $nthetas));
                }

                ##IF hockey-stick covariates
                unless ($if_printed{$covariate}{3}){
                    my $cut_off =  $statistics->{$covariate}{3}{'breakpoint'};
                    push @zero_statements, $blank . "H".$covariate . " = 0\n";
                    push @if_statements, $blank . "IF ($covariate .GT. " . sprintf($dec_str,$cut_off) .
                        ") H$covariate = $covariate" .sprintf($sign_dec_str,-$cut_off) ."\n";
                    $if_printed{$covariate}{3}=1;
                }

            }elsif ($parameter_covariate_form{$par}{$covariate}{'form'} == 1){
                my $most_common_key = $statistics->{$covariate}{1}{'most_common'};
                my $first_cat = 1;
                $parameter_covariate_form{$par}{$covariate}{'thetas'}={};
                foreach my $fact (sort {$a<=>$b} keys %{$statistics->{$covariate}{1}{'cat_hash'}}) {
                    my %mean = %{$statistics->{$covariate}{1}{'mean'}};
                    my %sd = %{$statistics->{$covariate}{1}{'sd'}};
                    if (($fact ne $most_common_key) and ($fact ne $missing_data_token)){
                        check_name(parameter => $par,covariate=>$covariate,version => $PsN::nm_major_version, factor => $fact);
                        $tmpstr .= $sign unless $first_cat;
                        $first_cat = 0;
                        $tmpstr = $tmpstr . "(".$par.$covariate.$fact . "$plus1)";
                        $nthetas++;
                        $parameter_covariate_form{$par}{$covariate}{'thetas'}->{$fact}=$nthetas;
                        $parameter_covariate_form{$par}{$covariate}{'lasso_coefficients'}->{$fact}=1;
                        push(@cutoff_thetas,$nthetas);
                        push(@cutoff_thetas_labels,$par.$covariate.$fact);
                        push (@factor_code,$blank.factor_string(parameter => $par,
                                                                covariate => $covariate.$fact,
                                                                thetanumber => $nthetas,
                                                                adaptive => $adaptive,
                                                                normalize => $normalize,
                                                                mean => $mean{$fact},
                                                                sd => $sd{$fact}));
                        unless ($if_printed{$covariate}{1}){
                            push @zero_statements, $blank . $covariate.$fact ." = 0\n";
                            push @if_statements, $blank . "IF ($covariate .EQ. $fact) $covariate$fact=1\n";
                        }

                        add_lasso_theta(model => $lasso_model,
                                        parameter => $par,
                                        covariate => $covariate.$fact,
                                        thetanumber => $nthetas,
                                        normalize => $normalize,
                                        log_scale => $log_scale,
                                        mean => $mean{$fact},
                                        sd => $sd{$fact},
                                        max => 1,
                                        min => 0);
                        if ($adaptive){
                            $nthetas++;
                            $parameter_covariate_form{$par}{$covariate}{'al_thetas'}->{$fact}=$nthetas;
                            push(@lasso_coefficient_code,$blank.add_adaptive_lasso_theta(model => $lasso_model,
                                                                                         parameter => $par,
                                                                                         covariate => $covariate.$fact,
                                                                                         thetanumber => $nthetas));
                        }
                    }
                }
                $if_printed{$covariate}{1}=1;
            }else{
                croak("Unknown form parameter $par covariate $covariate form ".$parameter_covariate_form{$par}{$covariate}{'form'});
            }
        }
        unshift @new_code, @{parse_row(parse_str => $blank . $tmpstr,
                                       parse_operator =>$sign,
                                       max_length => $row_length)};
    }
    unshift @new_code, "\n";
    unshift @new_code, @factor_code;
    unshift @new_code, "\n";
    unshift @new_code,@if_statements;
    unshift @new_code,@zero_statements;
    if ($adaptive){
        unshift @new_code, "\n";
        unshift(@new_code,@lasso_coefficient_code);
    }
    unshift @new_code, "\n";

    #add FACTOR
    unshift @new_code, $blank . "FACTOR = EXP(1-RATIO)\n";
    unshift @new_code, $blank . "IF (RATIO .GT. 5) EXIT 1 1\n";
    unshift @new_code, $blank . "RATIO = ABSSUM/TVALUE\n";
    unshift @new_code, "\n";

    $tmpstr = get_abssum(cutoff_thetas => \@cutoff_thetas);

    unshift @new_code, @{parse_row(parse_str => $blank . $tmpstr,
                                   parse_operator => "+",
                                   max_length => $row_length)};
    ## Set the initial t-value and make it a fixed variable

    $nthetas++;
    my $t_thetanumber=$nthetas;

    unshift @new_code, $blank . "TVALUE  = THETA(".$nthetas.")\n";
    $lasso_model->initial_values(parameter_type => 'theta',
                                 parameter_numbers => [[$nthetas]],
                                 new_values =>[[$t_value]],
                                 add_if_absent => 1);
    $lasso_model -> fixed(parameter_type => 'theta',
                          parameter_numbers => [[$nthetas]],
                          new_values => [[1]] );
    $lasso_model -> labels( parameter_type     => 'theta',
                            parameter_numbers => [[$nthetas]],
                            new_values        => [['TH'.($nthetas)." T-VALUE"]] );

    unshift @new_code, $blank . "\n;;; LASSO-BEGIN\n";

    ## Add the multiplication of the Typical Values with the
    #covariate for all params.
    add_tv_multiplication(code => \@old_code, parameters =>[keys %parameter_covariate_form], log_scale=> $log_scale);

    ## Merge the old_code and new_code
    @new_code = (@new_code, @old_code);

    ## Set the new code to the new model

    if ($use_pred) {
        $lasso_model->set_code(record => 'pred', code => \@new_code);
    } else {
        $lasso_model->set_code(record => 'pk', code => \@new_code);
    }
    return ($use_pred,\@cutoff_thetas,$t_thetanumber,\@weight_thetas,$lambda_theta,\@cutoff_thetas_labels);
}

sub add_tv_multiplication
{
    my %parm = validated_hash(\@_,
                              code => { isa => 'ArrayRef', optional => 0 },
                              parameters => { isa => 'ArrayRef', optional => 0 },
                              log_scale => { isa => 'Bool', optional => 0 },
        );
    my $code = $parm{'code'};
    my $parameters = $parm{'parameters'};
    my $log_scale = $parm{'log_scale'};
    #handle if/else clauses here, like in scm

    my $var='TV';
    my $sign = '*';
    if ($log_scale){
        $var = 'LNTV';
        $sign = '+';
    }

    my $success;
    foreach my $parameter (sort {lc($a) cmp lc($b)} @{$parameters}){
        $success = 0;
        for ( reverse @{$code} ) {
            #want to find last occurrence of TVpar set
            if ( /[^A-Z0-9_]*$var(\w+)\s*=\s*/ and $1 eq $parameter){
                #add new definition line after last occurence
                $_ = $_."\n".
                    $blank."$var$parameter = $var$parameter"."$sign$parameter"."COV\n";
                $success = 1;
                last; #only change the last line where appears
            }
        }
        unless ( $success ) {
            croak("Could not determine a good place to add the covariate relation.\n".
                  " i.e. No $var$parameter was found\n" );
        }
    }

}


sub xv_step_init
{
    my $self = shift;
    #self here will be xv_step object, shift gets single parameter given in xv_step_subs
    sub print_log {
        my $filename = shift;
        my $message  = shift;
        open(LOG, ">>$filename") or die "Can not create logfile: $filename\n$!";
        print LOG $message;
        close LOG;
    }
    my @estimation_models = @{$self->estimation_models()};
    my @prediction_models = @{$self->prediction_models()};
    my $own_parameters = $self->own_parameters();
    my $counter=0;

    print_log ($own_parameters->{'logfile'}->[0],  "Last t-value: " .$own_parameters->{'last_t_value'} ."\n");

    my $new_t_value = $own_parameters->{'last_t_value'} + $own_parameters->{'steplength'};
    foreach my $model ( @prediction_models){
        $model->initial_values(parameter_type => 'theta',
                               parameter_numbers => [[$own_parameters->{'t_thetanumber'}]],
                               new_values =>[[$new_t_value]]);
        $model->_write(overwrite => 1);
    }


    foreach my $model (@estimation_models) {
        #set new t-value
        $model->initial_values(parameter_type => 'theta',
            parameter_numbers => [[$own_parameters->{'t_thetanumber'}]],
            new_values =>[[$new_t_value]]);

        #Set the new MSFO file
        my $MSFO_file = "lasso_t_". $new_t_value ."_" .($counter++).".msfo1";

        # -1 means last record
        $model -> rename_msfo(name => $MSFO_file, add_if_absent => 1);

        $model->_write(overwrite => 1);
    }

    print_log ($own_parameters->{'logfile'}->[0],  "Last OFV sum: " . $own_parameters->{'last_ofv_sum'} ."\n");
    $own_parameters -> {'last_t_value'}+=$own_parameters->{'steplength'};
}

sub xv_step_analyze
{
    my $self = shift;
    my $retur;

    #self here will be xv_step object

    $retur = 1;

    my @estimation_models = @{$self->estimation_models()};
    my @prediction_models = @{$self->prediction_models()};
    my $own_parameters = $self->own_parameters();

    my @est_strings;

    my $sum_ofv = 0;
    my $fh1;
    my $j=1;
    open($fh1,  ">>", $own_parameters->{'pred_filename'});
    foreach my $pred_model (@prediction_models) {
        #use Data::Dumper;
        #print Dumper $pred_model -> outputs -> [0];
        if (defined $pred_model -> outputs -> [0] and $pred_model->outputs->[0]->have_output) {
            my $ofv =  $pred_model -> outputs -> [0] -> get_single_value(attribute => 'ofv');
            if (defined $ofv){
                $sum_ofv += $ofv;
            } else {
                $retur = 0;
                die "No defined ofv from the pred model!\n";
            }
            printf $fh1 "%-12.4f %-6.3f %-5d %-10d\n", $ofv, $own_parameters -> {'last_t_value'}, $j, $own_parameters->{'seed'};

            $j++;
        } else {
            $retur = 0;
            die "No defined output from the pred model!\n";
        }
    }
    printf $fh1 "%-12.4f %-6.3f %-5s %-10d\n", $sum_ofv, $own_parameters -> {'last_t_value'}, "All", $own_parameters->{'seed'};

    close($fh1);

    my @est_ofv;
    my $i=1;
    my $fh;
    print "\n" unless (ui -> silent());
    foreach my $est_model (@estimation_models){
        if (defined $est_model -> outputs -> [0] and $est_model->outputs->[0]->have_output){
            my $ofv =  $est_model -> outputs -> [0] -> get_single_value(attribute => 'ofv');
            if (defined $ofv){
                my $warning = '';
                unless (defined $est_model->outputs->[0]->get_single_value (attribute => 'minimization_successful')
                        and $est_model->outputs->[0]->get_single_value (attribute => 'minimization_successful')){
                    $warning = ' *minimization unsuccessful';
                    if (defined $est_model->outputs->[0]->get_single_value (attribute => 'rounding_errors')
                            and $est_model->outputs->[0]->get_single_value (attribute => 'rounding_errors')){
                        if ($self->significant_digits_accept() and
                            (defined $est_model->outputs->[0] -> get_single_value(attribute =>'significant_digits',
                                    problem_index => 0)) and
                            $est_model->outputs->[0]-> get_single_value(attribute =>'significant_digits',
                                problem_index => 0)>= $self->significant_digits_accept()){
                            my $dig = $est_model->outputs->[0]-> get_single_value(attribute =>'significant_digits',
                                problem_index => 0);
                            $warning = ' *minimization with rounding errors accepted, SIGDIG_'.$dig;
                        }else{
                            $warning .= ', rounding errors';
                            $self->warnings($self->warnings()+1);
                        }
                    }else{
                        $self->warnings($self->warnings()+1);
                    }
                }

                print_log ($own_parameters->{'logfile'}->[0],  "Estimation OFV in validation group $i: ". $ofv ."$warning\n");
                ui -> print( category => 'lasso',
                    message  => "Estimation OFV group $i: ". $ofv ."$warning");

                push @est_ofv, $ofv;
                push @est_strings,sprintf("%-12.4f %-6.3f %-5d %-10d", $ofv, $own_parameters -> {'last_t_value'}, $i, $own_parameters->{'seed'});

                $i++;

            }else {
                $retur = 0;
            }
        } else {
            $retur = 0;
        }
    }

    if (defined $own_parameters->{'est_ofv'}) {
        for (my $i = 0; $i <= $#est_ofv; $i++){
            if ($own_parameters->{'steplength'}>0){
                if ( $own_parameters->{'est_ofv'}->[$i] <= $est_ofv[$i]){
                    print_log ($own_parameters->{'logfile'}->[0],  "Warning: estimation OFV in group ".($i+1).
                        " did not decrease when increasing t compared to previous step\n");
                    $self->warnings($self->warnings()+1);
                }
            }else{
                if ( $own_parameters->{'est_ofv'}->[$i] >= $est_ofv[$i]){
                    print_log ($own_parameters->{'logfile'}->[0],  "Warning: estimation OFV in group ".($i+1).
                        " did not increase when decreasing t compared to previous step\n");
                    $self->warnings($self->warnings()+1);
                }
            }

        }
    }
    $own_parameters->{'est_ofv'} = \@est_ofv;

    my $mess = "Prediction OFV at t=".sprintf("%-12.2f",$own_parameters -> {'last_t_value'})." : $sum_ofv";
    print_log ($own_parameters->{'logfile'}->[0],  "$mess\n");
    ui -> print( category => 'lasso', message  => $mess);

    open($fh, ">>", $own_parameters->{'coeff_table'});
    $i=1;
    foreach my $pred_model (@prediction_models) {
        if (defined $pred_model -> outputs -> [0] and $pred_model->outputs->[0]->have_output) {
            my $cutoff_thetas = $self->subtool_arguments()->{'modelfit'}->{'cutoff_thetas'};
            my $init_val = $pred_model -> initial_values( parameter_type    => 'theta',
                parameter_numbers => [$cutoff_thetas])->[0];
            my $abssum = 0;
            foreach my $value (@{$init_val})  {
                $abssum+=abs($value);
            }

            my $factor = exp(1-($abssum/$own_parameters -> {'last_t_value'}));
            $est_strings[$i-1] = $est_strings[$i-1] . sprintf(" %-6.4f",$factor);
            my $labels = $pred_model->labels( parameter_type  => 'theta', problem_numbers => [1],
                parameter_numbers => [$cutoff_thetas] )->[0];
            my $j=0;
            foreach my $label1 (@{$labels})  {
                my @tmp = split(' ',$label1);
                my $value = @{$init_val}[$j]*$factor;
                printf $fh "%-6.3f %-5d %-6s %-10.7f %-10d\n",$own_parameters -> {'last_t_value'}, $i, $tmp[1], $value, $own_parameters->{'seed'};
                $j++;
            }
            $i++;
        } else {
            $retur = 0;
        }
    }
    close($fh);

    open($fh,  ">>", $own_parameters->{'est_filename'});
    $i=1;
    foreach my $est_model (@estimation_models){
        if (defined $est_model -> outputs -> [0] and $est_model->outputs->[0]->have_output
                and defined $est_model->outputs->[0]->get_single_value (attribute => 'minimization_successful')
                and $est_model->outputs->[0]->get_single_value (attribute => 'minimization_successful')) {
            if (defined $est_strings[$i-1])  {
                printf $fh $est_strings[$i-1] .sprintf(" %-3s\n",'OK');
            }
        }elsif (defined $est_model -> outputs -> [0] and $est_model->outputs->[0]->have_output
                and $self->significant_digits_accept() and
            (defined $est_model->outputs->[0] -> get_single_value(attribute =>'significant_digits',
                    problem_index => 0)) and
            $est_model->outputs->[0]-> get_single_value(attribute =>'significant_digits',
                problem_index => 0)>= $self->significant_digits_accept()){
            my $dig = $est_model->outputs->[0]-> get_single_value(attribute =>'significant_digits',
                problem_index => 0);
            if (defined $est_strings[$i-1])  {
                printf $fh $est_strings[$i-1] .sprintf(" %-3s\n",'SIGDIG_'.$dig);
            }
        }elsif (defined $est_model -> outputs -> [0] and $est_model->outputs->[0]->have_output
                and defined $est_model->outputs->[0]->get_single_value (attribute => 'rounding_errors')
                and $est_model->outputs->[0]->get_single_value (attribute => 'rounding_errors')){
            if (defined $est_strings[$i-1]) {
                print $fh $est_strings[$i-1] . sprintf(" %-3s\n",'ROUND_ERR');
            }
        } else {
            if (defined $est_strings[$i-1]) {
                print $fh $est_strings[$i-1] . sprintf(" %-3s\n",'ERROR');
            }
        }
        $i++;
    }
    close($fh);

    if ($own_parameters->{'last_ofv_sum'}<=$sum_ofv) {
        print_log ($own_parameters->{'logfile'}->[0], "OFV-sum is larger than the previous step\n");
        #do not change t_optimal
        $retur = 0 if ($own_parameters->{'converge'} eq "FIRSTMIN");
    } else {
        print_log ($own_parameters->{'logfile'}->[0],  "Found a better ofv $sum_ofv\n");
        if ($retur!=0) {#If all pred and est did terminate
            $own_parameters->{'t_optimal'} = $own_parameters->{'last_t_value'};
            ui -> print( category => 'lasso',
                message  => "T-optimal is now " .$own_parameters->{'t_optimal'});
            print_log ($own_parameters->{'logfile'}->[0], "T-optimal is now " .$own_parameters->{'t_optimal'} ."\n");
        } else {
            print_log ($own_parameters->{'logfile'}->[0], "Not all pred and est did terminate\n");
        }
        $own_parameters->{'last_ofv_sum'}=$sum_ofv;
    }

    if (defined $own_parameters->{'stop_t'} && $own_parameters->{'steplength'}>0
        && $own_parameters->{'last_t_value'}>=$own_parameters->{'stop_t'}) {
        print_log ($own_parameters->{'logfile'}->[0], "t-value greater or equal to " .$own_parameters->{'stop_t'} . "\n");
        $retur = 0 if ($own_parameters->{'converge'} ne "HALT");
    }

    if (defined $own_parameters->{'stop_t'} && $own_parameters->{'steplength'}<0
        && $own_parameters->{'last_t_value'}<=$own_parameters->{'stop_t'}) {
        print_log ($own_parameters->{'logfile'}->[0],"t-value smaller or equal to " .$own_parameters->{'stop_t'} . "\n");
        $retur = 0 if ($own_parameters->{'converge'} ne "HALT");
    }

    return $retur;
}

sub write_log
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        message => { isa => 'Str', optional => 1 }
    );
    my $message = $parm{'message'};

    open(LOG, ">>".$self->logfile->[0]) or
    die "Cannot open logfile: " . $self->logfile->[0] . "\n";
    print LOG $message."\n";
    close LOG;
}

sub general_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 },
        class => { isa => 'Str', optional => 1 },
        subm_threads => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my $class = $parm{'class'};
    my $subm_threads = $parm{'subm_threads'};
}

sub parse_relations
{
    #have unit test
    my %parm = validated_hash(\@_,
                              relations => { isa => 'Str', optional => 0 },
    );
    my $relations = $parm{'relations'};
    #a hash {parameter}{covariate}{form}
    my %parameter_covariate_form;

    my @sets = split( ',,' , $relations);
    #example relations
    #CL:WGT-2,SEX-1,RACE-1,,V:WGT-3-45.2,,KA:WGT-3,APGR-2
    my %breakpoints;
    foreach my $set (@sets){
        my @parlist = split (':',$set);
        croak("Error parsing relations: expected exactly one : in $set but found ".
              (scalar(@parlist)-1)) unless (scalar(@parlist)==2);
        my $parameter=$parlist[0];
        if (defined $parameter_covariate_form{$parameter}){
            croak("Error parsing relations: $parameter found twice ");
        }
        my @list = split (',',$parlist[1]);
        foreach my $covform (@list){
            my @pair = split ('-',$covform);
            my $breakpoint;
            if (scalar(@pair)==3){
                unless ($pair[1] eq '3'){
                    croak("Can only specify breakpoint (number after second dash) if ".
                          "parameterization is 3, but found $covform ".
                          "where parameterization is ".$pair[1])    ;
                }
                $breakpoint = $pair[2];
            }else{
                unless (scalar(@pair)==2){
                    croak("Error parsing relations: expected exactly one - in $covform but found ".
                          (scalar(@pair)-1))    ;
                }
            }
            my $covariate=$pair[0];
            my $function=$pair[1];

            unless ($function =~ /^(1|2|3)$/){
                croak("Error parsing relations: parameterization in $covform must be in the set [1,2,3]");
            }
            if (defined $parameter_covariate_form{$parameter}{$covariate}){
                croak("Error parsing relations: Multiple definitions of relation $parameter:$covariate");
            }
            $parameter_covariate_form{$parameter}{$covariate}{'form'}=$function;
            $breakpoints{$parameter.':'.$covariate}=$breakpoint;
        }
    }

    return (\%parameter_covariate_form,\%breakpoints);

}
sub setup_covariates
{
    my %parm = validated_hash(\@_,
                              relations => { isa => 'Str', optional => 0 },
                              data => { isa => 'data', optional => 0 },
                              model => { isa => 'model', optional => 0 },
    );
    my $relations = $parm{'relations'};
    my $data = $parm{'data'};
    my $model = $parm{'model'};

    #a hash {parameter}{covariate}{form}
    my ($parameter_covariate_form,$breakpoints)=parse_relations(relations => $relations);
    my $statistics = {}; #hashref

    foreach my $parameter (keys %{$parameter_covariate_form}){
        foreach my $covariate (keys %{$parameter_covariate_form->{$parameter}}){
            my $column_number;
            my ( $values_ref, $positions_ref ) = $model ->_get_option_val_pos ( problem_numbers => [1],
                                                                                name        => $covariate,
                                                                                record_name => 'input',
                                                                                global_position => 1  );
            $column_number = $positions_ref -> [0];
            croak("Cannot find $covariate in \$INPUT" )    unless ( defined $column_number );

            my $function = $parameter_covariate_form->{$parameter}->{$covariate}->{'form'};
            my $breakpoint;
            if ($function == 3){
                $breakpoint = $breakpoints->{$parameter.':'.$covariate}; #can be undef, allowed
            }
            unless (defined $statistics->{$covariate}{$function}){
                #unless already computed this statistic
                $statistics->{$covariate}{$function} =     $data -> lasso_calculate_covariate_statistics(
                    missing_data_token =>$data->missing_data_token,
                    column_number => $column_number,
                    function => $function,
                    breakpoint => $breakpoint);
            }
        }
    }

    return ($parameter_covariate_form,$statistics);
}

sub get_covrecord
{
    my %parm = validated_hash(\@_,
                              model => { isa => 'model', optional => 0 },
                              adjusted => {isa => 'Bool',optional => 0}
        );
    my $model = $parm{'model'};
    my $adjusted = $parm{'adjusted'};

    my $covrecordref=[];
    if ($adjusted){
        if (defined $model->problems->[0]->covariances and scalar(@{$model->problems->[0]->covariances})>0){
            $covrecordref = $model->problems->[0]->covariances->[0] -> _format_record() ;
            for (my $i=0; $i<scalar(@{$covrecordref}); $i++){
                $covrecordref->[$i] =~ s/^\s*\$CO[A-Z]*\s*//; #get rid of $COVARIANCE
                $covrecordref->[$i] =~ s/\s*$//; #get rid of newlines
            }
        }else{
            $covrecordref = ['PRINT=E'];
        }
    }
    return $covrecordref;
}

sub run_full_model
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 }
    );
    my $model = $parm{'model'};

    my @al_coeff=();

    my $message = "Estimating full model to get standard errors for weights in adjusted adaptive lasso";

    my $orig_fit =
        tool::modelfit->new( %{common_options::restore_options(@common_options::tool_options)},
                             base_directory     => $self ->directory(),
                             directory         => $self ->directory().
                             '/full_modelfit_dir1',
                             models         => [$model],
                             threads               => 1,
                             nm_output => 'ext,cov,coi,cor,phi',
                             logfile             => undef,
                             raw_results           => undef,
                             rplots => -1,
                             raw_results_append => 0,
                             raw_results_file => [$self->raw_results_file->[0]],
                             _raw_results_callback => $self->_optimal_lasso_raw_results_callback(model_number => 1),
                             prepared_models       => undef,
                             top_tool              => 0);

    ui -> print( category => 'lasso',
                 message => $message );

    $orig_fit -> run;

    return get_adjusted_al_coefficients(model=> $model,    cutoff_thetas => $self->cutoff_thetas);
}

sub update_al_coefficients
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        cutoff_thetas => { isa => 'ArrayRef', optional => 0 },
        coefficients => { isa => 'ArrayRef', optional => 0 },
    );
    my $model = $parm{'model'};
    my $cutoff_thetas = $parm{'cutoff_thetas'};
    my $coefficients = $parm{'coefficients'};
    #FIXME assumes previous coeffs == 1
    for(my $i=0; $i<scalar (@{$cutoff_thetas}); $i++){
        update_adaptive_theta(model=>$model,
                              coefficient => $coefficients->[$i],
                              thetanumber => $cutoff_thetas->[$i],
                              al_thetanumber => ($cutoff_thetas->[$i]+1));
    }
}

sub get_adjusted_al_coefficients
{
    my %parm = validated_hash(\@_,
        model => { isa => 'model', optional => 0 },
        cutoff_thetas => { isa => 'ArrayRef', optional => 0 }
    );
    my $model = $parm{'model'};
    my $cutoff_thetas = $parm{'cutoff_thetas'};
    my @al_coeff;

    my $output = $model -> outputs -> [0];
    unless (defined $output){
        croak("No output object from full model");
    }
    unless ( defined $output->problems and scalar(@{$output->problems})>0 ) {
        $output -> _read_problems;
    }
    unless ($output->get_single_value(attribute => 'covariance_step_successful')){
#    unless (0){
        my @labels = ();
        foreach my $thnum (@{$cutoff_thetas}){
            push(@labels,'THETA'.$thnum);
        }
        croak("Covariance step of full model ".$model->filename." failed, cannot proceed\n".
              "You must give abs(estimate)/(standard error) for full model parameters\n".
              join(', ',@labels)."\n".
              "as input to a new adaptive lasso run using option -al_coefficients.\n".
              "The order must be as above.\n");
    }
    my $coordinate_strings = $model->problems->[0]->get_estimated_attributes(parameter => 'theta',
                                                                             attribute => 'coordinate_strings');
    my $estimates = $output->get_filtered_values(parameter=>'theta',category=>'estimate');
    my $ses = $output->get_filtered_values(parameter=>'theta',category=>'se');

    foreach my $thnum (@{$cutoff_thetas}){
        my $label = 'THETA'.$thnum;
        for(my $i=0; $i<scalar (@{$coordinate_strings}); $i++){
            if ($label eq $coordinate_strings->[$i]){
                if ($ses->[$i] == 0){
                    croak("standard error for $label is 0, cannot compute weights for adjusted adaptive lasso");
                }
                push(@al_coeff,abs($estimates->[$i])/$ses->[$i]);
                last;
            }
        }
    }

    unless (scalar(@al_coeff) == scalar(@{$cutoff_thetas})){
        croak("did not find estimate and se for all cutoff thetas");
    }

    return \@al_coeff;
}

sub compute_lasso_coefficients
{
    my %parm = validated_hash(\@_,
                              al_coefficients => { isa => 'ArrayRef', optional => 0 },
                              theta_estimates => { isa => 'ArrayRef', optional => 0 },
                              factor => { isa => 'Num', optional => 0 },
                              cutoff => { isa => 'Num', optional => 0 },
    );
    my $al_coefficients = $parm{'al_coefficients'};
    my $theta_estimates = $parm{'theta_estimates'};
    my $factor = $parm{'factor'};
    my $cutoff = $parm{'cutoff'};

    my @lasso=();

    for (my $i=0; $i<scalar(@{$theta_estimates}); $i++){
        my $coeff;
        if (($al_coefficients->[$i] != 0) and abs($theta_estimates->[$i]) > ($cutoff/($al_coefficients->[$i]))){
            $coeff=$factor*($theta_estimates->[$i])*($al_coefficients->[$i]);
        }else{
            $coeff=0;
        }
        push(@lasso,$coeff);
    }
    return \@lasso;
}

sub modelfit_setup
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    # Assume one $PROBLEM one model
    my $model = $self -> models -> [0];
    my $n_original_thetas = $model->nthetas;

    my $dataobj = data->new(filename => $model->datafiles(absolute_path=>1)->[0],
                            idcolumn => $model->idcolumn,
                            missing_data_token => $self->missing_data_token,
                            ignoresign => $model->ignoresigns->[0]);

    ui -> print( category => 'lasso',
        message  => "Parsing relations and calculating covariate statistics" );

    my ($ref1,$ref2) = setup_covariates(relations => $self->relations,
                                        data => $dataobj,
                                        model => $model);

    #a hash {parameter}{covariate}{form}
    my %parameter_covariate_form = %{$ref1};
    $self->statistics($ref2);

    $dataobj = undef;
    ui -> print( category => 'lasso',
        message  => " ... done\n" );


    if(defined $self -> covariate_statistics_file){
        open( STAT, '>'.$self-> covariate_statistics_file );
        $Data::Dumper::Purity = 1;
        print STAT Dumper %{$self -> statistics}; #or give ref here??
        $Data::Dumper::Purity = 0;
        close( STAT );
    }

    $self->NOABORT_added(0);

    unless ($model->is_option_set(record => 'estimation',
            name   => 'NOABORT')) {
        $model->set_option(record_name => 'estimation',
            option_name => 'NOABORT');
        $self->NOABORT_added(1);
    }

    my $covrecordref = get_covrecord(adjusted => $self->adjusted,
                                     model => $model);
    $model -> remove_records( type => 'covariance');

    my $basic_model= $model->copy(filename => $self -> directory().'m'.$model_number.'/basic.mod',
                                  copy_datafile => 0,
                                  copy_output => 0);

    my $lasso_model= $model->copy(filename => $self -> directory().'m'.$model_number.'/lasso_initial.mod',
                                  copy_datafile => 0,
                                  copy_output => 0,
                                  output_same_directory => 1,
                                  write_copy => 0);

    my ($usepred,$cutoffref,$t_theta,$weightref,$lambda_theta,$labelsref) =
        setup_lasso_model(lasso_model => $lasso_model,
                          parameter_covariate_form => \%parameter_covariate_form,
                          t_value => $self->start_t,
                          statistics => $self->statistics,
                          normalize => $self->normalize,
                          log_scale => $self->log_scale,
                          missing_data_token => $self->missing_data_token,
                          adaptive => $self->adaptive);
    $self->use_pred($usepred);
    $self->cutoff_thetas($cutoffref);
    $self->cutoff_thetas_labels($labelsref);
    $self->t_thetanumber($t_theta);
    $self->weight_thetas($weightref);
    $self->lambda_thetanumber($lambda_theta);

    #create xv data, use relative path to this directory when running nonmem
    if (defined $self->stratify_on()){
        my $column_number;
        my ( $values_ref, $positions_ref ) = $model ->
        _get_option_val_pos ( problem_numbers => [1],
            name        => $self->stratify_on(),
            record_name => 'input',
            global_position => 1  );
        $column_number = $positions_ref -> [0];
        croak("Cannot find ".$self->stratify_on()." in \$INPUT" )
        unless ( defined $column_number );
        $self->stratify_on($column_number);
    }

    my $data_xv_step = tool::xv_step -> new(
        %{common_options::restore_options(@common_options::tool_options)},
        models => [$lasso_model],
        nr_validation_groups => $self->groups(),
        stratify_on      => $self -> stratify_on(),
        base_directory => $self->directory(),
        directory => $self->directory().'xv_data');
    $data_xv_step -> create_data_sets();



    my $full_model;
    my $al_coefficients;
    my $rawres_append=0;
    if ($self->adjusted){
        my $full_model= $lasso_model->copy(filename => $self -> directory().'m'.$model_number.'/full_model.mod',
                                           copy_datafile => 0,
                                           output_same_directory=> 1,
                                           copy_output => 0,
                                           write_copy => 0);
        setup_full_model(model=>$full_model,
                         covariance => $covrecordref,
                         use_pred => $usepred);
        $full_model->_write();

        if (defined $self->al_coefficients){
            my @values = split(',',$self->al_coefficients);
            unless(scalar(@values) == scalar(@{$self->cutoff_thetas})){
                croak("Input error al_coefficients: need ".scalar(@{$self->cutoff_thetas}).
                    " values in comma-separated list, but ".scalar(@values)." were given" );
            }
            foreach my $val (@values){
                unless ($val > 0){
                    croak("Input error al_coefficients: all values must be positive");
                }
            }
            $al_coefficients = \@values;
        }else{
            #run full model, get SE:s and compute coefficients, one value per cutoff_theta
            $al_coefficients = $self->run_full_model(model=> $full_model);
            $rawres_append=1; #FIXME
        }
        #update AL factors in lasso_model
        update_al_coefficients(model => $lasso_model,
                               cutoff_thetas => $self->cutoff_thetas,
                               coefficients => $al_coefficients);
    }else{
        $al_coefficients=[(1) x scalar(@{$cutoffref})];
    }
    $lasso_model->_write();

    #prepare log files
    my $est_filename =  $self->directory() .  "/est_ofv.log";
    my $pred_filename = $self->directory() .  "/pred_ofv.log";
    my $coeff_table =   $self->directory() .  "/coeff_table.log";
    my ($fh1,$fh2,$fh3);
    open($fh1,  ">", $est_filename);
    open($fh2 ,">", $pred_filename);
    open($fh3, ">", $coeff_table);

    printf $fh1 "%-12s %-6s %-5s %-10s %-6s %-3s\n", "Est_OFV", "t", "Group", "Seed", "Factor", "Termination";
    printf $fh2 "%-12s %-6s %-5s %-10s\n", "Pred_OFV", "t", "Group", "Seed";
    printf $fh3 "%-6s %-5s %-6s %-10s %-10s\n", "t", "Group", "COV", "Coeff", "Seed";

    close $fh1;
    close $fh2;
    close $fh3;

    #run the basic step unless defined pred_ofv_start_t (last_ofv_sum)

    my $common_seed = $self->seed();
    if (not defined $self->pred_ofv_start_t()){
        ui -> print( category => 'lasso',
            message  => "Running the basic xv_step (option pred_ofv_start_t undefined).\n" );
        my $basic_step =  tool::xv_step -> new(
            directory => $self->directory().'basic_xv_step',
            cutoff => $self->cutoff(),
            n_model_thetas => $basic_model->nthetas(),
            nr_validation_groups => $self->groups(),
            stratify_on      => $self -> stratify_on(),
            models => [$basic_model],
            prediction_data => $data_xv_step->prediction_data(),
            estimation_data => $data_xv_step->estimation_data(),
            subtool_arguments => { modelfit => {
                    %{common_options::restore_options(@common_options::tool_options)},
                    directory=> undef,
                    seed => $common_seed,
                    copy_data => 0,
                    top_tool => 0,
                    prepend_model_file_name => 1
                }
            });
        my $return_val = $basic_step->run();
        my @basic_pred_models = @{$basic_step->prediction_models()};
        my @basic_est_models = @{$basic_step->estimation_models()};

        my $sum_ofv = 0;
        my $j=1;
        my $fh;

        $j=1;
        open($fh,  ">>", $est_filename);
        foreach my $basic_mod (@basic_est_models)  {
            if (defined $basic_mod -> outputs -> [0] and $basic_mod->outputs->[0]->have_output) {
                my $ofv =  $basic_mod -> outputs -> [0] -> get_single_value(attribute => 'ofv');
                printf $fh "%-12.4f %-6.3f %-5d %-10d %-6s %-3s\n", $ofv, $self->start_t(), $j, $common_seed,'-', 'OK';
            } else {
                die("The basic estimation model nr $j, did not terminate\n");
            }
            $j++;
        }
        close($fh);


        $j=1;
        open($fh,  ">>", $pred_filename);
        foreach my $basic_mod (@basic_pred_models) {
            if (defined $basic_mod -> outputs -> [0] and $basic_mod->outputs->[0]->have_output) {
                my $ofv =  $basic_mod -> outputs -> [0] -> get_single_value(attribute => 'ofv');
                $sum_ofv += $ofv;

                printf $fh "%-12.4f %-6.3f %-5d %-10d\n", $ofv,$self->start_t(), $j, $common_seed;
            } else {
                die("The basic prediction model nr $j, did not terminate\n");
            }
            $j++;
        }

        $self->pred_ofv_start_t($sum_ofv);

        printf $fh "%-12.4f %-6.3f %-5s %-10d\n", $sum_ofv, $self->start_t(), "All", $common_seed;
        close($fh);
        my $mess = "Prediction OFV at t=".sprintf("%-12.2f",$self->start_t())." : $sum_ofv\n";
        print_log ($self->logfile->[0],  $mess);
        ui -> print( category => 'lasso',
            message  => "\n$mess");
    }

    my ($lasso_optimal,$t_optimal) = $self->run_lasso_iteration(iteration => 1,
                                                                append => $rawres_append,
                                                                common_seed =>  $common_seed,
                                                                est_filename =>  $est_filename,
                                                                pred_filename =>  $pred_filename,
                                                                basic_model =>  $basic_model,
                                                                lasso_model =>  $lasso_model,
                                                                coeff_table =>  $coeff_table,
                                                                data_xv_step=>  $data_xv_step);
    return unless (defined $lasso_optimal);

    #lasso optimal is written and run and updated with final estimates but not rewritten


    my $lasso_coefficients = compute_lasso_coefficients( al_coefficients => $al_coefficients,
                                                         theta_estimates => $self->cutoff_thetas_estimates,
                                                         factor => $self->factor_estimate,
                                                         cutoff => $self->cutoff);
    my $modeloptimal =  setup_regular_model(lasso_model => $lasso_optimal,
                                            directory => $self->directory.'m1/',
                                            adjusted => $self->adjusted,
                                            normalize => $self->normalize,
                                            iteration => 1,
                                            number => $self->evaluation_number,
                                            cutoff_thetas => $self->cutoff_thetas,
                                            cutoff_thetas_labels => $self->cutoff_thetas_labels,
                                            adaptive => $self->adaptive,
                                            use_pred => $self->use_pred,
                                            lasso_coefficients => $lasso_coefficients);
    $modeloptimal->_write;
    push(@{$self->model_optimal},$modeloptimal);



    if ($self->factor_estimate <0.9 or $self->factor_estimate >1.1) {
        $self->write_log(message =>"WARNING: Factor for the final lasso model: ".$self->factor_estimate);
        $self->warnings($self->warnings()+1);
    }
    write_coefficients(labels => $self->cutoff_thetas_labels,
                       factor => $self->factor_estimate,
                       convergence => (($t_optimal < $self->stop_t)? 1 : 0 ),
                       coefficients => $lasso_coefficients,
                       directory => $self->directory,
                       filename => 'lasso_coefficients.csv');

    if (defined $self->external_data){
        my $evaluation_model = setup_regular_model(lasso_model => $lasso_optimal,
                                                      directory => $self->directory.'m1/',
                                                      adjusted => $self->adjusted,
                                                      normalize => $self->normalize,
                                                      iteration => 1,
                                                      number => $self->evaluation_number,
                                                      external_data => $self->external_data,
                                                      cutoff_thetas => $self->cutoff_thetas,
                                                      cutoff_thetas_labels => $self->cutoff_thetas_labels,
                                                      adaptive => $self->adaptive,
                                                      use_pred => $self->use_pred,
                                                      lasso_coefficients => $lasso_coefficients);
        $evaluation_model->_write;
    }

    if ($self->adaptive and not ($self->adjusted)){
        #reset t to 0, reset pred inits etc
        @{$al_coefficients} = @{$lasso_coefficients}; #HERE

        my $iteration = 1;
#        foreach my $exponent ((1)){
        my $lasso_adaptive = $lasso_optimal -> copy(filename => $self->directory()."m1/lasso_adaptive_".$iteration.".mod",
                                                    copy_datafile => 0,
                                                    output_same_directory => 1,
                                                    write_copy => 0,
                                                    copy_output => 0);
        update_al_coefficients(model => $lasso_adaptive,
                               cutoff_thetas => $self->cutoff_thetas,
                               coefficients => $al_coefficients);
        $lasso_adaptive->_write;


        $iteration++;
        #run xv again
        $lasso_optimal = undef;
        ($lasso_optimal,$t_optimal) = $self->run_lasso_iteration(iteration => $iteration,
                                                                 append => 1,
                                                                 common_seed =>  $common_seed,
                                                                 est_filename =>  $est_filename,
                                                                 pred_filename =>  $pred_filename,
                                                                 basic_model =>  $basic_model,
                                                                 lasso_model =>  $lasso_adaptive,
                                                                 coeff_table =>  $coeff_table,
                                                                 data_xv_step=>  $data_xv_step);
        return unless (defined $lasso_optimal);


        $lasso_coefficients = compute_lasso_coefficients( al_coefficients => $al_coefficients,
                                                          theta_estimates => $self->cutoff_thetas_estimates,
                                                          factor => $self->factor_estimate,
                                                          cutoff => $self->cutoff);
        #FIXME change sub
        my $modeloptimal2 = setup_regular_model(lasso_model => $lasso_optimal,
                                                directory => $self->directory.'m1/',
                                                adjusted => $self->adjusted,
                                                normalize => $self->normalize,
                                                iteration => $iteration,
                                                number => $self->evaluation_number,
                                                cutoff_thetas => $self->cutoff_thetas,
                                                cutoff_thetas_labels => $self->cutoff_thetas_labels,
                                                adaptive => $self->adaptive,
                                                use_pred => $self->use_pred,
                                                lasso_coefficients => $lasso_coefficients);

        $modeloptimal2->_write;
        push(@{$self->model_optimal},$modeloptimal2);


        if ($self->factor_estimate<0.9 or $self->factor_estimate>1.1) {
            $self->write_log(message =>"WARNING: Factor for the final lasso model: ".$self->factor_estimate);
            $self->warnings($self->warnings()+1);
        }
        write_coefficients(labels => $self->cutoff_thetas_labels,
                           factor => $self->factor_estimate,
                           convergence => (($t_optimal < $self->stop_t)? 1 : 0 ),
                           coefficients => $lasso_coefficients,
                           filename => 'lasso_coefficients.csv',
                           directory => $self->directory);

        if (defined $self->external_data){
            my $evaluation_model = setup_regular_model(lasso_model => $lasso_optimal,
                                                       directory => $self->directory.'m1/',
                                                       adjusted => $self->adjusted,
                                                       normalize => $self->normalize,
                                                       iteration => $iteration,
                                                       number => $self->evaluation_number,
                                                       external_data => $self->external_data,
                                                       cutoff_thetas => $self->cutoff_thetas,
                                                       cutoff_thetas_labels => $self->cutoff_thetas_labels,
                                                       adaptive => $self->adaptive,
                                                       use_pred => $self->use_pred,
                                                       lasso_coefficients => $lasso_coefficients);
            $evaluation_model->_write;
        }
#        } end loop exponents
    }



    $self->tools([]) unless (defined $self->tools);
    if ($self->run_final_model){
        ui -> print( category => 'lasso',
                     message  => "Running normal model with covariate relations added.\n" );

        push( @{$self -> tools},
              tool::modelfit -> new (%{common_options::restore_options(@common_options::tool_options)},
                                     seed => $common_seed,
                                     models    => $self->model_optimal, #arrayref
                                     threads    => 1,
                                     base_directory => $self->directory(),
                                     raw_results_file => [$self->directory().'raw_results_final_model.csv'],
                                     directory => $self->directory().'final_model_modelfit_dir'));
    }
}

sub _optimal_lasso_raw_results_callback
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my ($subroutine,$dir,$file, $nonp_file);


    ($dir,$file)=
        OSspecific::absolute_path( $self ->directory(),
                                   $self -> raw_results_file()->[$model_number-1] );
    ($dir,$nonp_file) =
        OSspecific::absolute_path( $self ->directory(),
                                   $self -> raw_nonp_file()->[$model_number-1] );

    $subroutine = sub {
        my $modelfit = shift;
        my $mh_ref   = shift;
        my %max_hash = %{$mh_ref};

        $modelfit -> raw_results_file([$dir.$file] );
        $modelfit -> raw_nonp_file( [$dir.$nonp_file] );

        my ($start,$len) = split(',',$modelfit->raw_line_structure() -> {1}->{'theta'});
        my $thetaindex=$start;
        croak("could not find theta in raw results header") unless (defined $thetaindex);
#FIXME user prepare results to write coefficients, so that is based on raw results that will be used for updating and eval
        foreach my $row ( @{$modelfit -> raw_results()} ) {
            #only one row ever
            my $abssum=0;
            my @estimates=();
            my @al_vals=();
            foreach my $num (@{$self->cutoff_thetas}){
                $abssum += abs($row->[$thetaindex+$num-1]);
                push(@estimates,$row->[$thetaindex+$num-1]);
                if ($self->adaptive){
                    push(@al_vals,$row->[$thetaindex+$num]);
                }
            }
            my $tval = $row->[$thetaindex+($self->t_thetanumber)-1];
            my $factor;
            if ($tval > 0){
                $factor = exp(1-($abssum/$tval));
            }
            my $type='';
            if ($self->adaptive){
                if ($self->adjusted){
                    if ((defined $self->al_coefficients) or ($modelfit->raw_results_append)){
                        $type = 'aalasso';
                    }else{
                        $type='full';
                        $factor=undef;
                    }
                }else{
                    if (not $modelfit->raw_results_append){
                        $type='lasso';
                    }else{
                        $type = 'alasso';
                    }
                }
            }else{
                $type='lasso';
            }
            my @oldrow =@{$row};
            $oldrow[0]=$type;
            $self->cutoff_thetas_estimates(\@estimates);
            my $lasso_coefficients;
            if (defined $factor){
                $self->factor_estimate($factor) ; #undef for full model
                unless (scalar(@al_vals) == scalar(@estimates)){
                    @al_vals = (1) x scalar(@estimates);
                }
                $lasso_coefficients = compute_lasso_coefficients( al_coefficients => \@al_vals,
                                                                  theta_estimates => $self->cutoff_thetas_estimates,
                                                                  factor => $self->factor_estimate,
                                                                  cutoff => $self->cutoff);
            }else{
                $lasso_coefficients = [(undef) x scalar(@estimates) ];
            }
            $row = [@oldrow[0 .. ($thetaindex-1)],$factor,@{$lasso_coefficients},@oldrow[$thetaindex .. $#oldrow]];
        }
        my @old_header = @{$modelfit -> raw_results_header()};
        my @newlabel =();
        foreach my $label (@{$self->cutoff_thetas_labels}){
            push(@newlabel,'LC_'.$label);
        }
        my $headerindex;
        for (my $k=0; $k<scalar(@old_header);$k++){
            $headerindex = $k if ($old_header[$k] eq 'theta');
        }
        $modelfit -> raw_results_header(
            [@old_header[0 .. ($headerindex-1)],'FACTOR',@newlabel,@old_header[$headerindex .. $#old_header]]);

        my $newcount = 1 + scalar(@newlabel);
        foreach my $mod (sort({$a <=> $b} keys %{$modelfit->raw_line_structure()})){
            foreach my $category (keys %{$modelfit->raw_line_structure() -> {$mod}}){
                next if ($category eq 'line_numbers');
                my ($start,$len) = split(',',$modelfit->raw_line_structure() -> {$mod}->{$category});
                #we know model comes before ofv
                if ($start >= $thetaindex){
                    $modelfit->raw_line_structure() -> {$mod}->{$category} = ($start+$newcount).','.$len; #FACTOR, LC
                }
            }
            $modelfit->raw_line_structure() -> {$mod}->{'factor'} = $thetaindex.','.$newcount;
        }
        $self->raw_line_structure($modelfit -> raw_line_structure());
        $self->raw_line_structure() -> write( $dir.'raw_results_structure' );

        $self -> raw_results_header($modelfit -> raw_results_header());
        $self -> raw_results($modelfit -> raw_results());


    };

    return $subroutine;
}

sub run_lasso_iteration{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              iteration => { isa => 'Int', optional => 0 },
                              append => { isa => 'Bool', optional => 0 },
                              common_seed =>  { isa => 'Str', optional => 0 },
                              est_filename =>  { isa => 'Str', optional => 0 },
                              pred_filename =>  { isa => 'Str', optional => 0 },
                              basic_model =>  { isa => 'model', optional => 0 },
                              lasso_model =>  { isa => 'model', optional => 0 },
                              coeff_table =>  { isa => 'Str', optional => 0 },
                              data_xv_step=>  { isa => 'Ref', optional => 0 },
    );
    my $iteration = $parm{'iteration'};
    my $append = $parm{'append'};
    my $common_seed = $parm{'common_seed'};
    my $est_filename = $parm{'est_filename'};
    my $pred_filename = $parm{'pred_filename'};
    my $basic_model = $parm{'basic_model'};
    my $lasso_model = $parm{'lasso_model'};
    my $coeff_table = $parm{'coeff_table'};
    my $data_xv_step  = $parm{'data_xv_step'};


    my $t_optimal;

    if ($self->start_t() == $self->stop_t()){
        $t_optimal = $self->start_t()
    }else{
        my $message = "Running the cross-validation to find optimal t-value";
        if ($self->adaptive){
            $message .= ' iteration '.$iteration;
        }
        ui -> print( category => 'lasso',
                     message  => "$message.\n" );

        #run the xv

        my $cvobject = tool::xv->new(%{common_options::restore_options(@common_options::tool_options)},
                                     models => [$lasso_model],
                                     base_directory   => $self -> directory,
                                     directory        => $self -> directory.'xv_dir_it'.$iteration,
                                     subtool_arguments =>
                                     {xv_step => {%{common_options::restore_options(@common_options::tool_options)},
                                                  seed => $common_seed,
                                                  directory => undef,
                                                  cutoff => $self->cutoff(),
                                                  n_model_thetas => $basic_model->nthetas(),
                                                  nr_validation_groups => $self->groups(),
                                                  stratify_on      => $self -> stratify_on(),
                                                  estimation_data => $data_xv_step->estimation_data(),
                                                  prediction_data => $data_xv_step->prediction_data(),
                                                  own_parameters => {logfile => $self->logfile,
                                                                     last_t_value => $self->start_t(),
                                                                     steplength => $self->step_t(),
                                                                     basic_model => $basic_model,
                                                                     last_ofv_sum => $self->pred_ofv_start_t(),
                                                                     seed => $self->seed(),
                                                                     stop_t =>  $self->stop_t(),
                                                                     t_optimal => $self->start_t(),
                                                                     converge => $self->convergence(),
                                                                     est_filename => $est_filename,
                                                                     pred_filename => $pred_filename,
                                                                     coeff_table => $coeff_table,
                                                                     t_thetanumber => $self->t_thetanumber},
                                                  init => \&xv_step_init,
                                                  post_analyze =>  \&xv_step_analyze},
                                      modelfit => {%{common_options::restore_options(@common_options::tool_options)},
                                                   seed => $common_seed,
                                                   directory => undef,
                                                   prepend_model_file_name => 1,
                                                   cut_thetas_rounding_errors => 1,
                                                   handle_hessian_npd => 0,
                                                   copy_data => 0,
                                                   cutoff => $self->cutoff(),
                                                   cutoff_thetas => $self->cutoff_thetas,
                                      }}
            );

        $cvobject->run();
        $self->warnings($self->warnings() + $cvobject->warnings());
        $t_optimal = $cvobject->subtool_arguments->{'xv_step'}->{'own_parameters'}->{'t_optimal'} if (defined $cvobject->subtool_arguments);
}


    $self->write_log(message=>"The final t-value is: ". $t_optimal);

    if ($t_optimal<=0){
        ui -> print( category => 'lasso',
                     message =>"Of the *tested* t_values, none gave a better model than the model".
                     " without covariates (t_value=0). No use running lasso model.\n");
        return undef;
    }
    ui -> print( category => 'lasso',
                 message  => "The optimal t-value is $t_optimal. Running lasso model with t=$t_optimal\n" );

    my $lasso_optimal = $lasso_model -> copy(filename => $self->directory()."m1/lasso_optimal_".$iteration.".mod",
                                             copy_datafile => 0,
                                             output_same_directory => 1,
                                             write_copy => 0,
                                             copy_output => 0);

    $lasso_optimal->initial_values(parameter_type => 'theta',
                                   parameter_numbers => [[$self->t_thetanumber]],
                                   new_values =>[[$t_optimal]]);
    $lasso_optimal->_write();

    # Run a modelfit on the the whole data set, with the 'best' t-value

    my $mfitobj = tool::modelfit -> new (%{common_options::restore_options(@common_options::tool_options)},
                                         models    => [$lasso_optimal],
                                         seed => $common_seed,
                                         cut_thetas_rounding_errors => 1,
                                         handle_hessian_npd => 0,
                                         cutoff => $self->cutoff(),
#        cutoff_thetas => [$self->cutoff_thetas], #array of arrayref, is this right?????
                                         cutoff_thetas => $self->cutoff_thetas, #try this
                                         base_directory => $self->directory(),
                                         directory => $self->directory().'optimal_lasso_'.$iteration.'_modelfit_dir',
                                         top_tool =>0,
                                         rplots => -1,
                                         raw_results_append => $append,
                                         raw_results_file => [$self->raw_results_file->[0]],
                                         _raw_results_callback => $self->_optimal_lasso_raw_results_callback(model_number => 1),
                                         parent_threads=>1);

    $mfitobj->run();

    if (not defined $lasso_optimal -> outputs -> [0]
        or not defined $lasso_optimal -> outputs ->[0]->get_single_value(attribute => 'ofv')){
        croak("Couldn't execute the LASSO - optimal model\n");
    }
    #print minimization status to log file
    #Create the optimal model file (not LASSO)

    $lasso_optimal -> update_inits( from_output => $lasso_optimal->outputs->[0],
                                    update_fix => 1);

    return ($lasso_optimal,$t_optimal);
}

sub write_coefficients{
    my %parm = validated_hash(\@_,
                              labels => { isa => 'ArrayRef', optional => 0 },
                              coefficients => { isa => 'ArrayRef', optional => 0 },
                              directory => {isa => 'Str',optional => 0},
                              factor => {isa => 'Num',optional => 0},
                              convergence => {isa => 'Bool',optional => 0},
                              filename => {isa => 'Str',optional => 0},
        );
    my $directory = $parm{'directory'};
    my $coefficients = $parm{'coefficients'};
    my $labels = $parm{'labels'};
    my $filename = $parm{'filename'};
    my $factor = $parm{'factor'};
    my $convergence = $parm{'convergence'};

    my $file = $directory.$filename;
    my $printheader=1;
    if (-e $file){
        $printheader=0;
    }
    open(LOG, '>>'.$file);
    print LOG join(',',@{$labels}).',FACTOR,CONVERGED'."\n" if ($printheader);
    print LOG join(',',@{$coefficients}).','.$factor.','.$convergence."\n";
    close LOG;

}

sub setup_optimal_model
{
    #NOT used anywhere
    my %parm = validated_hash(\@_,
                              finalvalues => { isa => 'ArrayRef', optional => 0 },
                              base_model => { isa => 'model', optional => 0 },
                              parameter_covariate_form => { isa => 'HashRef', optional => 0 },
                              statistics => { isa => 'HashRef', optional => 0 },
                              use_pred => {isa => 'Bool', optional => 0},
                              normalize => {isa => 'Bool', optional => 0},
                              log_scale => {isa => 'Bool', optional => 0},
                              NOABORT_added => {isa => 'Bool', optional => 0},
                              directory => {isa => 'Str', optional => 0},
                              cutoff_thetas => { isa => 'ArrayRef', optional => 0 },
                              lasso_coefficients => { isa => 'ArrayRef', optional => 0 },
        );
    my $finalvalues = $parm{'finalvalues'};
    my $base_model = $parm{'base_model'};
    my %parameter_covariate_form = %{$parm{'parameter_covariate_form'}};
    my $statistics = $parm{'statistics'};
    my $use_pred = $parm{'use_pred'};
    my $normalize = $parm{'normalize'};
    my $log_scale = $parm{'log_scale'};
    my $NOABORT_added = $parm{'NOABORT_added'};
    my $directory = $parm{'directory'};
    my $cutoff_thetas = $parm{'cutoff_thetas'};
    my $lasso_coefficients = $parm{'lasso_coefficients'};

    #FIXME remove centering. Drop sd, do not recompute coefficients to include sd

    my $model_optimal = $base_model -> copy(filename =>$directory. "m1/optimal_model.mod",
                                            copy_datafile => 0,
                                            copy_output => 0,
                                            write_copy => 0,
                                            output_same_directory => 1);


    $model_optimal ->update_inits(from_hash => $finalvalues->[0], #first prob
                                  ignore_missing_parameters => 1);

    my $nthetas = $base_model->nthetas; #initiate counter for remainder thetas

    my %coefficients;

    for (my $i=0; $i< scalar(@{$cutoff_thetas}); $i++) {
        my $th_num = $cutoff_thetas->[$i];
        $coefficients{$th_num}=$lasso_coefficients->[$i];
    }

    #have %parameter_covariate_form here
    my $index=0;

    #thetas that are below cutoff are set to 0 and marked for removal,
    #if not below cutoff then pick up sd and init and compute new initial value and
    #unset boundaries


    my @if_statements; #only once for each covariate, not for each param.
    my @zero_statements; #only once for each covariate, not for each param.
    my %if_printed;
    my @factor_code;

    my %selected_cont;

    foreach my $par (sort {lc($a) cmp lc($b)} keys %parameter_covariate_form){
        foreach my $covariate (sort {lc($a) cmp lc($b)} keys %{$parameter_covariate_form{$par}}){
            if ($parameter_covariate_form{$par}{$covariate}{'form'} == 2){
                my $thnum = $parameter_covariate_form{$par}{$covariate}{'theta'};
                my $coeff = $coefficients{$thnum};
                unless ($coeff == 0){
                    my $name = $par."$covariate";
                    if (defined $selected_cont{$par}){
                        push(@{$selected_cont{$par}},$name);
                    }else {
                        $selected_cont{$par} = [$name];
                    }
                    $nthetas++;
                    push (@factor_code, $blank .final_theta_string(parameter => $par,
                                                                   covariate => $covariate,
                                                                   normalize => $normalize,
                                                                   thetanumber=>$nthetas,
                                                                   mean => $statistics->{$covariate}{2}{'mean'}));
                    add_optimal_theta(model => $model_optimal,
                                      parameter => $par,
                                      covariate => $covariate,
                                      thetanumber => $nthetas,
                                      coefficient => $coeff,
                                      normalize => $normalize,
                                      sd => $statistics->{$covariate}{2}{'sd'});
                }
            }elsif ($parameter_covariate_form{$par}{$covariate}{'form'} == 3){
                my $thnum = $parameter_covariate_form{$par}{$covariate}{'theta'};
                croak("bug hstick thnum 1 $par $covariate") unless (defined $thnum);
                my $coeff = $coefficients{$thnum};
                unless ($coeff == 0){
                    my $name = $par."$covariate";
                    if (defined $selected_cont{$par}){
                        push(@{$selected_cont{$par}},$name);
                    }else {
                        $selected_cont{$par} = [$name];
                    }
                    $nthetas++;
                    push (@factor_code, $blank .final_theta_string(parameter => $par,
                                                                   covariate => $covariate,
                                                                   normalize => $normalize,
                                                                   thetanumber=>$nthetas,
                                                                   mean => $statistics->{$covariate}{3}{'mean'}));
                    add_optimal_theta(model => $model_optimal,
                                      parameter => $par,
                                      covariate => $covariate,
                                      thetanumber => $nthetas,
                                      normalize => $normalize,
                                      coefficient => $coeff,
                                      sd => $statistics->{$covariate}{3}{'sd'});
                }
                $thnum = $parameter_covariate_form{$par}{$covariate}{'Htheta'};
                croak("bug hstick thnum 2 $par $covariate") unless (defined $thnum);
                $coeff = $coefficients{$thnum};
                unless ($coeff == 0){
                    my $name = $par."H$covariate";

                    if (defined $selected_cont{$par}){
                        push(@{$selected_cont{$par}},$name);
                    }else {
                        $selected_cont{$par} = [$name];
                    }
                    $nthetas++;
                    unless ($if_printed{$covariate}{3}){
                        my $cut_off =  $statistics->{$covariate}{3}{'breakpoint'};
                        push @zero_statements, $blank . "H".$covariate . " = 0";
                        push @if_statements, $blank . "IF ($covariate .GT. " . sprintf($dec_str,$cut_off) .
                            ") H$covariate = $covariate" .sprintf($sign_dec_str,-$cut_off);
                        $if_printed{$covariate}{3}=1;
                    }
                    push (@factor_code, $blank .final_theta_string(parameter => $par,
                                                                   covariate => 'H'.$covariate,
                                                                   thetanumber=>$nthetas,
                                                                   normalize => $normalize,
                                                                   mean => $statistics->{$covariate}{3}{'H-mean'}));

                    add_optimal_theta(model => $model_optimal,
                                      parameter => $par,
                                      covariate => 'H'.$covariate,
                                      normalize => $normalize,
                                      thetanumber => $nthetas,
                                      coefficient => $coeff,
                                      sd => $statistics->{$covariate}{3}{'H-sd'});
                }
            }elsif ($parameter_covariate_form{$par}{$covariate}{'form'} == 1){
                foreach my $fact (sort {$a<=>$b} keys %{$statistics->{$covariate}{1}{'cat_hash'}}) {
                    my $thnum = $parameter_covariate_form{$par}{$covariate}{'thetas'}{$fact};
                    next unless (defined $thnum);
                    my $coeff = $coefficients{$thnum};
                    unless ($coeff == 0){
                        my $name = $par.$covariate.$fact;
                        $nthetas++;
                        if (defined $selected_cont{$par}){
                            push(@{$selected_cont{$par}},$name);
                        }else {
                            $selected_cont{$par} = [$name];
                        }

                        push (@factor_code,$blank.final_theta_string(parameter => $par,
                                                                     covariate => $covariate.$fact,
                                                                     thetanumber => $nthetas,
                                                                     normalize => $normalize,
                                                                     mean => $statistics->{$covariate}{1}{'mean'}{$fact}));

                        unless ($if_printed{$covariate}{1}{$fact}){
                            push @zero_statements, $blank . $covariate.$fact ." = 0";
                            push @if_statements, $blank . "IF ($covariate .EQ. $fact) $covariate$fact=1";
                            $if_printed{$covariate}{1}{$fact} = 1;
                        }

                        add_optimal_theta(model => $model_optimal,
                                          parameter => $par,
                                          covariate => $covariate.$fact,
                                          thetanumber => $nthetas,
                                          normalize => $normalize,
                                          coefficient => $coeff,
                                          sd => $statistics->{$covariate}{1}{'sd'}{$fact});
                    }
                }
            }else{
                croak("unknown form par $par covariate $covariate form ".$parameter_covariate_form{$par}{$covariate}{'form'});
            }
        } #end loop covariates
    } #end loop parameters

    my @new_code;
    push @new_code, "\n";
    push @new_code,@zero_statements;
    push @new_code, "\n";
    push @new_code,@if_statements;
    push @new_code, "\n";
    push @new_code, @factor_code;
    push @new_code, "\n";
    my $plus1='+1';
    my $sign='*';
    if ($log_scale){
        $plus1='';
        $sign = '+';
    }
    foreach my $par (sort {lc($a) cmp lc($b)} keys %selected_cont){
        my $str = $par .'COV = ('.join("$plus1)$sign(",@{$selected_cont{$par}})."$plus1)";
        push @new_code,@{parse_row(parse_str => $blank . $str,
                                   parse_operator =>$sign,
                                   max_length => $row_length)};
    }

    my @old_code;
    if ($use_pred) {
        @old_code = @{$model_optimal->get_code(record => 'pred')};
    } else {
        @old_code = @{$model_optimal->get_code(record => 'pk')};
    }
    add_tv_multiplication(code => \@old_code, parameters =>[keys %selected_cont],log_scale => $log_scale);

#    print join("\n",@new_code)."\n";
    push @new_code,@old_code;

    if ($use_pred){
        $model_optimal->set_code(record => 'pred', code => \@new_code);
    }else{
        $model_optimal->set_code(record => 'pk', code => \@new_code);
    }

    if ($NOABORT_added){
        $model_optimal->remove_option(record_name => 'estimation',
            option_name => 'NOABORT');
    }
    return $model_optimal;
}

sub _modelfit_raw_results_callback
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};
    my $subroutine;

    return \&subroutine;
}

sub modelfit_analyze
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        model_number => { isa => 'Int', optional => 1 }
    );
    my $model_number = $parm{'model_number'};

    return unless (defined $self -> model_optimal and scalar(@{$self->model_optimal})>0);

    if ($self->run_final_model and (
        (not defined $self -> model_optimal-> [-1]-> outputs -> [0] or
        not $self->model_optimal->[-1]->outputs->[0]->get_single_value(attribute =>'minimization_successful')))){
        my $round;
        if (defined $self -> model_optimal->[-1]->outputs->[0]->get_single_value (attribute => 'rounding_errors')
                and $self -> model_optimal->[-1]->outputs->[0]->get_single_value (attribute => 'rounding_errors')){
            $round = ', rounding errors';
        }

        ui -> print( category => 'lasso',  message  => "No successful minimization of optimal model$round" );
    }
    ui -> print( category => 'lasso',  message  => "\nLasso done." );
    if ($self->warnings == 1){
        ui -> print( category => 'lasso',  message  => "There was 1 warning, please check ".
            $self->logfile->[0]." for details.");

    }elsif ($self->warnings > 0){
        ui -> print( category => 'lasso',
            message  => "There were ".$self->warnings." warnings, please check ".
            $self->logfile->[0]." for details.");
    }
}

sub prepare_results
{
    my $self = shift;
}

sub cleanup
{
    my $self = shift;

    #remove tablefiles in simulation NM_runs, they are
    #copied to m1 by modelfit and read from there anyway.
    for (my $samp=1;$samp<=$self->samples(); $samp++){
        unlink $self -> directory."/simulation_dir1/NM_run".$samp."/mc-sim-".$samp.".dat";
        unlink $self -> directory."/simulation_dir1/NM_run".$samp."/mc-sim-".$samp."-1.dat"; #retry
    }
}

1;
