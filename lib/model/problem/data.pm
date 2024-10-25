package model::problem::data;

use Mouse;
use MouseX::Params::Validate;
use include_modules;
use OSspecific;
use Config;

extends 'model::problem::record';

has 'ignoresign' => ( is => 'rw', isa => 'Str' );
has 'filename'  => ( isa => 'Str', reader => 'get_filename', writer => '_set_filename' );
has 'directory'  => ( isa => 'Str', reader => 'get_directory', writer => '_set_directory' );
has 'model_directory' => ( is => 'rw', isa => 'Maybe[Str]' );


sub BUILD
{
    my $self  = shift;
    #first option is always data file
    #now drop filename option and never treat it as option again
    my $skipopt = shift(@{$self->options});
    my ($dir,$name) = OSspecific::absolute_path($self->model_directory,$skipopt->name);
    $self->set_filename(filename => $dir.$name);
    $self->model_directory(undef);
    #remaining opts
    #if ignoresign is set, i.e. IGNORE or ACCEPT option
    #then remove it from set of options and store in ignoresign attribute

    #closing parenthesis is allowed as ignoresign, but not opening
    my @keep_opts=();
    my $optcount = scalar(@{$self->options});

    for (my $j=0; $j<$optcount; $j++ ) {
        my $option = $self->options->[$j];
        if ( defined $option){
            if ($option->name eq 'IGNORE' or index('IGNORE',$option ->name ) == 0) {
                my $value = $option->value;
                chomp( $value );
                if (defined $value and length($value)>0){
                    if ( $value =~ /^\(/ ) {
                        #opening parenthesis. This cannot be single ignoresign, NONMEM does not allow it,
                        #so assume beginning of list
                        push(@keep_opts,$option);
                    } else {
                        if (($value =~ /^'.'/) or ($value =~ /^"."/)){
                            croak("PsN does not support quoted IGNORE signs in \$DATA: IGNORE=".$value);
                        }
                        if (length($value)==1){
                            if ($value eq "'") {
                                croak("NONMEM does not support IGNORE=' in \$DATA");
                            }
                            #do not push to keep_opts, store as attribute
                            $self->ignoresign($value);
                        }else{
                            croak("Illegal IGNORE in \$DATA, must be single character: IGNORE=".$value);
                        }
                    }
                }else{
                    if (defined $self->ignoresign or
                        (($j+1)<$optcount and defined $self->options->[$j+1] and $self->options->[$j+1]->name =~ /^\(/ )){
                        #this is beginning of IGNORE (list)
                        push(@keep_opts,$option);
                    }else{
                        croak("No value of IGNORE in \$DATA, did you skip the equal sign in IGNORE= ?");
                    }
                }
            }else{
                push(@keep_opts,$option);
            }
        }
    }

    $self->options(\@keep_opts);
}

sub get_absolute_filename
{
    my $self = shift;
    return File::Spec->catfile($self->get_directory,$self->get_filename);
}

sub set_filename
{
    #setting filename
    my $self = shift;
    my %parm = validated_hash(\@_,
        filename => { isa => 'Str', optional => 0 },
        directory => { isa => 'Maybe[Str]', optional => 1 }
    );
    my $filename = $parm{'filename'};
    my $directory = $parm{'directory'};

    my ($dir,$file) = OSspecific::absolute_path($directory,$filename);
    $self->_set_filename($file);
    $self->_set_directory($dir);
}

sub format_filename
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        write_directory => { isa => 'Str', optional => 0 },
        relative_data_path => { isa => 'Bool', optional => 0 }
    );
    my $write_directory = $parm{'write_directory'};
    my $relative_data_path = $parm{'relative_data_path'};
    my $string;

    if ($relative_data_path) {
        my $cleaned_path;
        if ($Config{'osname'} ne 'MSWin32') {
            $cleaned_path = Cwd::abs_path($self->get_directory);     # Remove symlinks that makes abs2rel generate very long paths
        }
        if (not defined $cleaned_path) {
            $cleaned_path = $self->get_directory;                   # Fallback
        }

        my $path = File::Spec->abs2rel($cleaned_path, $write_directory);
        if ($path eq '.') {
            $string = $self->get_filename();
        } else {
            #abs2 rel does not give / or \, catfile adds it
            $string = File::Spec->catfile($path, $self->get_filename);
        }
    } else {
        $string = File::Spec->catfile($self->get_directory, $self->get_filename);
    }

    #check if contains spaces, then add quotes
    if ($string =~ /\s/) {
        $string = '"' . $string . '"';
    }

    return $string;
}

sub _is_ignore_accept
{
    # Return 1 for IGNORE, 2 for ACCEPT and 0 for neither
    my $name = shift;
    my $value = shift;
    if ($name =~ /^(IGNORE|IGNOR|IGNO|IGN|ACCEPT|ACCEP|ACCE|ACC)(\()?/) {
        my $ret;
        my $paren = $2;
        if ($name =~ /^I/) {
            $ret = 1;
        } else {
            $ret = 2;
        }
        if (defined $paren) {
            return $ret;
        } else {
            if ($value =~ /^\(/) {
                return $ret;
            }
        }
    }
    return 0;
}

sub have_ignore_accept
{
    # Do we have any ignore or accept?
    my $self = shift;

    for my $option (@{$self->options}) {
        my $ret = _is_ignore_accept($option->name, $option->value);
        if ($ret) {
            return $ret;
        }
    }
    return 0;
}

sub remove_ignore_accept
{
    # Remove all IGNORE=() and ACCEPT=() statements
    my $self = shift;

    my @options;
    my $in_parens;
    for my $option (@{$self->options}) {
        if ($in_parens) {
            if ($option->name =~ /\)$/) {
                $in_parens = 0;
            }
        } elsif ($option->name !~ /^(IGNORE|IGNOR|IGNO|IGN|ACCEPT|ACCEP|ACCE|ACC)/) {
            push @options, $option;
        } else {
            if (defined $option->value and $option->value =~ /^\(/) {
                $in_parens = 1;
            }
        }
    }
    $self->options(\@options);
}

sub _format_record
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        write_directory => { isa => 'Str', optional => 0 },
        relative_data_path => { isa => 'Bool', optional => 0 }
    );
    my $write_directory = $parm{'write_directory'};
    my $relative_data_path = $parm{'relative_data_path'};
    my @formatted;

    #overloaded for DATA
    # data::format_record

    #we make it easy and print all comments at the end, regardless of print order.

    my $filestring = $self->format_filename(write_directory => $write_directory,
                                            relative_data_path => $relative_data_path);

    my $line = 0;
    $formatted[$line] = '$DATA'.(' ' x (10 - length('DATA')) ).$filestring;

    #add ignoresign if defined, stored separately from options
    if (defined $self->ignoresign and length($self->ignoresign)> 0){
        # Check and add linebreak if necessary.
        my $foption = 'IGNORE='.$self->ignoresign;
        if (length( $formatted[$line].' '.$foption ) > 70 ){
            $formatted[$line] .= "\n";
            $line++;
            # Indent for next option
            push( @formatted, ' ' x 11 );
        }
        $formatted[$line] .= ' '.$foption;
    }

    my @comments = @{$self->comment};
    my @options = @{$self->options};

    for( my $i = 0; $i < scalar @options; $i++ ){
        my $foption = $options[$i] -> _format_option;

        # Check and add linebreak if necessary.
        if (length( $formatted[$line].' '.$foption ) > 70 ){
            $formatted[$line] .= "\n";
            $line++;
            # Indent for next option
            push( @formatted, ' ' x 11 );
        }
        $formatted[$line] .= ' '.$foption;
    }
    $formatted[$line] .= "\n";

    #add all comments last
    push(@formatted,@comments);

    return \@formatted;
}

1;
