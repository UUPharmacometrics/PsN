package model::problem::msfi;

use Mouse;
use MouseX::Params::Validate;
use include_modules;
use OSspecific;
use File::Spec qw(abs2rel catfile);
use File::Copy 'copy';

extends 'model::problem::record';

has 'filename'  => ( isa => 'Str', reader => 'get_filename', writer => '_set_filename' );
has 'directory'  => ( isa => 'Str', reader => 'get_directory', writer => '_set_directory' );
has 'model_directory' => ( is => 'rw', isa => 'Maybe[Str]' );
#problem numbering starts at 1, problem number 0 means external msfo
has 'msfo_from_problem_number' => ( isa => 'Int', reader => 'get_msfo_from_problem_number', writer => '_set_msfo_from_problem_number', default => 0 );
has 'internal_msfo_files' => (is => 'rw', isa => 'HashRef', default => sub { {} });

our @msf_additional_files = ('_ETAS','_RMAT','_SMAT');

sub BUILD
{
    my $self  = shift;
    #first option is always msfi file
    #now drop filename option and never treat it as option again
    my $skipopt = shift(@{$self->options});
    my $raw_filename = $skipopt->name;
    if (defined $self->internal_msfo_files->{$raw_filename}){
        $self->_set_msfo_from_problem_number($self->internal_msfo_files->{$raw_filename});
        $self->set_filename(filename => $raw_filename);
    }else{
        my ($dir,$name) = OSspecific::absolute_path($self->model_directory,$raw_filename);
        $self->set_filename(filename => $dir.$name);
    }
    $self->model_directory(undef);

}
sub get_absolute_filename
{
    my $self = shift;
    if ($self->get_msfo_from_problem_number >0){
        return $self->get_filename;
    }else{
        return File::Spec->catfile($self->get_directory,$self->get_filename);
    }
}
sub set_filename
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              filename => { isa => 'Str', optional => 0 },
                              directory => { isa => 'Maybe[Str]', optional => 1 }
        );
    my $filename = $parm{'filename'};
    my $directory = $parm{'directory'};

    if ($self->get_msfo_from_problem_number >0){
        if (defined $directory){
            croak("cannot give directory name for internal msf file in msfi set_filename");
        }
        $self->_set_filename($filename);
    }else{
        #external msf file
        my ($dir,$file) = OSspecific::absolute_path($directory,$filename);
        $self->_set_filename($file);
        $self->_set_directory($dir);
    }
}

sub format_filename
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              write_directory => { isa => 'Str', optional => 0 },
                              relative_msfi_path => { isa => 'Bool', default => 1 }
        );
    my $write_directory = $parm{'write_directory'};
    my $relative_msfi_path = $parm{'relative_msfi_path'};
    my $string;

    if ($self->get_msfo_from_problem_number >0){
        $string = $self->get_filename; #always local filename without directory
    }else{
        if ($relative_msfi_path){
            my $path = File::Spec->abs2rel($self->get_directory,$write_directory);
            if ($path eq '.'){
                $string = $self->get_filename();
            }else{
                #abs2 rel does not give / or \, catfile adds it
                $string = File::Spec->catfile($path,$self->get_filename);
            }
            #        print "write_directory is $write_directory msfi_dir is ".$self->get_directory." path is $path\n";
        }else{
            $string = File::Spec->catfile($self->get_directory,$self->get_filename);
        }
    }

    #check if contains spaces, then add quotes
    if ($string =~ /\s/){ #FIXME also = , ; () need quotes
        $string = '"'.$string.'"';
    }

    if (length($string)> 71){
        warn "msfi file string too long, more than 71,\n $string";
    }

    return $string;

}
sub _format_record
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        write_directory => { isa => 'Str', optional => 0 },
        relative_msfi_path => { isa => 'Bool', default => 1 },
    );
    my $write_directory = $parm{'write_directory'};
    my $relative_msfi_path = $parm{'relative_msfi_path'};

    my @formatted;

    #overloaded for MSFI
    # msfi::format_record
    #

    #we make it easy and print all comments at the end, regardless of print order.

    my $filestring = $self->format_filename(write_directory => $write_directory,
                                            relative_msfi_path => $relative_msfi_path);

    if (length($filestring)> 71){
        warn "msfi file string too long, more than 71,\n $filestring";
    }

    my $line =0;
    $formatted[$line] = '$MSFI'.(' ' x (10 - length('MSFI')) ).$filestring;

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

sub get_additional_msfo_files
{
    my %parm = validated_hash(\@_,
                              msfname => { isa => 'Str', optional => 0 },
        );
    my $msfname = $parm{'msfname'};

    my @array=();
    my $extension='';
    if ($msfname =~ s/(\.[^.]+)$//){
        $extension = $1;
    }

    foreach my $extra (@msf_additional_files){
        push(@array,$msfname.$extra.$extension);
    }

    return \@array;
}

sub get_basename_msftype_extension
{
    my %parm = validated_hash(\@_,
                              filename => { isa => 'Str', optional => 0 },
        );
    my $filename = $parm{'filename'};

    my $base =$filename;
    my $type = '';
    my $extension='';

    if ($base =~ s/(\.[^.]+)$//){
        $extension = $1;
    }

    foreach my $extra (@msf_additional_files){
        if ($base =~ s/($extra)$//){
            $type = $extra;
            last;
        }
    }
    return ($base,$type,$extension);

}

sub copy_msfi_file
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              write_directory => { isa => 'Str', optional => 0 },
                              ignore_missing_file => {isa => 'Bool', default => 0},
                              overwrite => {isa => 'Bool', default => 0},
    );

    my $write_directory = $parm{'write_directory'};
    my $ignore_missing_file = $parm{'ignore_missing_file'};
    my $overwrite = $parm{'overwrite'};

    unless ($self->get_msfo_from_problem_number > 0){
        my $path = File::Spec->abs2rel($self->get_directory,$write_directory);
        unless ($path eq '.') {
            #write directory and current directory are not the same
            my $old = $self->get_absolute_filename();
            my $new = File::Spec->catfile($write_directory,$self->get_filename);
            if (-e $old){
                if (-e $new and (not $overwrite)){
                    croak("msfi file $new already exists when copying from $old, and not allowed to overwrite in copy_msfi_file");
                }else{
                    copy($old,$new);
                    my $old_extra = get_additional_msfo_files(msfname => $old);
                    my $new_extra = get_additional_msfo_files(msfname => $new);
                    for (my $j=0; $j<scalar(@{$old_extra}); $j++){
                        if (-e $old_extra->[$j]){
                            copy($old_extra->[$j],$new_extra->[$j]);
                        }
                    }
                }
            }else{
                croak("msfi file $old does not exist in copy_msfi_file to $new") unless $ignore_missing_file;
            }
            $self->set_filename(filename => $new);
        }
    }
}

1;
