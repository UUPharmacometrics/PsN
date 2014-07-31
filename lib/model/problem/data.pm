package model::problem::data;

use Moose;
use MooseX::Params::Validate;
use include_modules;
use OSspecific;
use File::Spec qw(abs2rel catfile);

extends 'model::problem::record';

has 'ignoresign' => ( is => 'rw', isa => 'Str' );
has 'ignore_list' => ( is => 'rw', isa => 'ArrayRef[Str]' );
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
	my @keep_opts=();
	foreach my $option ( @{$self->options} ) {
		if ( defined $option){ 
			if ($option->name eq 'IGNORE' or index('IGNORE',$option ->name ) == 0) {
				my $value = $option->value;
				chomp( $value );
				if (defined $value and length($value)>0){
					if ( $value =~ /\(*.\)/ ) {
						$value =~ s/\(//g;
						$value =~ s/\)//g;
						my @raw_list = split(',',$value);
						$self->ignore_list([]) unless defined $self->ignore_list; # In case the reference is undef
						push( @{$self->ignore_list}, @raw_list );
						push(@keep_opts,$option);
					} else {
						if (($value =~ /^'/) or ($value =~ /^"/)){
							croak("PsN does not support quoted IGNORE signs in \$DATA");
						}
						#do not push to keep_opts, store as attribute
						$self->ignoresign($value);
					}
				}else{
					print "\nWarning: empty value of IGNORE option in \$DATA\n";
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


	if ($relative_data_path){
		my $path = File::Spec->abs2rel($self->get_directory,$write_directory);
		if ($path eq '.'){
			$string = $self->get_filename();
		}else{
			#abs2 rel does not give / or \, catfile adds it
			$string = File::Spec->catfile($path,$self->get_filename);
		}
#		print "write_directory is $write_directory data_dir is ".$self->get_directory." path is $path\n";
	}else{
		$string = File::Spec->catfile($self->get_directory,$self->get_filename);
	}

	#check if contains spaces, then add quotes
	if ($string =~ /\s/){
		$string = '"'.$string.'"';
	}

	if (length($string)> 80){
		carp("datafile string too long, more than 80,\n $string");
	}

	return $string;

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
	# 

	#we make it easy and print all comments at the end, regardless of print order.

	my $filestring = $self->format_filename(write_directory => $write_directory,
											relative_data_path => $relative_data_path);

	if (length($filestring)> 80){
		#we should not end up here, should have been picked up by model
		#that should copy data if too long
		#TODO fix not implemented yet
#		croak("This is a bug: datafile string too long, more than 80,\n $filestring"); #croak useful during development 
		carp("datafile string too long, more than 80,\n $filestring");
	}

	my $line =0;
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

	my @comments = defined($self->comment) ? @{$self->comment} : ();    
	my @options = defined($self->options) ? @{$self->options} : ();

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

no Moose;
__PACKAGE__->meta->make_immutable;
1;
