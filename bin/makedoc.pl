#!/usr/bin/perl

use strict;
use XML::LibXML;
use XML::XPath;
use XML::XPath::XMLParser;

my $xp = XML::XPath->new(filename => $ARGV[0]);
my $format = $ARGV[1];
my $only_class = $ARGV[2];


#my $suffix = defined $format ? '.'.$format : '.txt';
my $suffix = '.pod';

# Get the dia objects in this file
my $objectset     = $xp->find('/dia:diagram/dia:layer/dia:object[@type=\'UML - Class\' or '.
			      '@type=\'UML - LargePackage\']');
my $classset      = $xp->find('/dia:diagram/dia:layer/dia:object[@type=\'UML - Class\']');
my $objecttypeset = $xp->find('/dia:diagram/dia:layer/dia:object[@type=\'UML - Class\' or '.
			      '@type=\'UML - LargePackage\']/@type');
my $positionset   = $xp->find('/dia:diagram/dia:layer/dia:object[@type=\'UML - Class\' or '.
			      '@type=\'UML - LargePackage\']/dia:attribute[@name=\'obj_pos\']/dia:point/@val');
my $rectangleset  = $xp->find('/dia:diagram/dia:layer/dia:object[@type=\'UML - Class\' or '.
			      '@type=\'UML - LargePackage\']/dia:attribute[@name=\'obj_bb\']/dia:rectangle/@val');
my $nameset       = $xp->find('/dia:diagram/dia:layer/dia:object[@type=\'UML - Class\' or '.
			      '@type=\'UML - LargePackage\']/dia:attribute[@name=\'name\']/dia:string/text()');
my ($path_ref, $classpath_ref, $packagepath_ref) = &set_paths( $objecttypeset, $nameset, $positionset, $rectangleset );
my @paths = @{$path_ref};
my @classpaths = @{$classpath_ref};
my @packagepaths = @{$packagepath_ref};


system( 'export MANPATH=/users/lasse/PsN/Diagrams/doc' );


my $i = 0;
my $doc_root_path = `pwd`.'/doc';
foreach my $classnode ($classset->get_nodelist) {
    my $nameset = $classnode->find('dia:attribute[@name=\'name\']/dia:string/text()');
    my @nodes = $nameset->get_nodelist;
    my $raw_name  = $nodes[0] -> getValue;
    my $name = $raw_name;
    $name =~ s/#//g;

    my $attributeset = $classnode->find('dia:attribute[@name=\'name\']/dia:string[text()=\''.
					$raw_name.'\']/parent::*/parent::*/dia:attribute[@name='.
					'\'attributes\']');
#					'\'attributes\']/dia:composite[@type=\'umlattribute\']');
    my $methodset = $classnode->find('dia:attribute[@name=\'name\']/dia:string[text()=\''.
				     $raw_name.'\']/parent::*/parent::*/dia:attribute[@name='.
				     '\'operations\']/dia:composite[@type=\'umloperation\']');
    my $method_name_set = $classnode->find('dia:attribute[@name=\'name\']/dia:string[text()=\''.
					   $raw_name.'\']/parent::*/parent::*/dia:attribute[@name='.
					   '\'operations\']/dia:composite[@type=\'umloperation\']'.
					   '/dia:attribute[@name=\'name\']/dia:string/text()');

    my %module_doc      = &get_documentation( 'lib/'.$classpaths[$i].'/'.$name.'_subs.pm',
					     $doc_root_path );
    my ( $m_n_ref, $m_a_ref ) = &get_method_arguments( $methodset, $method_name_set );
    my @method_names = @{$m_n_ref};
    my @method_attributes = @{$m_a_ref};
    my $c_a_ref = &get_class_attributes( $attributeset );
    my @class_attributes = @{$c_a_ref};

#     foreach my $name ( @method_names ) {
# 	print "MNAME: $name \n";
#     }
#     foreach my $m ( @class_attributes ) {
# 	while ( my ($key, $value) = each %{$m}) {
# 	    print "$key = ",join(' ',@{$value}),"\n";
# 	}

#	foreach my $row ( @{$m} ) {
#	    print "Aname, type, value: ", join("\t",@{$row}), "\n";
#	}
#    }
#    die;
    if ( defined $classpaths[$i] and 
	 $classpaths[$i] ne '' and
	 not -e 'doc/'.$classpaths[$i] ) {
	my $acc_path;
	foreach my $part ( split('/', $classpaths[$i]) ) {
	    print "PART: $part \n";
	    unless ( -e 'doc/'.$acc_path.$part ) {
		mkdir 'doc/'.$acc_path.$part or die "Could not create directory doc/".$acc_path.$part,"\n"; ;
	    }
	    $acc_path = $acc_path.$part.'/';
	}
    }

    open( DOC, ">doc/".$classpaths[$i].'/'.$name.$suffix );

    print DOC "\n=begin pod\n\nUpdated ".`date`."\n=end pod\n\n";
    print DOC &format_name($name, $classpaths[$i], $module_doc{'include'}, $format);
    print DOC &format_description($module_doc{'description'}, $format) if defined $module_doc{'description'};
#    print DOC &format_synopsis( $name, \@method_names, \@method_attributes, $format);
    print DOC &format_synopsis($module_doc{'synopsis'}, $format) if defined $module_doc{'synopsis'};
    print DOC &format_accessors( $name, \@class_attributes, \@method_names, $format);
    print DOC &format_methods( $name, \@method_names, \@method_attributes, \%module_doc, \@class_attributes, $format);
    print DOC &format_examples($module_doc{'examples'}, $format) if defined $module_doc{'examples'};
    print DOC &format_see_also($module_doc{'see_also'}, $format) if defined $module_doc{'see_also'};
    print DOC &format_author;

    close( DOC );
    if ( $format eq 'html' ) {
	system( "pod2html --title ".$name." doc/".$classpaths[$i].'/'.$name.$suffix.' >doc/'.$classpaths[$i].'/'.$name.'.html' );
	system( "rm doc/".$classpaths[$i].'/'.$name.$suffix );
    }    
    if ( $format eq 'man' ) {
	system( "pod2man doc/".$classpaths[$i].'/'.$name.$suffix.' >doc/'.$classpaths[$i].'/'.$name.'.man' );
    }
    $i++;
}

sub format_accessors {
    my ( $class_name, $c_a_ref, $m_n_ref, $format ) = @_;
    my @class_attr = @{$c_a_ref};
    my @method_names = @{$m_n_ref};
    my $form_str;
    my @extra_names = ();
    
    if ( $#class_attr >= 0 and defined $class_attr[0]->{'name'} ) {
	for ( my $i = 0; $i < scalar @class_attr; $i++ ){
	    if ( defined $format and ( $format eq 'pod' or $format eq 'html' ) ) {
		$form_str = "=head1 ACCESSORS\n\n";
	    } else {
		$form_str = "ACCESSORS\n\n";
	    }
	    foreach my $method_name ( @method_names ) {
		push( @extra_names, $method_name ) if ( $method_name eq $class_attr[$i]->{'name'} );
		print "Found method $method_name that matches accessor name\n" if ( $method_name eq $class_attr[$i]->{'name'} );
	    }
	    $form_str = $form_str.' '.$class_attr[$i]->{'name'}."\n";
	}
#     if ( defined $class_attr{'name'} ) {
# 	if ( defined $format and ( $format eq 'pod' or $format eq 'html' ) ) {
# 	    $form_str = "=head1 ACCESSORS\n\n";
# 	} else {
# 	    $form_str = "ACCESSORS\n\n";
# 	}
# 	for ( my $i = 0; $i < scalar @{$class_attr{'name'}}; $i++ ){
# 	    foreach my $method_name ( @method_names ) {
# 		push( @extra_names, $method_name ) if ( $method_name eq $class_attr{'name'}[$i] );
# 		print "Found method $method_name that matches accessor name\n" if ( $method_name eq $class_attr{'name'}[$i] );
# 	    }
# 	    $form_str = $form_str.' '.$class_attr{'name'}[$i]."\n";
# 	}
	$form_str = $form_str."\n";
	$form_str = $form_str."All class attributes should only be accessed through the attribute ".
	    "accessors. The default behaviour of the accessors is to return the current value of ".
	    "the attribute or to set it to a new value if an argument is given.\n\n".
	    " \$value = $class_name -> accessor\n\n".
	    " $class_name -> accessor( \$new_value )\n\n";
	if ( $#extra_names >= 0 ) {
	    $form_str = $form_str."Accessors that do not behave like this are listed below and a ".
		"description of these can be found in the L</METHODS> section\n\n";
	    foreach my $extra_name ( @extra_names ) {
		$form_str = $form_str."L</".$extra_name.">\n";
	    }
	    $form_str = $form_str."\n";
	}
    }
    return $form_str;
}

sub format_author {
    return '=head1 SUPPORT/AUTHOR'."\n\n"."Copyright 2004 Lars Lindbom and Niclas Jonsson.\n".
	'This module is free software and comes AS IS with NO '.
	'WARRANTY. You may distribute the software according to the terms'.
	' of the Gnu GPL.'."\n\n".'For support see the documentation and bug'.
	' reports at L<http://psn.sourceforge.net>'."\n\n".
	'Lars Lindbom, lars.lindbom@farmbio.uu.se'."\n\n";
}

sub format_methods {
    # Big sub routine, I know
    my ( $class_name, $m_n_ref, $m_a_ref, $m_d_ref, $c_a_ref, $format ) = @_;
    my @method_names = @{$m_n_ref};
    my @method_attributes = @{$m_a_ref};
    my %module_doc = %{$m_d_ref};
    my @class_attr = @{$c_a_ref};
    my $form_str;

    if ( defined $format and ( $format eq 'pod' or $format eq 'html' ) ) {
	$form_str = "=head1 METHODS\n\n";
    } else {
	$form_str = "METHODS\n\n";
    }

    # Constructor
    $form_str = $form_str."=head2 new\n\n  new(";
    if ( $#class_attr >= 0 and defined $class_attr[0]{'name'} ) {
	$form_str = $form_str.' ';
	for ( my $i = 0; $i <= $#class_attr; $i++ ){
	    if ( defined $class_attr[$i]{'value'} and
		 $class_attr[$i]{'value'} ne '' ) {
		$form_str = $form_str.'[';
	    }
	    $form_str = $form_str.$class_attr[$i]{'name'}.' => ';
	    my @type = split(' ', $class_attr[$i]{'type'});
	    if ( defined $type[0] ) {
		if ( $type[0] eq 'array' ) {
		    $form_str = $form_str.'\@'.$type[2];
		} elsif ( $type[0] eq 'hash' ) {
		    $form_str = $form_str.'\%'.$type[2];
		} else {
		    $form_str = $form_str.'$'.$type[1];
		}
	    } else {
		$form_str = $form_str.'undefined type';
	    }
	    if ( defined $class_attr[$i]{'value'} and
		 $class_attr[$i]{'value'} ne '' ) {
		$form_str = $form_str.']';
	    }
	    $form_str = $form_str.",\n       " unless ( $i == $#class_attr );
	} 
	$form_str = $form_str.' ';
    }
    $form_str = $form_str.')';
    $form_str = $form_str."\n\n";
    # Constructor attribute default values:
    if ( $#class_attr >= 0 ) {
	my $tmp_str;
	for ( my $k = 0; $k <= $#class_attr; $k++ ) {
	    if ( defined $class_attr[$k]{'value'} and
		 $class_attr[$k]{'value'} ne '' ) {
		$tmp_str = $tmp_str.'    '.$class_attr[$k]{'name'}.
		    ' ' x (40-length($class_attr[$k]{'name'})).$class_attr[$k]{'value'}."\n";
	    }
	}
	if ( length($tmp_str) > 0 ) {
	    $form_str = $form_str."=head3 Default values\n\n";
	    $form_str = $form_str."$tmp_str\n";
	}
    }
    # Add text for constructor:
    if ( defined $module_doc{'new'} ) {
	foreach my $line ( @{$module_doc{'new'}} ) {
	    $line =~ s/^\ //;
	    $form_str = $form_str.$line;
	}
	$form_str = $form_str."\n\n";
    }


    for ( my $i = 0; $i <= $#method_names; $i++ ) {
	# Do not print any documenation on privat methods:
	next if ( $method_names[$i] =~ /^_/ );
	my @out_args = ();
	my @out_types = ();
	my @out_values = ();
	my @in_args = ();
	my @in_types = ();
	my @in_values = ();
	for ( my $j = 0; $j < scalar @{$method_attributes[$i]}; $j++ ) {
	    if ( $method_attributes[$i][$j]{'kind'} == 2 ) {
		push( @out_args, $method_attributes[$i][$j]{'name'} );
		push( @out_types, $method_attributes[$i][$j]{'type'} );
		push( @out_values, $method_attributes[$i][$j]{'value'} );
	    } else {
		push( @in_args, $method_attributes[$i][$j]{'name'} );
		push( @in_types, $method_attributes[$i][$j]{'type'} );
		push( @in_values, $method_attributes[$i][$j]{'value'} );
	    }
	}
# 	for ( my $j = 0; $j < scalar @{$method_attributes[$i]{'kind'}}; $j++ ) {
# 	    if ( $method_attributes[$i]{'kind'}[$j] == 2 ) {
# 		push( @out_args, $method_attributes[$i]{'name'}[$j] );
# 		push( @out_types, $method_attributes[$i]{'type'}[$j] );
# 		push( @out_values, $method_attributes[$i]{'value'}[$j] );
# 	    } else {
# 		if( $method_names[$i] eq 'case_deletion' ) {
# 		    print "Arg name:  ",$method_attributes[$i]{'name'}[$j],"\n";
# 		    print "Arg value: ",$method_attributes[$i]{'value'}[$j],"\n";
# 		    print "Arg type:  ",$method_attributes[$i]{'type'}[$j],"\n";
# 		}
# 		push( @in_args, $method_attributes[$i]{'name'}[$j] );
# 		push( @in_types, $method_attributes[$i]{'type'}[$j] );
# 		push( @in_values, $method_attributes[$i]{'value'}[$j] );
# 	    }
# 	}
	if ( defined $format and ( $format eq 'pod' or $format eq 'html' ) ) {
	    $form_str = $form_str."=head2 ";
	} else {
	    $form_str = $form_str."  "
	    }
	$form_str = $form_str.$method_names[$i]."\n\n";
	if ( $#out_args >= 0 ) {
	    if ( $#out_args >= 1 ) {
		$form_str = $form_str.'  ( ';
		for ( my $j = 0; $j <= $#out_args; $j++ ) {
		    my @type = split(' ', $out_types[$j]);
		    if ( $type[0] eq 'array' or $type[0] eq 'hash' ) {
			$form_str = $form_str.'$'.$out_args[$j].'_ref';
		    } else {
			$form_str = $form_str.'$'.$out_args[$j];
		    }
		    $form_str = $form_str.', ' unless ( $j == $#out_args );
		}
		$form_str = $form_str.' ) =';
#		$form_str = $form_str.'  ( $'.join( ', $', @out_args )." ) =";
	    } else {
		my @type = split(' ', $out_types[0]);
		if ( $type[0] eq 'array' or $type[0] eq 'hash' ) {
		    $form_str = $form_str.'  $'.$out_args[0].'_ref =';
		} else {
		    $form_str = $form_str.'  $'.$out_args[0].' =';
		}
#		$form_str = $form_str.'  $'.$out_args[0]." =";
	    }
	}
	$form_str = $form_str."\n      \$$class_name -> ".$method_names[$i];
	if ( $#in_args >= 0 ) {
	    $form_str = $form_str.'( ';
	    for ( my $k = 0; $k <= $#in_args; $k++ ) {
		if ( defined $in_values[$k] and
		     $in_values[$k] ne '' ) {
		    $form_str = $form_str.'[';
		}
		$form_str = $form_str.$in_args[$k].' => ';
		my @type = split(' ', $in_types[$k]);
		if ( $type[0] eq 'array' ) {
		    $form_str = $form_str.'\@'.$type[2];
		} elsif ( $type[0] eq 'hash' ) {
		    $form_str = $form_str.'\%'.$type[2];
		} else {
		    $form_str = $form_str.'$'.$type[1];
		}
		if ( defined $in_values[$k] and
		     $in_values[$k] ne '' ) {
		    $form_str = $form_str.']';
		}
		$form_str = $form_str.",\n         ".' ' x length($class_name.' -> '.$method_names[$i]) unless ( $k == $#in_args );
	    }
	    $form_str = $form_str.' )';
	} else {
	    $form_str = $form_str.'()';
	}
	$form_str = $form_str."\n\n";
	if ( $#out_args >= 0 ) {
	    my $deref = 0;
	    for ( my $j = 0; $j <= $#out_args; $j++ ) {
		my @type = split(' ', $out_types[$j]);
		if ( $type[0] eq 'array' ) {
		    $form_str = $form_str.'  @'.$out_args[$j].' = @{$'.$out_args[$j]."_ref}\n";
		    $deref++;
		} elsif ( $type[0] eq 'hash' ) {
		    $form_str = $form_str.'  %'.$out_args[$j].' = %{$'.$out_args[$j]."_ref}\n";
		    $deref++;
		}
	    }
	    $form_str = $form_str."\n\n" if $deref;
	}
	# Add the default values
	if ( $#in_args >= 0 ) {
	    my $tmp_str;
	    for ( my $k = 0; $k <= $#in_args; $k++ ) {
		if ( defined $in_values[$k] and
		     $in_values[$k] ne '' ) {
		    $tmp_str = $tmp_str.'    '.$in_args[$k].
			' ' x (40-length($in_args[$k])).$in_values[$k]."\n";
		}
	    }
	    if ( length($tmp_str) > 0 ) {
		$form_str = $form_str."=head3 Default values\n\n";
		$form_str = $form_str."$tmp_str\n";
	    }
	}

	# Add the text from the code
	if ( defined $module_doc{$method_names[$i]} ) {
	    foreach my $line ( @{$module_doc{$method_names[$i]}} ) {
		$line =~ s/^\ //;
		$form_str = $form_str.$line;
	    }
	    $form_str = $form_str."\n\n";
	}
    }
    return $form_str;
}

sub format_see_also {
    my $text = shift;
    my $format = shift;
    my $form_str;

    if ( defined $format and ( $format eq 'pod' or $format eq 'html' ) ) {
	$form_str = "=head1 SEE ALSO\n\n";
	if ( defined $text ) {
	    foreach my $line ( @{$text} ) {
		$line =~ s/^ //;
		$form_str = $form_str.$line;
	    }
	    $form_str = $form_str."\n\n";
	}
    } else {
	$form_str = "SEE ALSO\n\n";
	if ( defined $text ) {
	    foreach my $line ( @{$text} ) {
		$line =~ s/^ //;
		$form_str = $form_str.$line;
	    }
	    $form_str = $form_str."\n\n";
	}
    }
    return $form_str;
}    

sub format_synopsis {
    my $text = shift;
    my $format = shift;
    my $form_str;

    if ( defined $format and ( $format eq 'pod' or $format eq 'html' ) ) {
	$form_str = "=head1 SYNOPSIS\n\n";
	if ( defined $text ) {
	    foreach my $line ( @{$text} ) {
		$line =~ s/^ //;
		$form_str = $form_str.$line;
	    }
	    $form_str = $form_str."\n\n";
	}
    } else {
	$form_str = "SYNOPSIS\n\n";
	if ( defined $text ) {
	    foreach my $line ( @{$text} ) {
		$line =~ s/^ //;
		$form_str = $form_str.$line;
	    }
	    $form_str = $form_str."\n\n";
	}
    }
    return $form_str;
}    

sub format_examples {
    my $text = shift;
    my $format = shift;
    my $form_str;

    if ( defined $format and ( $format eq 'pod' or $format eq 'html' ) ) {
	$form_str = "=head1 EXAMPLES\n\n";
	if ( defined $text ) {
	    foreach my $line ( @{$text} ) {
		$line =~ s/^ //;
		$form_str = $form_str.$line;
	    }
	    $form_str = $form_str."\n\n";
	}
    } else {
	$form_str = "EXAMPLES\n\n";
	if ( defined $text ) {
	    foreach my $line ( @{$text} ) {
		$line =~ s/^ //;
		$form_str = $form_str.$line;
	    }
	    $form_str = $form_str."\n\n";
	}
    }
    return $form_str;
}    

sub format_synopsis_old {
    my ( $class_name, $m_n_ref, $m_a_ref, $format ) = @_;
    my @method_names = @{$m_n_ref};
    my @method_attributes = @{$m_a_ref};
    my $form_str;

    if ( defined $format and ( $format eq 'pod' or $format eq 'html' ) ) {
	$form_str = "=head1 SYNOPSIS\n\n";
    } else {
	$form_str = "SYNOPSIS\n\n";
    }

    for ( my $i = 0; $i <= $#method_names; $i++ ) {
	if ( defined $format and ( $format eq 'pod' or $format eq 'html' ) ) {
	    $form_str = $form_str.'  ';
	} else {
	    $form_str = $form_str.'        ';
	}
	my @out_args = ();
	my @out_types = ();
	my @out_values = ();
	my @in_args = ();
	my @in_types = ();
	my @in_values = ();
	for ( my $j = 0; $j < scalar @{$method_attributes[$i]{'kind'}}; $j++ ) {
	    if ( $method_attributes[$i]{'kind'}[$j] == 2 ) {
		push( @out_args, $method_attributes[$i]{'name'}[$j] );
		push( @out_types, $method_attributes[$i]{'type'}[$j] );
		push( @out_values, $method_attributes[$i]{'value'}[$j] );
	    } else {
		push( @in_args, $method_attributes[$i]{'name'}[$j] );
		push( @in_types, $method_attributes[$i]{'type'}[$j] );
		push( @in_values, $method_attributes[$i]{'value'}[$j] );
	    }
	}
	if ( $#out_args >= 0 ) {
	    if ( $#out_args >= 1 ) {
		$form_str = $form_str.'( $'.join( ', $', @out_args )." ) = ";
	    } else {
		$form_str = $form_str.'$'.$out_args[0]." = ";
	    }
	}
	$form_str = $form_str."\n      \$$class_name -> ".$method_names[$i];
	if ( $#in_args >= 0 ) {
	    $form_str = $form_str.'( ';
	    for ( my $k = 0; $k <= $#in_args; $k++ ) {
		if ( defined $in_values[$k] and
		     $in_values[$k] ne '' ) {
		    $form_str = $form_str.'[';
		}
		$form_str = $form_str.$in_args[$k].' => ';
		my @type = split(' ', $in_types[$k]);
		if ( $type[0] eq 'array' ) {
		    $form_str = $form_str.'\@'.$type[2];
		} elsif ( $type[0] eq 'hash' ) {
		    $form_str = $form_str.'\%'.$type[2];
		} else {
		    $form_str = $form_str.'$'.$type[1];
		}
		if ( defined $in_values[$k] and
		     $in_values[$k] ne '' ) {
		    $form_str = $form_str.']';
		}
		my $pad = length($method_names[$i])+length($class_name);
		$form_str = $form_str.",\n             ".' ' x $pad unless ( $k == $#in_args );
	    }
	    $form_str = $form_str.' )';
	} else {
	    $form_str = $form_str.'()';
	}
	$form_str = $form_str."\n\n";
    }
    $form_str = $form_str."\n";
    return $form_str;
}

# Måste justeras så att undef attribut registreras, dvs som get_method_parameters
sub get_class_attributes {
    my $attribute_set = shift;
    my @arguments = ( 'name', 'type', 'value' );
    my @attr = ();
#    my %part;
    foreach my $attribute_node ($attribute_set->get_nodelist) {
	# Should be only one node in attribute_set
	my $part_set = $attribute_node -> find('dia:composite');
	foreach my $node ($part_set->get_nodelist) {
	    my %parts = ();
	    foreach my $arg ( @arguments ) {
		my $attribute = $node -> find('dia:attribute[@name=\''.$arg.'\']'.
					      '/dia:string/text()');
		my @attribute_node = $attribute->get_nodelist;
		# Should only be one element in @attribute_node
		if ( $#attribute_node < 0 ) {
		    $parts{$arg} = undef;
#		    print "UNDEF $arg\n";
		} elsif ( $#attribute_node == 0 ) {
		    my $name = $attribute_node[0] -> getValue;
		    $name =~s/#//g;
		    $parts{$arg} = $name;
#		    print "$arg = $name\n";
		} elsif ( $#attribute_node > 0 ) {
		    die "Problem reading $arg nodes, too many matching\n";
		}
	    }
# 	    my $part_set = $attribute_node -> find('dia:composite[@type=\'umlattribute\']'.
# 						   '/dia:attribute[@name=\''.$arg.'\']'.
# 						   '/dia:string/text()');
#	    $part{$arg} = [];
#	    foreach my $node ($part_set->get_nodelist) {
#		my $name = $node -> getValue;
#		$name =~s/#//g;
#		push( @{$part{$arg}}, $name );
#		print "TYPE: $arg:\t$name\n";
#	    }
	    push( @attr, \%parts );
	}
    }
    return ( \@attr );
}    

sub get_method_arguments {
    my $method_set = shift;
    my $method_name_set = shift;
    my @method_names = ();
    my @method_attributes = ();

    my $idx = 0;
    my $cd = 0;
    foreach my $node ($method_name_set->get_nodelist) {
	my $name = $node -> getValue;
	$name =~s/#//g;
	push( @method_names, $name );
	$cd = $idx if $name eq 'case_deletion';
	$idx++;
    }
    
    $idx = 0;
    foreach my $method_node ($method_set->get_nodelist) {
	my (@attr_names, @attr_types, @attr_values) = ((),(),());
	my @arguments = ( 'name', 'type', 'value' );
	my @attr = ();
	my $attr_set = $method_node -> find('dia:attribute[@name=\'parameters\']/dia:composite');
	foreach my $node ($attr_set->get_nodelist) {
	    my %parts = ();
	    foreach my $arg ( @arguments ) {
		my $attribute = $node -> find('dia:attribute[@name=\''.$arg.'\']'.
						  '/dia:string/text()');
		my @attribute_node = $attribute->get_nodelist;
		# Should only be one element in @attribute_node
		if ( $#attribute_node < 0 ) {
		    $parts{$arg} = undef;
		} elsif ( $#attribute_node == 0 ) {
		    my $name = $attribute_node[0] -> getValue;
		    $name =~s/#//g;
		    $parts{$arg} = $name;
		} elsif ( $#attribute_node > 0 ) {
		    die "Problem reading $arg nodes, too many matching\n";
		}
	    }
	    # Different handling of 'kind':
	    my $attribute = $node -> find( 'dia:attribute[@name=\'kind\']'.
					       '/dia:enum/@val');
	    my @attribute_node = $attribute->get_nodelist;
	    # Should only be one element in @attribute_node
	    if ( $#attribute_node < 0 ) {
		$parts{'kind'} = undef;
	    } elsif ( $#attribute_node == 0 ) {
		$parts{'kind'} = $attribute_node[0] -> getValue;
	    } elsif ( $#attribute_node > 0 ) {
		die "Problem reading kind nodes, too many matching\n";
	    }
	    push( @attr, \%parts );
	}
# 	foreach my $arg ( @arguments ) {
# 	    my $attr_set = $method_node -> find('dia:attribute[@name=\'parameters\']'.
# 						'/dia:composite/dia:attribute[@name=\''.$arg.'\']'.
# 						'/dia:string/text()');
# 	    $attr{$arg} = [];
# 	    foreach my $node ($attr_set->get_nodelist) {
# 		my $name = $node -> getValue;
# 		$name =~s/#//g;
# 		push( @{$attr{$arg}}, $name );
# 		print "$arg\t$name\n" if $cd == $idx;
# 	    }
# 	}
# 	# Different handling of 'kind':
# 	my $attr_set = $method_node -> find('dia:attribute[@name=\'parameters\']'.
# 					    '/dia:composite/dia:attribute[@name=\'kind\']'.
# 					    '/dia:enum/@val');
# 	$attr{'kind'} = [];
# 	foreach my $node ($attr_set->get_nodelist) {
# 	    my $name = $node -> getValue;
# 	    push( @{$attr{'kind'}}, $name );
# 	}
	push( @method_attributes, \@attr );
	$idx++;
    }
    return ( \@method_names, \@method_attributes );
}

sub format_description {
    my $text = shift;
    my $format = shift;
    my $form_str;

    if ( defined $format and ( $format eq 'pod' or $format eq 'html' ) ) {
	$form_str = "=head1 DESCRIPTION\n\n";
	if ( defined $text ) {
	    foreach my $line ( @{$text} ) {
		$line =~ s/^ //;
		$form_str = $form_str.$line;
	    }
	    $form_str = $form_str."\n\n";
	}
    } else {
	$form_str = "DESCRIPTION\n\n";
	if ( defined $text ) {
	    foreach my $line ( @{$text} ) {
		$line =~ s/^ //;
		$form_str = $form_str.$line;
	    }
	    $form_str = $form_str."\n\n";
	}
    }
    return $form_str;
}

sub format_name {
    my $name = shift;
    my $path  = shift;
    my $text = shift;
    my $format = shift;
    my $form_str;

    if ( defined $path ) {
	$path =~ s/\//::/g;
	$path = $path.'::';
    }

    if ( defined $format and ( $format eq 'pod' or $format eq 'html' ) ) {
	$form_str = "=head1 NAME\n\n".$path.$name;
	if ( defined $text ) {
	    $form_str = $form_str.' - ';
	    foreach my $line ( @{$text} ) {
		$line =~ s/^\s*//;
		$form_str = $form_str.$line;
	    }
	}
    } else {
	$form_str = uc($path.$name)."\n";
	if ( defined $text ) {
	    foreach my $line ( @{$text} ) {
		$line =~ s/^\s*//;
		$form_str = $form_str.$line;
	    }
	}
    }
    $form_str = $form_str."\n\n";
    return $form_str;
}

sub get_documentation {
    my $filename = shift;
    my $doc_root_path = shift;

    my %doc;
    open( FILE, $filename );
    my @file = <FILE>;
    close( FILE );
    for ( @file ) {
      print if /doc_root_path/;
      s/doc_root_path/$doc_root_path/g;
    }
    my $in_sub = 0;
    my $sub;
    foreach ( @file ) {
	next unless ( /^start/ or $in_sub );
	if ( /^start/ ) {
	    $in_sub = 1;
	    my @row = split;
	    $sub = $row[1];
#	    print "$sub\n";
	    next;
	}
	unless ( /^\s*\#/ or /^\s*\{/ ) {
	    $in_sub = 0;
	    next;
	}
	unless ( /^\s*\{/ ) {
	    s/^\s*\#//;
	    push( @{$doc{$sub}}, $_ );
	}
    }
    return %doc;
}

sub set_paths {
    my ( $objecttypeset, $nameset, $positionset, $rectangleset ) = @_;
    my @packages = ();
    my @names = ();
    my @positions = ();
    my @rectangles = ();
    
    foreach my $node ($objecttypeset->get_nodelist) {
	my $type = $node -> getValue;
	if ( $type eq 'UML - LargePackage' ) {
	    push( @packages, 1 );
	} else {
	    push( @packages, 0 );
	}
    }

    foreach my $node ($nameset->get_nodelist) {
	my $name = $node -> getValue;
	$name =~s/#//g;
	push( @names, $name );
    }

    foreach my $node ($positionset->get_nodelist) {
	my @position = split(',',$node -> getValue);
	push( @positions, \@position );
    }

    foreach my $node ($rectangleset->get_nodelist) {
#	print "TYPE: ",$node -> getNodeType,"\n";
	my @rectangle = split(/[,;]/,$node -> getValue);
	push( @rectangles, \@rectangle );
    }

    sub find_dep {
	my ( $pac_ref, $names_ref, $pos_ref, $rec_ref, $only_test ) = @_;
	my @packages = @{$pac_ref};
	my @names = @{$names_ref};
	my @pos   = @{$pos_ref};
	my @bb    = @{$rec_ref};
	
#	print "Called with ",scalar @{$pos_ref}," positions\n",
#	"\t",scalar @bb," rectangles and ", scalar @names," names\n";
#	print "Names: @names\n";
#	print "Position 0: @{$pos[0]}\n";
	my @paths = ();
	for ( my $i = 0; $i <= $#pos; $i++ ) {
	    next if ( defined $only_test and $only_test != $i );
	    my $width = undef;
	    my $holding_id = undef;
	    for ( my $j = 0; $j <= $#bb; $j++ ) {
		next if ( $j == $i or not $packages[$j] );
		if ( $pos[$i][0] > $bb[$j][0] and $pos[$i][0] < $bb[$j][2] and
		     $pos[$i][1] > $bb[$j][1] and $pos[$i][1] < $bb[$j][3] ) {
		    # Found a package that holds the given position
#		    print "WIDTH: ",$bb[$j][2] - $bb[$j][0],"\n";
		    if ( not defined $width or 
			 ($bb[$j][2] - $bb[$j][0] < $width) ) {
			$width = $bb[$j][2] - $bb[$j][0];
			$holding_id = $j;
#			print "YES! $names[$i] in $names[$j]\n";
		    }
		}
	    }
	    if ( defined $holding_id ) {
		my (@nc, @nn, @np, @nr) = ((),(),(),());
		my $l = 0;
		my $test_only;
		for ( my $k = 0; $k <= $#names; $k++ ) {
		    next if ( $k == $i );
#		    print "Pushing ",$names[$k],"\n";
		    push( @nc, $packages[$k] );
		    push( @nn, $names[$k] );
		    push( @nr, $bb[$k] );
		    push( @np, $pos[$k] );
		    $test_only = $l if ( $k == $holding_id );
		    $l++;
		}
		my @innerpath = &find_dep( \@nc, \@nn, \@np, \@nr, $test_only );
		if ( defined $innerpath[0] ) {
		    push( @paths, $innerpath[0].'/'.$names[$holding_id] );
		} else {
		    push( @paths, $names[$holding_id] );
		}
#		print "$names[$i] PATH: $paths[$#paths]\n";
	    } else {
		push( @paths, undef );
	    }
	}
	return @paths;
    }

    my @paths = &find_dep( \@packages, \@names, \@positions, \@rectangles );
    my ( @cl_paths, @pac_paths ) = ((),());
    for ( my $i = 0; $i <= $#paths; $i++ ) {
	if ( $packages[$i] ) {
	    push( @pac_paths, $paths[$i] );
	} else {
	    push( @cl_paths, $paths[$i] );
	} 
#	print "$packages[$i]\t$names[$i]:\t$paths[$i]\n";
    }
    return ( \@paths, \@cl_paths, \@pac_paths );
}

die;


# DOM TYPE specifications
#         UNKNOWN_NODE (0)                The node type is unknown (not part of DOM)
                                                                                                
#         ELEMENT_NODE (1)                The node is an Element.
#         ATTRIBUTE_NODE (2)              The node is an Attr.
#         TEXT_NODE (3)                   The node is a Text node.
#         CDATA_SECTION_NODE (4)          The node is a CDATASection.
#         ENTITY_REFERENCE_NODE (5)       The node is an EntityReference.
#         ENTITY_NODE (6)                 The node is an Entity.
#         PROCESSING_INSTRUCTION_NODE (7) The node is a ProcessingInstruction.
#         COMMENT_NODE (8)                The node is a Comment.
#         DOCUMENT_NODE (9)               The node is a Document.
#         DOCUMENT_TYPE_NODE (10)         The node is a DocumentType.
#         DOCUMENT_FRAGMENT_NODE (11)     The node is a DocumentFragment.
#         NOTATION_NODE (12)              The node is a Notation.
                                                                                                
#         ELEMENT_DECL_NODE (13)          The node is an ElementDecl (not part of DOM)
#         ATT_DEF_NODE (14)               The node is an AttDef (not part of DOM)
#         XML_DECL_NODE (15)              The node is an XMLDecl (not part of DOM)
#         ATTLIST_DECL_NODE (16)          The node is an AttlistDecl (not part of DOM)





my $nodeset;
foreach my $node ($nodeset->get_nodelist) {
    print "FOUND\n\n",
        XML::XPath::XMLParser::as_string($node),
        "\n\n";
}



my $parser = XML::LibXML -> new;

my $doc = $parser -> parse_file( $ARGV[0] );

my $root = $doc -> getDocumentElement;




#&traverse_elements( $root );

my @object_layer;
if ( $root -> hasChildNodes ) {
    my @rchildren = $root -> getChildnodes;
    foreach my $rchild ( @rchildren ) {
	if ( $rchild -> getName eq 'dia:layer' and
	     $rchild -> getAttribute( 'name' ) eq 'Background' ) {
	    print "found Background\n";
	    @object_layer = $rchild -> getChildnodes;
	}
    }
}

my @classes;
foreach my $perhaps_object ( @object_layer ) {
    if ( $perhaps_object -> getName eq 'dia:object' ) {
	print "Object of type ",$perhaps_object -> getAttribute( 'type' ),"\n";
	if ( $perhaps_object -> getAttribute( 'type' ) eq 'UML - Class' ) {
	    push( @classes, [ $perhaps_object, &get_path( $perhaps_object, \@object_layer )] );
	}
    }
} 

sub get_attr {
    my $type  = shift;
    my $class = shift;
    print "type $type\n";
    print "TYPE: ",$class -> getAttribute('type'),"\n";
#    my @attributes = $class -> getChildnodes;
    my @attributes = $class -> childNodes; # equiv to getChildnodes?
    print "ATTRIBUTES: ",scalar @attributes,"\n";
    my $i =1;
    foreach my $attr ( @attributes ) {
	print $i++,"\t",ref($attr),"\n";
	if ( ref( $attr ) eq 'XML::LibXML::Element' ) {
	    print "NAME: ",$attr -> getAttribute( 'name' ),"\n";
	} elsif (ref( $attr ) eq 'XML::LibXML::Text' ) {
	    print "TEXT: ",$attr -> textContent,"\n";
	}	    
	if ( ref( $attr ) eq 'XML::LibXML::Element' and 
	     $attr -> getAttribute( 'name' ) eq $type ) {
	    my @attr_children = $attr -> getChildnodes;
	    my $j = 1;
	    foreach my $attrchild ( @attr_children ) {
		print "Child ",$j++,"\t",ref($attrchild),"\n";
		if ( ref( $attrchild ) eq 'XML::LibXML::Element' ) {
		    print "NAME: ",$attrchild -> getAttribute( 'name' ),"\n";
		    print "Value: ",$attrchild -> nodeValue,"\n";
		}
	    }
	    print "DIA NAME: ",$attr_children[1] -> getName,"\n";
	    if ( $attr_children[1] -> getName eq 'dia:string' ) {
		print  "Value2: ",$attr_children[1] -> getValue,"\n";
		return $attr_children[1] -> nodeValue;
	    } else{
		return $attr_children[1] -> getAttribute( 'val' );
	    }
	}
    }
}

sub get_path {
    my $perhaps_object = shift;
    my $position = &get_attr( 'obj_pos', $perhaps_object);
    my $obj_ref = shift;
    my @objects = @{$obj_ref};
    my @pos = split(',', $position);
    my $width = undef;
    my $inner_package = undef;
    foreach my $object ( @objects ) {
	next if $object -> isSameNode( $perhaps_object );
	if ( $object -> getName eq 'dia:object' and
	     $object -> getAttribute( 'type' ) eq 'UML - LargePackage') {
	    my @bb = split(/[,;]/,&get_attr( 'obj_bb', $object ));
	    print "POS: ",$pos[0],',',$pos[1],"\n";
	    print "X: ",$bb[0],',',$bb[2],"\n";
	    print "Y: ",$bb[1],',',$bb[3],"\n";
	    if ( $pos[0] > $bb[0] and $pos[0] < $bb[2] and
		 $pos[1] > $bb[1] and $pos[1] < $bb[3] ) {
		# Found a package that holds the given position
		print "WIDTH: ",$bb[2] - $bb[0],"\n";
		if ( not defined $width or 
		     ($bb[2] - $bb[0] < $width) ) {
		    $width = $bb[2] - $bb[0];
		    $inner_package = $object;
		    print "YES!\n";
		}
	    }
	}
    }
    my $path;
    if ( defined $inner_package ) {
	$path = &get_attr( 'name', $inner_package );
	print "PATH: ",$path,"\n";
	my $deep_path = &get_path( $inner_package, $obj_ref );
	$path = defined $deep_path ? $path.'/'.$deep_path : $path;
    }
    print "Returning path: $path\n";
    return $path;
}


print scalar @object_layer,"\n";

sub traverse_elements {
    my $elem = shift;
    print "REF: ",ref( $elem ),"\n";
#    if ( ref( $elem ) eq 'XML::LibXML::Element' ) {
	print "NAME: ",$elem -> getName,"\n";
	print "TEXT: ",$elem -> getValue,"\n";
#	print "DATA: ",$elem -> getData,"\n";
#    }
 

    if( $elem -> hasChildNodes ) {
	my @children = $elem -> getChildnodes;
	foreach my $child ( @children ) {
	    &traverse_elements( $child );
	}
    }
}
