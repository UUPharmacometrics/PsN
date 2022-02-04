package model::problem::record;

use include_modules;
use Mouse;
use MouseX::Params::Validate;

use model::problem::record::option;

has 'options' => ( is => 'rw', isa => 'ArrayRef[model::problem::record::option]', default => sub { [] } );
has 'record_arr' => ( is => 'rw', isa => 'Maybe[ArrayRef]', clearer => 'clear_record_arr' );
has 'comment' => ( is => 'rw', isa => 'ArrayRef', default => sub { [] } );
has 'print_order' => ( is => 'rw', isa => 'ArrayRef[Int]', default => sub { [] } );

sub BUILD
{
    my $self = shift;

    # To construct an option you only need to supply an array of
    # strings containg the record block. _read_option then parses
    # those strings.
    $self->_read_options;
    $self->clear_record_arr;
}

sub add_option
{
    my ($self, %parm) = validated_hash(\@_,
        init_data => { isa => 'Any', optional => 0 }
    );
    push(@{$self->options}, model::problem::record::option->new(%{$parm{'init_data'}}));
}

sub remove_option
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        name => { isa => 'Str', optional => 1 },
        fuzzy_match => { isa => 'Bool', default => 0, optional => 1 }
    );
    my $name = $parm{'name'};
    my $fuzzy_match = $parm{'fuzzy_match'};

    my @options = @{$self->options};
    my @new_options = ();
    foreach my $option ( @options ) {
        next if ( $option -> name eq $name );

        next if ( $fuzzy_match and index( $name, $option -> name ) == 0 );
        next if ( $fuzzy_match and index(  $option -> name,$name ) == 0 ); #experimental

        push( @new_options, $option );
    }
    $self->options(\@new_options);
}

sub _add_option
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        option_string => { isa => 'Str', optional => 1 }
    );
    my $option_string = $parm{'option_string'};

    # Create a new option. $option_string should be of the form
    # "option=value". TODO catch any error from below.
    my $opt_obj = model::problem::record::option->new(option_string => $option_string);
    if ($opt_obj) {
        push(@{$self->options}, $opt_obj);
    }
}

sub _read_options
{
    my $self = shift;

    #
    # record::_read_options
    #

    # This is a tricky method that parses options. The basic parsing
    # is real easy, it loops over the records strings, for each
    # string it looks for options of the form "option=value" or
    # "option", those substrings are then used to creat options. But
    # while the parser does this it keeps count of the number of
    # options it has parsed, and pushes it on the "print_order"
    # array (which has a bit missleading name) when it finds a
    # comment. This is then used to remember where comments appear
    # in the NONMEM modelfile. Unfortunataly this only holds for
    # ordinary records, code records are cut verbatim from the
    # modelfile, so they will also have their comments in place, but
    # init records will loose any comments associated with them.

    # TODO This is a hack, I admit, a nicer way would be to store
    # comments with the option it was found near and defer
    # formatting to the option class.

    my @row = ();
    my $order = 0;
    my $input_record = 0;
    my $first_filename_record = 0;
    # Get the recordname from the class name.
    my @class_names = split('::',ref($self));
    my $fname = uc(pop(@class_names));
    $input_record = 1 if ($fname eq 'INPUT');
    $first_filename_record = 1 if (($fname eq 'DATA') or ($fname eq 'MSFI'));

    # Loop over all given strings.
    my $num = defined $self->record_arr ? scalar(@{$self->record_arr}) : 0;
    for ( my $i = 0; $i < $num; $i++ ) {
        # Store it in $_ for all string matching to look nice
        $_ = $self->record_arr->[$i];

        if( /^\s*(\;.*)$/ ) {
            # This is a comment on a line of its own.
            if( $order == 0 ) {
                push(@{$self->comment}, $1 . "\n");
            } else {
                push(@{$self->comment},"\n" . $1 . "\n");
            }
            # Record after which option the comment appeared.
            push(@{$self->print_order}, $order);
        } else {
            # Get rid of $RECORD
            s/^\s*\$\w+//;
            if ($first_filename_record){
                if (s/^\s*("|')([^"']+)\1\s//){
                    #only do this for first option of first record line
                    #remove quotes from quoted filename with possible spaces
                    $self -> _add_option( option_string => $2 );
                    $order++;
                    $first_filename_record = 0;
                }elsif (/\w/){
                    #have first option (non-empty line) but something not quoted
                    $first_filename_record = 0;
                }
            }
            # remove spaces near '='
            s/\s*([\=])\s*/$1/g;
            # remove spaces after ',' except if  '=,'
            s/([^\=][\,])\s*/$1/g;
            # remove spaces before ','
            s/\s*([\,])/$1/g;
            # remove spaces directly inside parentheses
            s/\s*\)/\)/g;
            s/\(\s*/\(/g;

            # Find trailing comments.
            my $comment;
            if( /(\;.*)$/ ) {
                # We find comments here, but we add it to 'the comment'
                # member after we have parsed all options. Only so we can
                # know how many options we found.

                $comment = ' ' . $1 . "\n";
                # Get rid of trailing comments
                s/\;.*$//g;
            }
            # if INPUT record, replace commas with spaces
            if ($input_record){
                s/,/ /g;
            }

            @row = split; #must handle things like COMP=(ABS DEFDOSE), if enclosed by parentheses group together
            my $optstring='';
            my $append_next_string=0;
            for ( @row ) {
                $optstring .= $_;
                if ($append_next_string){
                    if( /\)\s*$/ ) { #found closing parenthesis
                        $append_next_string=0; #append nothing more, continue through to add_option
                    } else {
                        $optstring .= ' ';
                        next;
                    }
                }elsif( /=\(/ && /[^,\)]\s*$/) { #opening parenthesis without a closing and not ending w comma
                    $append_next_string=1;
                    $optstring .= ' ';
                    next;
                }
                # Create options.
                $self -> _add_option( option_string => $optstring );
                $optstring='';
                $order++;
            }
            if ($append_next_string){
                #did not find closing parenthesis to an opening parenthesis
                croak("Failed parsing of record string: " . $self->record_arr->[$i] . " ");
            }

            if( (defined $comment) and length( $comment ) > 0 ) {
                # This is a comment at the end of a line.
                push(@{$self->comment}, ' ' . $1 . "\n");
                push(@{$self->print_order}, $order);
            }
        }
    }
}

sub _format_record
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        number_format => { isa => 'Maybe[Int]', optional => 1 },
    );
    my $number_format = $parm{'number_format'};
    my @formatted;

    # record::format_record
    #

    # This method might be even more trickier than _read_options,
    # but don't worry, I'll walk you through it. You should read
    # the comments in _read_options first though, it might help.

    # Get the recordname from the class name.
    my @class_names = split('::',ref($self));
    my $fname = uc(pop(@class_names));

    # Get some members from the object.
    my @print_order = defined $self->print_order ? @{$self->print_order} : ();
    my @comments = @{$self->comment};
    my @options = @{$self->options};

    # Each element of @print_order is a number which says how many
    # options(since the previous comment was printed) that should
    # be processed before the next comment should be
    # printed. There will be one element in print order for each
    # comment. So here we intialize $opts_before_comment which is
    # the current number of options we should process. If
    # $opts_before_comment is -1 no more comments will be expected
    # nor printed.

    my $opts_before_comment = scalar @print_order > 0 ? shift @print_order : -1;

    # $last_is_option is a  boolean which is true if  we printed a
    # option in the last iteration  of the loop below and false if
    # we printed a comment. It is  used to see if we need to print
    # an extra  "\n" since comments  is expected to have their own
    # "\n" while options don't

    my $last_is_option = 1;
    my $line = 0;

    # Loop over all options. Actually we loop one step to long,
    # since we might have comments after the last option.
    for (my $i = 0; $i <= scalar(@options); $i++) {

        # See if we have processed enough options to print
        # commments. It is a loop since we might have multiple lines
        # of comments.
        while( $i == $opts_before_comment ){
            my $comment = shift(@comments);

            # add the comment
            $formatted[$line++] .= $comment;

            # If we expect more options ($i <= $#options) and we have
            # printed the recordname ($i > 0) we indent before
            # printing the next option.
            if( $i <= $#options and $i > 0 ){
                $formatted[$line] .= ' ' x 11;
            }

            if( scalar @print_order > 0 ) {
                $opts_before_comment = shift @print_order;
            } else {
                unless( scalar @comments > 0 ){

                    # If we have more comments, it likely mean that
                    # someone has appended comments manually. TODO This
                    # might become a mess and we should probably add a
                    # feature to add commments before or after the record.

                    $opts_before_comment = -1;
                }
            }

            $last_is_option = 0;
        }

        # Print the record name (with indentation)
        if( $i == 0 ){
            my $spaces = '';
            my $count = 10 - length($fname);
            $spaces =  ' ' x ($count) if ($count >0);
            push( @formatted , "\$".$fname . $spaces );
        }

        # Check that we have not processed all options.
        if( $i <= $#options ){
            my $option = $options[$i];
            # Let the option class format the option.
            my $foption = $option -> _format_option;

            # Check and add linebreak if necesary.
            if ( length( $formatted[$line].' '.$foption ) > 70 ){
                $formatted[$line] .= "\n";
                $line++;
                # Indent for next option
                # if this is $PROBLEM then add comment ; since options are actually comments
                if ($fname eq 'PROBLEM'){
                    push( @formatted, ';          '); #10 spaces
                }else{
                    push( @formatted, ' ' x 11 );
                }
            }

            $formatted[$line] .= ' '.$foption;
            $last_is_option = 1;
        }
    }

    # Print a line break if the last item was an option (as oposed
    # to a comment.
    if ($last_is_option) {
        $formatted[$line] .= "\n";
    }

    return \@formatted;
}

sub renumber_msfo
{
    my $self = shift;
    my %parm = validated_hash(\@_,
        numberstring => { isa => 'Str', optional => 0 }
        );
    my $numberstring = $parm{'numberstring'};

    my @options = defined($self->options) ? @{$self->options} : ();
    foreach my $opt (@options){
        if ($opt->name =~ /^\s*MSFO?/  and (defined $opt->value and $opt->value ne '')){
            my $line = $opt->value;
            #everything up to but not including optional dot
            $line =~ s/[0-9]+[^0-9.]*/$numberstring/ ;
            $opt->value($line);
        }
    }
}
sub rename_msfo
{
    my $self = shift;
    my %parm = validated_hash(\@_,
                              name => { isa => 'Str', optional => 0 },
                              add_if_absent => { isa => 'Bool', optional => 1, default => 0 },
        );
    my $name = $parm{'name'};
    my $add_if_absent = $parm{'add_if_absent'};

    my @options = defined($self->options) ? @{$self->options} : ();
    my $found=0;
    foreach my $opt (@options){
        if ($opt->name =~ /^\s*MSFO?\s*$/  and (defined $opt->value and $opt->value ne '')){
            $opt->value($name);
            $found = 1;
        }
    }
    if ($add_if_absent and (not $found)){
        $self -> _add_option( option_string => 'MSFO='.$name );
    }
}

1;
