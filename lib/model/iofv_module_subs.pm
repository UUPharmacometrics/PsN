# {{{ include statements

start include statements
use Carp;
use Config;
end include statements


# {{{ Original Documentation

###############################################################
#             		     INTRODUCTION                     #
###############################################################

# Individual objective function values can be a useful model 
# selection diagnostic. Their use in the selection of covariates
# for inclusion in a non-linear mixed effects model is descibed 
# in: Sadray, Jonsson and Karlsson, PharmRes, 16(8) 1999, 
# pp. 1260-1265 

# This script calculates the individual difference in objective
# function value between two models ("basic" and "full").  Optionally
# it also creates a file with the necessary fortran sub-routine.

# To use the script, you need perl installed on your system. Perl
# can be obtained free of charge from www.perl.com. Perl, and this
# script, can be used on both UNIX machines and PCs.

# Questions, comments and bug reports should be sent to 
# niclas.jonsson@biof.uu.se.

# (c) Copyright 1999 Niclas Jonsson and Mats Karlsson


################################################################
#       	   TO CREATE THE CONTR SUBROUTINE              #
################################################################

# To create the CONTR subroutine that is necessary to extract
# the individual objective function values, do the following:

#  1. On the command line, type: 
#     perl ofv1 -p

#     This creates a file called iofvcont.f.

#  2. Open the iofvcont.f file for editing and change the 
#     parameter statement on line two to reflect the number of
#     observations you have NONMEM compiled for. (The NONMEM 
#     default is 50.)

#  3. Open the NM-TRAN model file for which you want to obtain
#     individual objective function values and add the following line 
#     before the $SUBROUTINE line:
#     $CONTR DATA=(ID)

#  4. Change the $SUBROUTINE line to include the iofvcont.f file, e.g.:
#     $SUBROUTINE ADVAN4 TRANS2 CONTR=iofvcont.f

#  5. When the model is run, a file called fort80 (or something similar)
#     is created. It containes two columns. The first is the ID numbers
#     and the second the individual objectives.
#     There are (no of individuals)*(number of function evaluations) lines 
#     the file. Only the last (no of individuals) lines are of interest.

#########################################################################
# TO COMPUTE THE INDIVIDUAL DIFFERENCES IN THE OBJECTIVE FUNCTION VALUE #
#                          BETWEEN TWO MODELS                           #
#########################################################################

# BACKGROUND

# The script assumes that the ID and associated individual objective
# function value are stored in two files.  These files will
# automatically be the output from the "iofvcont.f" file although
# the user has to rename the output file after each run.

# This is what you have to do in order to get a list of individual
# objective value differences between two models:

# 1. Create the iofvcont.f file (see above):

# 2. For each model you run, a file called "fort80" (or similar,
#    depending on your compiler) will be created.  It will usually be
#    very long and only the last number for each individual will be
#    used, but that is taken care of by the script.  What you need to
#    do is to rename this file after every run (to assure that it is
#    not written over).  If basic and final models are runs 5 and 6,
#    the following code can serve as example:

#    nmfe run5.mod run5.lst
#    mv ftn80 iotab5 
#    nmfe run6.mod run6.lst
#    mv ftn80 iotab6 

# 3. When you have run the basic and full model run the perl script by
#    typing: 
#    perl ofv1

#    Thereafter follows instructions. Names of output files will be given.

# }}}

# {{{ new

start new
{
  my $base_model = $this->base_model;
  
  if( $base_model -> is_option_set( record => 'subroutine',
				    name => 'CONTR' ) ){

    croak('CONTR in $SUBROUTINE is already set, iofv cannot be computed' );

  }

  $base_model -> add_records( type => 'subroutine',
			      record_strings => ['CONTR=iofvcont.f'] );

  $base_model -> set_records( type => 'contr',
			      record_strings => ['DATA=(ID)'] );
}
end new

# }}}

# {{{ post_process

start post_process
{

  if( $PsN::nm_major_version == '7' ){
    croak("option -iofv is not yet supported for NONMEM7");
  }


  my $sizes_dir = $PsN::nmdir;
  $sizes_dir .= '/SIZES';
  if( $Config{osname} eq 'MSWin32' ){
    $sizes_dir =~ s/([^\\])\\([^\\])/$1\\\\$2/g;
    $sizes_dir =~ s/\//\\\\/g;
  }

  my $base_model = $self->base_model;

  # Figure out if we have an sdtab and what number it has
  my ( $sd_ref, $junk ) = $base_model -> problems -> [0] -> 
      _option_val_pos( name        => 'FILE',
		       record_name => 'table',
		       exact_match => 0 );

  my $sdno = '1';

  if( defined $sd_ref ) {
    foreach my $tabname ( @{$sd_ref} ) {
      if( $tabname =~ /[sd,pa]tab(\d+)/ ) {
	$sdno= $1;
      }
    }
  }

  open(CONTR,">iofvcont.f");
  print CONTR << "EOF";
      subroutine contr (icall,cnt,ier1,ier2)
      INCLUDE "$sizes_dir"
C     parameter (no=50)
      common /rocm1/ y(no),data(no,3),nobs
      integer nobs, un
      double precision cnt,y
      OPEN(80,FILE=\'iotab$sdno\')
      if (icall.le.1) return
      call ncontr (cnt,ier1,ier2,l2r)
C     individual obj. funct. value for indiv. jj = cnt
      write(80,10) data(1,1),cnt
   10 FORMAT(1E12.4E2,1E12.4E2) 
      return
      end

EOF
 
    close CONTR;

}
end post_process

# }}}

# {{{ post_run_process
start post_run_process
{
  my $base_model = $self->base_model;

  # Figure out if we have an sdtab and what number it has
  my ( $sd_ref, $junk ) = $base_model -> problems -> [0] -> 
      _option_val_pos( name        => 'FILE',
		       record_name => 'table',
		       exact_match => 0 );

  my $sdno = '1';

  if( defined $sd_ref ) {
    foreach my $tabname ( @{$sd_ref} ) {
      if( $tabname =~ /[sd,pa]tab(\d+)/ ) {
	$sdno= $1;
      }
    }
  }

  my $data = $base_model -> datas -> [0];
  
  my $ids = $data -> column_to_array( column => $data -> idcolumn -1 );

  my ($first_id, $last_id) = ($ids -> [0],$ids -> [$#{$ids}]);

  if( -e "iotab$sdno" ){
    open( IOTAB, "<iotab$sdno" );
    my @iotab = <IOTAB>;
    my @values;
    my @starting_points;

    my $previous_id = $last_id;

    for( my $i = 0;$i < scalar @iotab; $i++ ){
      my @line = split( ' ',$iotab[$i] );
      
      push( @values, $line[1] );

      if( $previous_id == $last_id and $line[0] == $first_id ){
	push( @starting_points, $i );
      }

      $previous_id = $line[0];
    }

    if( defined $base_model -> covariance ){

      @iotab = @iotab[$starting_points[$#starting_points-1] .. $starting_points[$#starting_points]-1];

    } else {

      @iotab = @iotab[$starting_points[$#starting_points-1] .. $starting_points[$#starting_points]-1];

    }
    close(IOTAB);

    open IOTAB, ">iotab$sdno";
    print( IOTAB @iotab );
    close IOTAB;

    my @eo;
    if ( defined $base_model -> extra_output() ) {
      @eo = @{$base_model -> extra_output()};
    }
    
    push( @eo, "iotab$sdno" );
    $base_model -> extra_output( \@eo );
    
  } else {
    croak("Unable to open iotab: iotab$sdno ." );
  }
}
end post_run_process
# }}}

print("Enter the name of the file with the OFVs for the basic model:\n");
$basic = <STDIN>;

print("Enter the name of the file with the OFVs for the full model:\n");
$full = <STDIN>;

print("Enter the number of subjects:\n"); 
$ind = <STDIN> ;

chomp($basic);
chomp($full);

$out1 = "deltofv1.res";

## Open the files with OFVs and extract the relevant values
open (FILE1, $basic) || die("Couldn't open $basic for reading!\n");
while(<FILE1>) {
  chomp;
  ($id,$basic_mof) = split(' ',$_);
  push @basic, $basic_mof;
  push @id, $id;
}
@basic = splice(@basic,-$ind);
@ids   = splice(@id,-$ind);
close FILE1;

open (FILE2, $full) || die("Couldn't open $full for reading!\n");
while(<FILE2>) {
  chomp;
  ($junk,$full_mof) = split(' ',$_);
  push @full, $full_mof;
}
@full = splice(@full,-$ind);
close FILE2;

## Compute the differences

foreach $i (0..@basic-1) {
  push @diff, $basic[$i] - $full[$i];
}

## Create the arrays to be printed
foreach $diff (@diff) {
  push @unsorted, sprintf "%12.4e%12.4e", $ids[$index],$diff[$index];
  $index++;
}

@sorted = 
  map  {$_-> [0]}
  sort {$a -> [1] <=> $b ->[1]}
  map  {[$_, (split ' ')[1]]}
@unsorted;

open (OUTFILE1, ">$out1") || 
  die ("Cannot open output file $out1 for writing!\n");

print OUTFILE1 ("ID DELTAOFV SORTID SORTDELTAOFV\n");
$index = 0;
foreach $diff (@diff) {
  printf OUTFILE1 "%s    %s\n", $unsorted[$index],$sorted[$index];
  $index++;
}
close OUTFILE1;
print ("Output are in file ",$out1,"\n");

