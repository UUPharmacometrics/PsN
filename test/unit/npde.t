#!/etc/bin/perl


use strict;
use warnings;
use Test::More;	#count them!
use Test::Exception;
use File::Path 'rmtree';	
use FindBin qw($Bin);
use lib "$Bin/.."; #location of includes.pm
use includes; #file with paths to PsN packages
use npde_util;
use file;
use tool::ebe_npde;

my $filedir = $includes::testfiledir . '/npde/';
#my @file_array=($filedir.'original.etas',$filedir.'sim-1.etas',$filedir.'sim-2.etas',$filedir.'sim-3.etas');
my @file_array=($filedir.'original.phi',$filedir.'sim-1.phi',$filedir.'sim-2.phi',$filedir.'sim-3.phi');

#my @headers = ('ETA1','ETA2');
my @headers = ('ETA(1)','ETA(2)');

my $est_matrix=[];
my $mean_matrix=[];

my $ok = npde_util::read_table_files(\@file_array,\@headers,$est_matrix,$mean_matrix,1);
	#in reference to empty array to put results [over columns][over individuals][over samples/files]
	#in reference to empty array to put mean [over columns][over individuals] without original


is ($ok, 0, "read_table_files return status");

cmp_ok($est_matrix->[0]->[0]->[0],'==',-5.50879E-02,'ETA1 ind 1 original');
cmp_ok($est_matrix->[0]->[1]->[0],'==',-3.34657E-01,'ETA1 ind 2 original');
cmp_ok($est_matrix->[1]->[3]->[0],'==',-4.50197E-01,'ETA2 ind 4 original');
cmp_ok($est_matrix->[0]->[0]->[1],'==',-2.46807E-01,'ETA1 ind 1 sim 1');
cmp_ok($est_matrix->[1]->[0]->[1],'==',2.10681E-01,'ETA2 ind 1 sim 1');
cmp_ok($est_matrix->[1]->[4]->[1],'==',1.16354E-01,'ETA2 ind 5 sim 1');
cmp_ok($est_matrix->[0]->[1]->[2],'==',1.44944E-01,'ETA1 ind 2 sim 2');
cmp_ok($est_matrix->[1]->[2]->[2],'==',1.14446E-01,'ETA2 ind 3 sim 2');
cmp_ok($est_matrix->[1]->[4]->[2],'==',-4.12498E-01,'ETA2 ind 5 sim 2');
cmp_ok($est_matrix->[0]->[1]->[3],'==',-4.66493E-02,'ETA1 ind 2 sim 3');
cmp_ok($est_matrix->[1]->[2]->[3],'==',-1.60687E-01,'ETA2 ind 3 sim 3');
cmp_ok($est_matrix->[1]->[4]->[3],'==',9.17580E-02,'ETA2 ind 5 sim 3');

my $diff = 1E-10;
cmp_ok(abs($mean_matrix->[0]->[0]-(-0.4060175/3)),'<',$diff,'mean ETA1 ind 1');
cmp_ok(abs($mean_matrix->[0]->[2]-(-0.1616416/3)),'<',$diff,'mean ETA1 ind 3');
cmp_ok(abs($mean_matrix->[0]->[4]-0.272154/3),'<',$diff,'mean ETA1 ind 5');
cmp_ok(abs($mean_matrix->[1]->[1]-0.76082/3),'<',$diff,'mean ETA2 ind 2');
cmp_ok(abs($mean_matrix->[1]->[3]-0.0576/3),'<',$diff,'mean ETA2 ind 4');
cmp_ok(abs($mean_matrix->[1]->[4]-(-0.204386/3)),'<',$diff,'mean ETA2 ind 5');


if (0){
	for (my $i=0;$i < scalar(@{$est_matrix->[0]}); $i++){
		for (my $k=0;$k < scalar(@{$est_matrix->[0]->[$i]}); $k++){
			for (my $j=0;$j < scalar(@{$est_matrix}); $j++){
				print $est_matrix->[$j]->[$i]->[$k]." ";
			}
			print "\n";
		}
		print "\n";
	}
}

my $decorr = [];
my $shrink;
$ok = npde_util::decorrelation($est_matrix,$mean_matrix,$decorr,$shrink);
is ($ok, 0, "decorrelation return status");

#param #indiv $sample

cmp_ok(abs($decorr->[0]->[0]->[0]-(0.6059160950135)),'<',$diff,'decorr orig ETA1 ind 1');
cmp_ok(abs($decorr->[1]->[0]->[0]-(9.2945678814619)),'<',$diff,'decorr orig ETA2 ind 1');
cmp_ok(abs($decorr->[0]->[1]->[0]-(-3.9217365078990)),'<',$diff,'decorr orig ETA1 ind 2');
cmp_ok(abs($decorr->[1]->[1]->[0]-(101.9532558734349)),'<',$diff,'decorr orig ETA2 ind 2');
cmp_ok(abs($decorr->[0]->[2]->[0]-(4.5160319298809)),'<',$diff,'decorr orig ETA1 ind 3');
cmp_ok(abs($decorr->[1]->[2]->[0]-(-349.1157719927939)),'<',$diff,'decorr orig ETA2 ind 3');
cmp_ok(abs($decorr->[0]->[3]->[0]-(-0.0829056697377)),'<',$diff,'decorr orig ETA1 ind 4');
cmp_ok(abs($decorr->[1]->[3]->[0]-(-3.3232965189893)),'<',$diff,'decorr orig ETA2 ind 4');
cmp_ok(abs($decorr->[0]->[4]->[0]-(0.8133168467060)),'<',$diff,'decorr orig ETA1 ind 5');
cmp_ok(abs($decorr->[1]->[4]->[0]-(4.3913013766347)),'<',$diff,'decorr orig ETA2 ind 5');

cmp_ok(abs($decorr->[0]->[0]->[1]-(-0.841608576391480)),'<',$diff,'decorr sim1 ETA1 ind 1');
cmp_ok(abs($decorr->[1]->[0]->[1]-(-0.790587337033390)),'<',$diff,'decorr sim1 ETA2 ind 1');
cmp_ok(abs($decorr->[0]->[1]->[1]-(0.442611071700808)),'<',$diff,'decorr sim1 ETA1 ind 2');
cmp_ok(abs($decorr->[1]->[1]->[1]-(1.066503057914682)),'<',$diff,'decorr sim1 ETA2 ind 2');
cmp_ok(abs($decorr->[0]->[2]->[1]-(-0.490641459420216)),'<',$diff,'decorr sim1 ETA1 ind 3');
cmp_ok(abs($decorr->[1]->[2]->[1]-(-1.045277136280930)),'<',$diff,'decorr sim1 ETA2 ind 3');
cmp_ok(abs($decorr->[0]->[3]->[1]-(1.087900848979833)),'<',$diff,'decorr sim1 ETA1 ind 4');
cmp_ok(abs($decorr->[1]->[3]->[1]-(-0.387046607170625)),'<',$diff,'decorr sim1 ETA2 ind 4');
cmp_ok(abs($decorr->[0]->[4]->[1]-(-0.976156115546330)),'<',$diff,'decorr sim1 ETA1 ind 5');
cmp_ok(abs($decorr->[1]->[4]->[1]-(-0.616808374955168)),'<',$diff,'decorr sim1 ETA2 ind 5');

cmp_ok(abs($decorr->[0]->[0]->[3]-(1.105473005976996)),'<',$diff,'decorr sim3 ETA1 ind 1');
cmp_ok(abs($decorr->[1]->[0]->[3]-(-0.333560738681136)),'<',$diff,'decorr sim3 ETA2 ind 1');
cmp_ok(abs($decorr->[0]->[1]->[3]-(-1.144924277218235)),'<',$diff,'decorr sim3 ETA1 ind 2');
cmp_ok(abs($decorr->[1]->[1]->[3]-(-0.149939096868167)),'<',$diff,'decorr sim3 ETA2 ind 2');
cmp_ok(abs($decorr->[0]->[2]->[3]-(-0.659915824304090)),'<',$diff,'decorr sim3 ETA1 ind 3');
cmp_ok(abs($decorr->[1]->[2]->[3]-(0.947546536148295)),'<',$diff,'decorr sim3 ETA2 ind 3');
cmp_ok(abs($decorr->[0]->[3]->[3]-(-0.879142618748254)),'<',$diff,'decorr sim3 ETA1 ind 4');
cmp_ok(abs($decorr->[1]->[3]->[3]-(-0.748626468429881)),'<',$diff,'decorr sim3 ETA2 ind 4');
cmp_ok(abs($decorr->[0]->[4]->[3]-(-0.046093664205007)),'<',$diff,'decorr sim3 ETA1 ind 5');
cmp_ok(abs($decorr->[1]->[4]->[3]-(1.153780181600242)),'<',$diff,'decorr sim3 ETA2 ind 5');

for (my $i=0;$i < scalar(@{$decorr->[0]}); $i++){
	for (my $k=0;$k < scalar(@{$decorr->[0]->[$i]}); $k++){
		for (my $j=0;$j < scalar(@{$decorr}); $j++){
			print $decorr->[$j]->[$i]->[$k]." ";
		}
		print "\n";
	}
	print "\n";
}
   
my $npde = [];
my $pde=[];
$ok = npde_util::npde_comp($decorr,$pde,$npde);
is ($ok, 0, "npde_comp eta return status");

for (my $i=0;$i < scalar(@{$pde->[0]}); $i++){
	for (my $j=0;$j < scalar(@{$pde}); $j++){
		print $pde->[$j]->[$i].' ';
	}
	print "\n";
}



@headers = ('OBJ');
$est_matrix=[];
$mean_matrix=[];
my $ok = npde_util::read_table_files(\@file_array,\@headers,$est_matrix,$mean_matrix,1);
	#in reference to empty array to put results [over columns][over individuals][over samples/files]
	#in reference to empty array to put mean [over columns][over individuals] without original


is ($ok, 0, "read_table_files obj return status");
cmp_ok($est_matrix->[0]->[0]->[0],'==',6.2233704337784372,'objv ind 1 original');
cmp_ok($est_matrix->[0]->[3]->[0],'==',11.873179897235525,'objv ind 4 original');
cmp_ok($est_matrix->[0]->[1]->[1],'==',8.5706676932580130,'objv ind 2 sim 1');
cmp_ok($est_matrix->[0]->[4]->[2],'==',14.846316011886847,'objv ind 5 sim 2  ');
cmp_ok($est_matrix->[0]->[2]->[3],'==',11.088612997217446,'objv ind 3 sim 3');
cmp_ok($est_matrix->[0]->[4]->[3],'==',8.4664237311178105,'objv ind 5 sim 3');


@file_array=($filedir.'original_iwres.dta',$filedir.'iwres-1.dta',$filedir.'iwres-2.dta',$filedir.'iwres-3.dta');
@headers = ('IWRES');
$est_matrix=[];
$mean_matrix=[];
my $ok = npde_util::read_table_files(\@file_array,\@headers,$est_matrix,$mean_matrix,1);
is ($ok, 0, "read_table_files iwres return status");
cmp_ok($est_matrix->[0]->[1]->[0],'==',-8.2625E-03,'iwres rec 2 original');
cmp_ok($est_matrix->[0]->[26]->[0],'==',-0.00096254,'iwres rec 27 original');
cmp_ok($est_matrix->[0]->[69]->[0],'==',-1.0819E-01,'iwres rec 70 original');
cmp_ok($est_matrix->[0]->[19]->[1],'==',-5.9717E-02,'iwres rec 20 sim1');
cmp_ok($est_matrix->[0]->[55]->[1],'==',-0.071941,'iwres rec 56 sim1');
cmp_ok($est_matrix->[0]->[11]->[2],'==',0.047954,'iwres rec 12  sim2');
cmp_ok($est_matrix->[0]->[35]->[2],'==',0.044685,'iwres rec 36 sim2');
cmp_ok($est_matrix->[0]->[41]->[3],'==',-0.027903,'iwres rec 42 sim3');
cmp_ok($est_matrix->[0]->[69]->[3],'==',-0.00014352,'iwres rec 70  sim3');

if (0){
	for (my $i=0;$i < scalar(@{$est_matrix->[0]}); $i++){
		for (my $k=0;$k < scalar(@{$est_matrix->[0]->[$i]}); $k++){
			for (my $j=0;$j < scalar(@{$est_matrix}); $j++){
				print $est_matrix->[$j]->[$i]->[$k]." ";
			}
			print "\n";
		}
		print "\n";
	}
}

$decorr = [];
$shrink;
$ok = npde_util::decorrelation($est_matrix,$mean_matrix,$decorr,$shrink);
is ($ok, 0, "decorrelation iwres return status");

if (0){
	for (my $i=0;$i < scalar(@{$decorr->[0]}); $i++){
#		for (my $k=0;$k < scalar(@{$decorr->[0]->[$i]}); $k++){
		my $k=0;
			for (my $j=0;$j < scalar(@{$decorr}); $j++){
				print $i.' '.$decorr->[$j]->[$i]->[$k]." ";
			}
			print "\n";
#		}
#		print "\n";
	}
}

cmp_ok(abs($decorr->[0]->[1]->[0]-(0.638611548099674)),'<',$diff,'decorr iwres orig obs 2');
cmp_ok(abs($decorr->[0]->[11]->[0]-(-0.736694095480004)),'<',$diff,'decorr iwres orig obs 12');
cmp_ok(abs($decorr->[0]->[13]->[0]-(-1.203446153572685)),'<',$diff,'decorr iwres orig obs 14');
cmp_ok(abs($decorr->[0]->[19]->[0]-(2.609820823136891)),'<',$diff,'decorr iwres orig obs 20');
cmp_ok(abs($decorr->[0]->[26]->[0]-(0.686777491567809)),'<',$diff,'decorr iwres orig obs 27');
cmp_ok(abs($decorr->[0]->[28]->[0]-(7.707426070595523)),'<',$diff,'decorr iwres orig obs 29');
cmp_ok(abs($decorr->[0]->[35]->[0]-(-1.364989105237338)),'<',$diff,'decorr iwres orig obs 36');
cmp_ok(abs($decorr->[0]->[41]->[0]-(-7.737241423980805)),'<',$diff,'decorr iwres orig obs 42');
cmp_ok(abs($decorr->[0]->[43]->[0]-(1.997089891305987)),'<',$diff,'decorr iwres orig obs 44');
cmp_ok(abs($decorr->[0]->[48]->[0]-(-8.603521487423736)),'<',$diff,'decorr iwres orig obs 49');
cmp_ok(abs($decorr->[0]->[55]->[0]-(0.552813297977162)),'<',$diff,'decorr iwres orig obs 56');
cmp_ok(abs($decorr->[0]->[57]->[0]-(0.248672597279443)),'<',$diff,'decorr iwres orig obs 58');
cmp_ok(abs($decorr->[0]->[62]->[0]-(0.177695160276012)),'<',$diff,'decorr iwres orig obs 63');
cmp_ok(abs($decorr->[0]->[69]->[0]-(-1.781701458701277)),'<',$diff,'decorr iwres orig obs 70');



@file_array=($filedir.'original_iwres.dta');
@headers = ('ID','MDV');
$est_matrix=[];
$mean_matrix=[];
my $ok = npde_util::read_table_files(\@file_array,\@headers,$est_matrix,$mean_matrix,0);
is ($ok, 0, "read_table_files id mdv return status");

cmp_ok($est_matrix->[0]->[0]->[0],'==',1,'id rec 1 original');
cmp_ok($est_matrix->[0]->[39]->[0],'==',3,'id rec 40 original');
cmp_ok($est_matrix->[0]->[64]->[0],'==',5,'id rec 65 original');
cmp_ok($est_matrix->[1]->[10]->[0],'==',1,'mdv rec 11  original');
cmp_ok($est_matrix->[1]->[35]->[0],'==',0,'mdv rec 36 original');
cmp_ok($est_matrix->[1]->[62]->[0],'==',0,'mdv rec 63 original');


done_testing();
