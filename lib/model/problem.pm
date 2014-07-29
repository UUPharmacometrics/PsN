package model::problem;
#use Carp;
use include_modules;
use Data::Dumper;
my @print_order_omega_before_pk = ('sizes','problem','input','bind','data','abbreviated','msfi','contr','subroutine','prior','thetap','thetapv','omegap','omegapd','sigmap','sigmapd','model','tol','infn','omega','anneal','pk','level','aesinitial','aes','des','error','pred','mix','theta','thetai','thetar','sigma','etas','phis','simulation','estimation','covariance','nonparametric','table','scatter');
my @print_order = ('sizes','problem','input','bind','data','abbreviated','msfi','contr','subroutine','prior','thetap','thetapv','omegap','omegapd','sigmap','sigmapd','model','tol','infn','pk','level','aesinitial','aes','des','error','pred','mix','theta','thetai','thetar','omega','anneal','sigma','etas','phis','simulation','estimation','covariance','nonparametric','table','scatter');
my @sde_print_order = ('sizes','problem','input','bind','data','abbreviated','msfi','contr','subroutine','prior','thetap','thetapv','omegap','omegapd','sigmap','sigmapd','model','tol','infn','theta','thetai','thetar','omega','anneal','sigma','etas','phis','pk','level','aesinitial','aes','des','error','pred','mix','simulation','estimation','covariance','nonparametric','table','scatter');
my %abbreviations;
my %unsupported_records;


# Here we intialize a hash used to find long names for abbreviated
# record names. We use the print_order array which contains all
# allowed record types.


foreach my $record_name( @print_order,'warnings','finedata' ){
	my $uc_short_type = _get_uc_short_type($record_name);
	$abbreviations{$uc_short_type} = $record_name;
}
#foreach my $rec (){
#    $unsupported_records{$rec}=1;
#}


sub _get_uc_short_type{
	my $record_name = shift;

	# As of NM7.3 there are many THETA variants, and the default handling
	# cannot distinguish between THETA and its abbreviations plus THETAI=THI THETAR=THR THETAP THETAPV
	# Require that THETAI THI THETAR THR THETAP THETAPV must be spelled exactly like that, no abbreviations.
	# Treat any other TH:ish string as $THETA

	my $uc_short_type = substr(uc($record_name),0,3);
	if ( $uc_short_type eq 'AES'){
		if (length($record_name)>3){
			#this must be aesinitial
			$uc_short_type = $uc_short_type.'I';
		}else{
			#must be aes
			$uc_short_type = $uc_short_type.' ' ;
		}
	}elsif( $uc_short_type eq 'THE'){
		if (lc($record_name) eq 'thetai'){
			$uc_short_type = 'THI';
		}elsif (lc($record_name) eq 'thetar'){
			$uc_short_type = 'THR';
		}elsif (lc($record_name) eq 'thetap') {
			$uc_short_type = 'THETAP';
		}elsif (lc($record_name) eq 'thetapv') {
			$uc_short_type = 'THETAPV';
		}
		#else stick with $uc_short_type THE, it is THETA
	}elsif( $uc_short_type eq 'OME'){
		if (lc($record_name) eq 'omegap') {
			$uc_short_type = 'OMEGAP';
		}elsif (lc($record_name) eq 'omegapd') {
			$uc_short_type = 'OMEGAPD';
		}
		#else stick with $uc_short_type OME, it is OMEGA
	}elsif( $uc_short_type eq 'SIG'){
		if (lc($record_name) eq 'sigmap') {
			$uc_short_type = 'SIGMAP';
		}elsif (lc($record_name) eq 'sigmapd') {
			$uc_short_type = 'SIGMAPD';
		}
		#else stick with $uc_short_type SIG, it is SIGMA
	}
	#else stick with whatever short type
	return $uc_short_type;
}


use Moose;
use MooseX::Params::Validate;
use model::mirror_plot_module;
use model::cwres_module;
use model::problem::nonparametric;
use model::problem::theta;
use model::problem::thetai;
use model::problem::thetar;
use model::problem::thetap;
use model::problem::thetapv;
use model::problem::sigma;
use model::problem::sigmap;
use model::problem::sigmapd;
use model::problem::omega;
use model::problem::omegap;
use model::problem::omegapd;
use model::problem::tol;
use model::problem::estimation;
use model::problem::pred;
use model::problem::mix;
use model::problem::aesinitial;
use model::problem::prior;
use model::problem::table;
use model::problem::simulation;
use model::problem::bind;
use model::problem::etas;
use model::problem::level;
use model::problem::phis;
use model::problem::anneal;
use model::problem::scatter;
use model::problem::sizes;
use model::problem::msfi;
use model::problem::model;
use model::problem::contr;
use model::problem::abbreviated;
use model::problem::aes;
use model::problem::pk;
use model::problem::des;
use model::problem::error;
use model::problem::covariance;
use model::problem::infn;
use model::problem::subroutine;
use model::problem::data;
use model::problem::input;
use model::problem::problem;
use data;

has 'problems' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::problem]]' );
has 'inputs' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::input]]' );
has 'datas' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::data]]' );
has 'subroutines' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::subroutine]]' );
has 'infns' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::infn]]' );
has 'covariances' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::covariance]]' );
has 'errors' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::error]]' );
has 'dess' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::des]]' );
has 'pks' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::pk]]' );
has 'aess' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::aes]]' );
has 'abbreviateds' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::abbreviated]]' );
has 'contrs' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::contr]]' );
has 'models' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::model]]' );
has 'msfis' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::msfi]]' );
has 'sizess' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::sizes]]' );
has 'scatters' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::scatter]]' );
has 'anneals' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::anneal]]' );
has 'phiss' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::phis]]' );
has 'levels' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::level]]' );
has 'etass' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::etas]]' );
has 'binds' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::bind]]' );
has 'simulations' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::simulation]]' );
has 'tables' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::table]]' );
has 'priors' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::prior]]' );
has 'aesinitials' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::aesinitial]]' );
has 'mixs' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::mix]]' );
has 'preds' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::pred]]' );
has 'estimations' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::estimation]]' );
has 'tols' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::tol]]' );
has 'omegas' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::omega]]' );
has 'omegaps' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::omegap]]' );
has 'omegapds' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::omegapd]]' );
has 'sigmas' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::sigma]]' );
has 'sigmaps' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::sigmap]]' );
has 'sigmapds' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::sigmapd]]' );
has 'thetas' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::theta]]' );
has 'thetais' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::thetai]]' );
has 'thetars' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::thetar]]' );
has 'thetaps' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::thetap]]' );
has 'thetapvs' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::thetapv]]' );
has 'table_files' => ( is => 'rw', isa => 'Maybe[ArrayRef[data]]' );
has 'nonparametrics' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::problem::nonparametric]]' );
has 'cwres_modules' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::cwres_module]]' );
has 'mirror_plot_modules' => ( is => 'rw', isa => 'Maybe[ArrayRef[model::mirror_plot_module]]' );
has 'cwres' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'mirror_plots' => ( is => 'rw', isa => 'Maybe[Int]', default => 0 );
has 'directory' => ( is => 'rw', isa => 'Str' );
has 'ignore_missing_files' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'ignore_missing_output_files' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'prob_arr' => ( is => 'rw', isa => 'Maybe[ArrayRef[Str]]' );
has 'sde' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'nwpri_ntheta' => ( is => 'rw', isa => 'Int' );
has 'nwpri_neta' => ( is => 'rw', isa => 'Int' );
has 'omega_before_pk' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'tbs' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'dtbs' => ( is => 'rw', isa => 'Bool', default => 0 );
has 'tbs_lambda' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'tbs_zeta' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'tbs_delta' => ( is => 'rw', isa => 'Maybe[Str]' );
has 'tbs_thetanum' => ( is => 'rw', isa => 'Int' );
has 'shrinkage_module' => ( is => 'rw', isa => 'model::shrinkage_module' );
has 'eigen_value_code' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'nonparametric_code' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'shrinkage_code' => ( is => 'rw', isa => 'Bool', default => 1 );
has 'iwres_shrinkage_table' => ( is => 'rw', isa => 'Str' );
has 'eta_shrinkage_table' => ( is => 'rw', isa => 'Str' );
has 'own_print_order' => ( is => 'rw', isa => 'Maybe[ArrayRef]' );

sub BUILD
{
	my $self  = shift;

	unless ( defined $self->problems ) {
		# Parse given problem lines.
		$self -> _read_records();

		$self->prob_arr(undef);

		$self->update_prior_information();

		if (defined $self -> estimations() and ($PsN::nm_major_version > 6)){
			my $default_format='s1PE12.5';
			my $reset_file = 0;
			my $reset_nolabel = 0;
			my $reset_notitle = 0;
			my $reset_format = 0;
			my $found_order = 0;
			my @estims = @{$self -> estimations()};
			for (my $i=0; $i <= $#estims; $i++){
				my $est = $estims[$i];
				my $opt_ref = $est -> options();
				if ( defined $opt_ref ) {
					foreach my $option ( @{$opt_ref} ) {
						if (index('ORDER',$option->name())==0){
							$found_order = 1 ;
							next;
						}
						if (index('FILE',$option->name())==0){
							$reset_file = 1 ;
							if ($i == $#estims){
								$option->value('psn.ext');
								$reset_file = 0; #already done
							}
							next;
						}
						if (index('NOTITLE',$option->name())==0){
							$reset_notitle = 1 ;
							if ($i == $#estims){
								$option->value('0');
								$reset_notitle = 0; #already done
							}
							next;
						}
						if (index('NOLABEL',$option->name())==0){
							$reset_nolabel = 1 ;
							if ($i == $#estims){
								$option->value('0');
								$reset_nolabel = 0; #already done
							}
							next;
						}
					} #end loop options
				}
				if ($i == $#estims){
					#now we know that if we need to reset something, that option was not 
					#set in last $EST, only in a previous one. Means we can add option
					$est->_add_option(option_string => 'NOLABEL=0') if ($reset_nolabel);
					$est->_add_option(option_string => 'NOTITLE=0') if ($reset_notitle);
					$est->_add_option(option_string => 'FILE=psn.ext') if ($reset_file);
					$est->_add_option(option_string => 'FORMAT='.$default_format) if ($reset_format);
				}
			}
			if ($found_order){
				$self -> remove_option( record_name => 'estimation',
						record_number => 0,
						option_name => 'ORDER',
						fuzzy_match => 1 );
				print "\n***Warning***\n".
					"Option ORDER in \$EST is not yet supported by PsN. It has been removed from ".
					"the control stream.\n";
			}
		}
	}
	# Initialize table file objects (if any)
# Kajsa 2014-04-25 skip this, assume a big waste
#	$self -> _read_table_files( ignore_missing_files => $self->ignore_missing_output_files );

	if ( $self->cwres ) {
		$self->cwres_modules([]) unless defined $self->cwres_modules;
		push( @{$self->cwres_modules}, model::cwres_module->new( problem => $self,
																		  mirror_plots => $self->mirror_plots) );
	}

	if( $self->tbs or $self->dtbs){
		if (defined $self->nwpri_ntheta()){
			ui -> print( category => 'all',
					message => "Warning: \$PRIOR NWPRI is not supported in combination with -tbs or -dtbs.",
					newline => 1);
		}

		$self -> tbs_transform();
	}
}


sub add_nonparametric
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->nonparametrics([]) unless defined $self->nonparametrics;
	push( @{$self->nonparametrics}, model::problem::nonparametric->new( %{$parm{'init_data'}} ) );
}

sub add_theta
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->thetas([]) unless defined $self->thetas;
	push( @{$self->thetas}, model::problem::theta->new( %{$parm{'init_data'}} ) );
}

sub add_sigma
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->sigmas([]) unless defined $self->sigmas;
	push( @{$self->sigmas}, model::problem::sigma->new( %{$parm{'init_data'}} ) );
}

sub add_omega
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->omegas([]) unless defined $self->omegas;
	push( @{$self->omegas}, model::problem::omega->new( %{$parm{'init_data'}} ) );
}

sub add_tol
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->tols([]) unless defined $self->tols;
	push( @{$self->tols}, model::problem::tol->new( %{$parm{'init_data'}} ) );
}

sub add_estimation
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->estimations([]) unless defined $self->estimations;
	push( @{$self->estimations}, model::problem::estimation->new( %{$parm{'init_data'}} ) );
}

sub add_pred
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->preds([]) unless defined $self->preds;
	push( @{$self->preds}, model::problem::pred->new( %{$parm{'init_data'}} ) );
}

sub add_mix
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->mixs([]) unless defined $self->mixs;
	push( @{$self->mixs}, model::problem::mix->new( %{$parm{'init_data'}} ) );
}

sub add_aesinitial
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->aesinitials([]) unless defined $self->aesinitials;
	push( @{$self->aesinitials}, model::problem::aesinitial->new( %{$parm{'init_data'}} ) );
}

sub add_prior
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->priors([]) unless defined $self->priors;
	push( @{$self->priors}, model::problem::prior->new( %{$parm{'init_data'}} ) );
}

sub add_table
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->tables([]) unless defined $self->tables;
	push( @{$self->tables}, model::problem::table -> new ( %{$parm{'init_data'}} ) );
}

sub add_simulation
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->simulations([]) unless defined $self->simulations;
	push( @{$self->simulations}, model::problem::simulation -> new ( %{$parm{'init_data'}} ) );
}

sub add_bind
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->binds([]) unless defined $self->binds;
	push( @{$self->binds}, model::problem::bind->new( %{$parm{'init_data'}} ) );
}

sub add_etas
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->etass([]) unless defined $self->etass;
	push( @{$self->etass}, model::problem::etas -> new ( %{$parm{'init_data'}} ) );
}

sub add_level
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->levels([]) unless defined $self->levels;
	push( @{$self->levels}, model::problem::level -> new ( %{$parm{'init_data'}} ) );
}

sub add_phis
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->phiss([]) unless defined $self->phiss;
	push( @{$self->phiss}, model::problem::phis -> new ( %{$parm{'init_data'}} ) );
}

sub add_anneal
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->anneals([]) unless defined $self->anneals;
	push( @{$self->anneals}, model::problem::anneal -> new ( %{$parm{'init_data'}} ) );
}

sub add_scatter
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->scatters([]) unless defined $self->scatters;
	push( @{$self->scatters}, model::problem::scatter -> new ( %{$parm{'init_data'}} ) );
}

sub add_sizes
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->sizess([]) unless defined $self->sizess;
	push( @{$self->sizess}, model::problem::sizes -> new ( %{$parm{'init_data'}} ) );
}

sub add_msfi
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->msfis([]) unless defined $self->msfis;
	push( @{$self->msfis}, model::problem::msfi -> new ( %{$parm{'init_data'}} ) );
}

sub add_model
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->models([]) unless defined $self->models;
	push( @{$self->models}, model::problem::model -> new ( %{$parm{'init_data'}} ) );
}

sub add_contr
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->contrs([]) unless defined $self->contrs;
	push( @{$self->contrs}, model::problem::contr -> new ( %{$parm{'init_data'}} ) );
}

sub add_abbreviated
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->abbreviateds([]) unless defined $self->abbreviateds;
	push( @{$self->abbreviateds}, model::problem::abbreviated -> new ( %{$parm{'init_data'}} ) );
}

sub add_aes
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->aess([]) unless defined $self->aess;
	push( @{$self->aess}, model::problem::aes -> new ( %{$parm{'init_data'}} ) );
}

sub add_pk
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->pks([]) unless defined $self->pks;
	push( @{$self->pks}, model::problem::pk -> new ( %{$parm{'init_data'}} ) );
}

sub add_des
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->dess([]) unless defined $self->dess;
	push( @{$self->dess}, model::problem::des -> new ( %{$parm{'init_data'}} ) );
}

sub add_error
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->errors([]) unless defined $self->errors;
	push( @{$self->errors}, model::problem::error -> new ( %{$parm{'init_data'}} ) );
}

sub add_covariance
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->covariances([]) unless defined $self->covariances;
	push( @{$self->covariances}, model::problem::covariance -> new ( %{$parm{'init_data'}} ) );
}

sub add_infn
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->infns([]) unless defined $self->infns;
	push( @{$self->infns}, model::problem::infn -> new ( %{$parm{'init_data'}} ) );
}

sub add_subroutine
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->subroutines([]) unless defined $self->subroutines;
	push( @{$self->subroutines}, model::problem::subroutine -> new ( %{$parm{'init_data'}} ) );
}

sub add_data
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->datas([]) unless defined $self->datas;
	push( @{$self->datas}, model::problem::data -> new ( %{$parm{'init_data'}} ) );
}

sub add_input
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->inputs([]) unless defined $self->inputs;
	push( @{$self->inputs}, model::problem::input -> new ( %{$parm{'init_data'}} ) );
}

sub add_problem
{
	my ($self, %parm) = validated_hash(@_, 
		init_data => {isa => 'Any', optional => 0}
	);
	$self->problems([]) unless defined $self->problems;
	push( @{$self->problems}, model::problem::problem -> new ( %{$parm{'init_data'}} ) );
}

sub add_prior_distribution
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		from_output => { isa => 'output', optional => 1 },
		problem_number => { isa => 'Int', default => 0, optional => 1 },
		df_string => { isa => 'Str', optional => 0 }
	);
	my $from_output = $parm{'from_output'};
	my $problem_number = $parm{'problem_number'};
	my $df_string = $parm{'df_string'};

	#First check that do not already have prior
	if ((defined $self->priors()) and scalar(@{$self -> priors()})>0 ){
		print "Warning: Cannot add prior to \$PROB which already has a \$PRIOR\n";
		return;
	}
	#Set ntheta=number of THETAs in input model.
	#Set neta=number of diagonal OMEGAs in input model.
	#Set neps=0
	#Set nthp=ntheta
	#Set netp=neta
	my $ntheta = $self->record_count(record_name=> 'theta');
	my $nthp = $ntheta;
	my $neta = $self->nomegas('with_same'=>1, 'with_correlations' => 0);
	my $netp=$neta;
	my $neps=0;
	#Add record to simulation model
	#$PRIOR NWPRI NTHETA=ntheta,NETA=neta,NEPS=neps,NTHP=nthp,NETP=netp
	#Need to set NPEXP?? number of prior experiments
	my $plev='';
	$plev= 'PLEV=0.99' 
	if ((defined $self->simulations()) and scalar(@{$self -> simulations()})>0 );
	my $record_string=" NWPRI NTHETA=$ntheta NETA=$neta NTHP=$nthp NETP=$netp $plev";
	$self -> set_records( 'type' => 'prior',
		'record_strings' => [$record_string] );

	#update own_print_order
	if (defined $self->own_print_order and scalar(@{$self->own_print_order})>0){
		my %leading_order = {'sizes' => 1,'problem'=> 1,'input'=> 1,'bind'=> 1,'data'=> 1,'abbreviated'=> 1,'msfi'=> 1,'contr'=> 1,'subroutine'=> 1};
		my @new_order =();
		my $added=0;
		foreach my $rec (@{$self->own_print_order}){
			if (not $added and (not $leading_order{$rec}==1)){
				push (@new_order,'prior');
				$added = 1;
			}
			push (@new_order,$rec);
		}
		$self->own_print_order(\@new_order);
	}else{
		print "\nError add_prior_distribution, no own_print_order\n";
	}


	#Add ntheta new $THETA FIX. Initial estimates are final THETA estimates from lst-file.
	my $ref = $from_output->get_single_value(attribute=>'thetacoordval',
		problem_index => ($problem_number-1),
		subproblem_index => 0);
	unless (defined $ref){
		print "Cannot add prior if output data lacks THETA estimates\n";
		return;
	}
	my %thetas = %{$ref};
	for (my $i=1;$i<=$nthp;$i++){
		my $val=$thetas{'THETA'.$i};
		$self -> add_records(type => 'theta',
			record_strings => ["$val FIX"]);
	}

	$ref = $from_output->get_single_value(attribute=>'covariance_matrix',
		problem_index=>($problem_number-1),
		subproblem_index=>0);
	unless (defined $ref){
		print "Cannot add prior if output data lacks covariance matrix\n";
		return;
	}

	#Add diagonal $OMEGA FIX where initial estimates is the leading ntheta block 
	#from the variance-covariance matrix in .cov
	my @record_strings = ();
	my $index=0;
	for (my $i=0;$i<$nthp;$i++){
		my @arr;
		for (my $j=0;$j<=$i;$j++){
			if ($j == $i){
				my $init = sprintf("%.15G",$ref->[$index]);#must format with E here
				push(@arr,$init,'FIX');
			}
			$index++;
		}
		push(@record_strings,join(' ',@arr));
	}
	my @omega_variance;
	for (my $i=1+$nthp;$i<=($netp+$nthp);$i++){
		my $index=-1+$i*$i/2;
		push(@omega_variance,$ref->[$index]);
	}
	$self -> add_records(type => 'omega',
		record_strings => \@record_strings);

	#Add $OMEGA FIX where size is neta and initial estimates are final $OMEGA estimate 
	#from lst. Form must match original $OMEGA form in lst.
	$ref = $from_output->get_single_value(attribute=>'omegacoordval',
		problem_index => ($problem_number-1),
		subproblem_index => 0);

	unless (defined $ref){
		print "Cannot add prior if output data lacks OMEGA estimates\n";
		return;
	}
	my %omegas = %{$ref};

	#loop over this models omega records
	#create copy of record
	#use coordinate strings to replace inits with values from output

	my $set_prior_etas=0;
	my @all_formatted;
	my $size=0;
	foreach my $record (@{$self->omegas()}){
		last if ($set_prior_etas >= $neta);
		my @record_strings;
		my $block = 0;
		if ($record->type() eq 'BLOCK'){
			$block = 1;
			if ($record->same()){
				@record_strings = ('BLOCK SAME');
				$self->add_records(type=> 'omega',
					record_strings => \@record_strings);
				$set_prior_etas += $size;
				next;
			}else{
				$size = $record->size();
				@record_strings = ("BLOCK($size) FIX");
			}
		}else{
			$size = 0;
		}
		foreach my $opt (@{$record->options()}){
			my $label = $opt->coordinate_string();
			my $init = sprintf("%.15G",$omegas{$label});#must format with E here
			if ($block){
				push(@record_strings,"$init");
			}else{
				$size++;
				push(@record_strings,"$init FIX");
			}
		}
		$set_prior_etas += $size;
		print "Error too many new omegas\n" if ($set_prior_etas > $neta);
		$self->add_records(type=> 'omega',
			record_strings => \@record_strings);
	}

	my @dflist = split(/,/,$df_string);
	foreach my $df (@dflist){
		$self -> add_records(type => 'theta',
			record_strings => ["$df FIX"]);
	}

	#immediately after adding $PRIOR etc must run update_prior_information on problem
	$self->update_prior_information();
}

sub store_inits
{
	my $self = shift;

	if ( defined $self->thetas ) {
	  foreach my $theta ( @{$self->thetas} ) {
	    $theta -> store_inits;
	  }
	}
	if ( defined $self->omegas ) {
	  foreach my $omega ( @{$self->omegas} ) {
	    $omega -> store_inits;
	  }
	}
	if ( defined $self->sigmas ) {
	  foreach my $sigma ( @{$self->sigmas} ) {
	    $sigma -> store_inits;
	  }
	}
}

sub restore_inits
{
	my $self = shift;

	if ( defined $self->thetas ) {
	  foreach my $theta ( @{$self->thetas} ) {
	    $theta -> restore_inits;
	  }
	}
	if ( defined $self->omegas ) {
	  foreach my $omega ( @{$self->omegas} ) {
	    $omega -> restore_inits;
	  }
	}
	if ( defined $self->sigmas ) {
	  foreach my $sigma ( @{$self->sigmas} ) {
	    $sigma -> restore_inits;
	  }
	}
}

sub set_random_inits
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 degree => { isa => 'Num', default => 0.1, optional => 1 },
		 basic_model => { isa => 'model', optional => 1 },
		 problem_index => { isa => 'Int', default => 0, optional => 1 }
	);
	my $degree = $parm{'degree'};
	my $basic_model = $parm{'basic_model'};
	my $problem_index = $parm{'problem_index'};

	if (($degree >= 1) or ($degree <= 0)){
		croak("Illegal input to set_random_inits, degree $degree is not between 0 and 1");
	}

	my $bound_problem;
	if (defined $basic_model){
		if (defined $basic_model->problems and defined $basic_model->problems->[$problem_index]){
			$bound_problem = $basic_model->problems->[$problem_index];
		}else{
			croak("bug in problem->set_random_inits: basic_model does not match update model");
		}
	}

	foreach my $param ('theta','omega','sigma'){
		my $accessor=$param.'s';
		my $nrec = 0;
		if (defined $self -> $accessor) {
			$nrec = scalar(@{$self -> $accessor});
		}

		next unless ( $nrec > 0); #no parameter in this problem
		if (defined $bound_problem){
			unless (defined $bound_problem->$accessor and scalar(@{$bound_problem->$accessor})==$nrec){
				croak("bug in problem->set_random_inits: basic_model does not match update model $param");
			}
		}
		for (my $i=0; $i< $nrec; $i++){
			my $record = $self->$accessor->[$i];
			if  ($record->same() or $record->fix() or $record->prior()) {
				next;
			}
			unless (defined $record -> options()) {
				croak("$param record has no values in set_random_inits");
			}

			if (defined $bound_problem){
				$record -> set_random_inits(degree => $degree,
											bound_record => $bound_problem->$accessor->[$i]);
			}else{
				$record -> set_random_inits(degree => $degree);
			}
		}
	}
}


sub record_count
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 record_name => { isa => 'Str', optional => 1 }
	);
	my $record_name = $parm{'record_name'};
	my $return_value = 0;

	$return_value = 0;
	my $accessor = $record_name . 's';
	if( defined $self->$accessor ){
	    foreach my $record ( @{$self->$accessor} ){
		if( defined $record -> options ){
		    $return_value += @{$record -> options};
		}
	    }
	}

	return $return_value;
}

sub _init_attr
{
	my $self = shift;
	my %test = @_;
	my %parm = validated_hash(\@_,
		 parameter_type => { isa => 'Str', optional => 1 },
		 get_same => { isa => 'Bool', default => 0, optional => 1 },
		 with_priors => { isa => 'Bool', default => 0, optional => 1 },
		 parameter_numbers => { isa => 'Maybe[ArrayRef[Int]]', optional => 1 },
		 attribute => { isa => 'Str', optional => 1 },
		 new_values => { isa => 'ArrayRef[Maybe[Str]]', optional => 1, default => [] },
		 add_if_absent => { isa => 'Bool', default => 0, optional => 1 },
		MX_PARAMS_VALIDATE_NO_CACHE => 1,
	);
	my $parameter_type = $parm{'parameter_type'};
	my $get_same = $parm{'get_same'};
	my $with_priors = $parm{'with_priors'};
	my @parameter_numbers = defined $parm{'parameter_numbers'} ? @{$parm{'parameter_numbers'}} : ();
	my $attribute = $parm{'attribute'};
	my @new_values = @{$parm{'new_values'}};
	my $add_if_absent = $parm{'add_if_absent'};
	my @parameter_values;

	# Private method, should preferably not be used outside model.pm
	# The add_if_absent argument tells the method to add an init (theta,omega,sigma)
	# if the parameter number points to a non-existing parameter with parameter number
	# one higher than the highest presently included. Only applicatble if
	# new_values are set. Default value = 0;

	my $accessor = $parameter_type.'s';
	unless( $self -> can($accessor) ){
	    croak("problem -> _init_attr: Error unknown parameter type: $parameter_type" );
	}

	my @records;
	if( defined $self->$accessor ){
		@records = @{$self->$accessor};
	} else {
		@records = ();
	}
	
	my @options = ();

	# {{{ Check that the size of parameter_numbers and new_values match
	my %num_val;
	if ( $#parameter_numbers >= 0 and $#new_values >= 0 ) {
		if ( $#parameter_numbers == $#new_values ) {
			for ( my $i = 0; $i <= $#new_values; $i++ ) {
				$num_val{$parameter_numbers[$i]} = $new_values[$i];
			}
		} else {
			die "Model::problem -> _init_attr: The number of specified ".
				"parameters (@parameter_numbers) and values (@new_values) do not match for parameter $parameter_type".
				" and attribute $attribute\n";
		}
	}
	# }}}

	my $prev_size = 1;
	if ( scalar @new_values > 0 ) {
		# {{{ Update values

		# OBS! We are using 'normal' numbering in parameter_numbers, i.e. they begin
		# at one (1).
		my $opt_num = 1;
		# Ugly solution to add non-existing options:
		my %found;
		foreach my $num ( @parameter_numbers) {
			$found{$num} = 0;
		}

		my @diagnostics = ();
		foreach my $record ( @records ) {
			if ( $record -> same() ) {
				# SAME == true: Nothing to be done. Just move forward to next $OMEGA but
				# increase counter first

				$opt_num += $prev_size;
			} else {
				foreach my $option ( @{$record -> options} ) {
					if ( scalar @parameter_numbers > 0 ) {
						foreach my $num ( @parameter_numbers ) {
							if ( $num == $opt_num ) {
								$found{$num}++;
								if ( $attribute eq 'init' ) {
									push( @diagnostics,
										  $option -> check_and_set_init( new_value => $num_val{$num} ) );
								} elsif( $attribute eq 'fix' and defined $record -> size() 
										 and ($record-> type() eq 'BLOCK') ){
									# size() tells us this MIGHT be a block and we must fix on record level.
									#check type also
									$record -> fix( $num_val{$num} );
								} else {
									$option -> $attribute( $num_val{$num} );
								}
							}
						}
					} else {
						if ( $attribute eq 'init' ) {
							push( @diagnostics,
								  $option -> check_and_set_init( new_value => shift( @new_values ) ) );
						} elsif( $attribute eq 'fix' and defined $record -> size()
								 and ($record-> type() eq 'BLOCK')){
							# size() tells us this MIGHT a block and we must fix on record level.Check type also
							$record -> fix( shift( @new_values ) );
						} else {
							$option -> $attribute( shift( @new_values ) );
						}
					}
					$opt_num++;
				}
				if( $parameter_type eq 'theta' ){
					$prev_size = scalar @{$record -> options};
				} else {
					my $size = $record -> size;
					if( (defined $size) and ($record->type eq 'BLOCK') ) {
						$prev_size = ($size*($size+1))/2;
					} else {
						$prev_size = scalar @{$record -> options};
					}
				}
			}
		}
		# If $add_if_absent is set, any parameters that were not found above are
		# added below:
		
		my @nums = sort {$a<=>$b} keys %found;
		my $new_record = "model::problem::$parameter_type" -> new();
		my $do_add_record;
		my $added_thetas=1;
		my $added_sigmas=1;
		my $added_omegas=1;
		foreach my $num ( @nums ) {
			if ( $add_if_absent and
				 not $found{$num} ) {
				$do_add_record = 1;
				unless($num == $opt_num) {
					croak("Attempt to add a parameter with higher number ($num) than the number\n".
						  "of parameters + 1 ($opt_num)\n" );
				}
				# Get the last record of $parameter_type
				# my $new_record = $records[$#records];
				my $option_class;

				my $coordinate_string;
				if( $parameter_type eq 'theta' ){
					$option_class = 'model::problem::record::theta_option';
					my $index = $self->record_count('record_name' => 'theta')+$added_thetas;
					$coordinate_string='THETA'.$index;
					$added_thetas++;
				} else {
					$option_class = 'model::problem::record::init_option';
					if( $parameter_type eq 'omega' ){
						my $index = $self->nomegas('with_correlations' => 0,'with_same' => 1)+$added_omegas;
						$coordinate_string='OMEGA('.$index.','.$index.')';
						$added_omegas++;
					}else {
						my $index = $self->nsigmas('with_correlations' => 0,'with_same' => 1)+$added_sigmas;
						$coordinate_string='SIGMA('.$index.','.$index.')';
						$added_sigmas++;
					}
				}

				# Push a new option to this last record
				my $option = $option_class -> new(coordinate_string => $coordinate_string);
				if ( $attribute eq 'init' ) {
					$option -> check_and_set_init( new_value => $num_val{$num} );
				} elsif( $attribute eq 'fix' and defined $new_record -> size()
						 and ($new_record-> type() eq 'BLOCK')){

					# size() tells us this is MIGHT be a block and we must fix on
					# record level. This will never happen, as we can't
					# add BLOCKS, at least not like this.

					$new_record -> fix( $num_val{$num} );
				} else {
					$option -> $attribute( $num_val{$num} );
				}
				$new_record->options([]) unless (defined $new_record->options());
				push( @{$new_record->options}, $option );

				# So we've added a parameter. Possible to add more,
				# lets increase the highest found:
				$opt_num++;
			}
		}
		if ( $attribute eq 'init' ) {
			# We're updating but might be returning diagnostics
			# Use the default return parameter parameter_values for this
			@parameter_values = @diagnostics;
		}

		if( $do_add_record ){
			push( @records, $new_record );
			$self->$accessor(\@records);
		}

		# }}} Update values
	} else {
		# {{{ Retrieve values

		my @prev_values = ();
		my $done=0;
		foreach my $record ( @records ) {
			last if ($done);
			last if ($record->prior() and (not $with_priors));
			unless ( $record -> same() ) {
				@prev_values = ();
				if ( defined $record -> options ) {
					foreach my $option ( @{$record -> options} ) {
						if ($option->prior() and (not $with_priors)){
							$done=1;
							last;
						}
						push( @prev_values, $option -> $attribute );
					}
				} else {
					carp("Trying to get attribute $attribute, ".
						 "but no options defined in record ".ref($record) );
				}
				$prev_size = $record -> size unless ( $record -> same );
			}
			if( $record -> same() and (not $get_same)) {
				for( my $i = 0; $i <= $#prev_values; $i++ ) {
					$prev_values[$i] = undef;
				}
			}
			push( @parameter_values, @prev_values );
		}
		
		if ( scalar @parameter_numbers > 0 ) {
			my @part_vals = ();
			foreach my $num ( @parameter_numbers ) {
				push( @part_vals, $parameter_values[$num -1] );
			}
			@parameter_values = @part_vals;
		} else {
			carp("Model::problem -> _init_attr: parameter_numbers undefined, using all." );
		}
		
		# }}} Retrieve values
	}

	return \@parameter_values;
}

sub indexes
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		parameter_type => { isa => 'Str', optional => 1 },
		parameter_numbers => { isa => 'Maybe[ArrayRef[Num]]', optional => 1 },
		with_priors => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $parameter_type = $parm{'parameter_type'};
	my $parameter_numbers = $parm{'parameter_numbers'}; 
	my $with_priors = $parm{'with_priors'};
	my @indexes;

	# The Indexes method returns the coordinate_string for all init_options
	# THETA1, THETA2 or OMEGA(1,1) etc or SIGMA(1,1) SIGMA(2,1)etc
	# indexes are also returned if BLOCK SAME 

	my $row = 1;
	my $accessor = $parameter_type . 's';
	my $previous_size = 0;
	my @previous;
	my $done = 0;

	if( defined $self->$accessor ){

		foreach my $record ( @{$self -> $accessor} ) {
			last if ($done);
			last if ($record->prior() and (not $with_priors));
			if( $record -> same() ) {
				if( $previous_size == 0 ){
					croak("You can't have an $parameter_type ".
						"estimate defined as SAME if there is no previous estimate" );
				}
				#add $previous_size to all @previous rows and cols
				my @these=();
				foreach my $coord ( @previous ) {
					if ($coord =~ /THETA(\d+)/){
						push( @these, 'THETA'.($1+$previous_size)); 

					}elsif ($coord =~ /(OMEGA|SIGMA)\((\d+)\,(\d+)\)/ ){
						push( @these, $1.'('.($2+$previous_size).','.($3+$previous_size).')');
					} else {
						croak("Unknown coordinate string $coord");
					}
				}
				push( @indexes,@these);
				@previous = @these;
			} elsif ( defined $record -> options() ) {
				if (defined $record ->size()){
					$previous_size = $record ->size();
				} else {
					#if no size and not same then must be diagonal
					$previous_size = scalar(@{$record -> options()});
				}
				@previous = ();
				foreach my $option ( @{$record -> options()} ) {
					if ($option->prior() and (not $with_priors)){
						$done = 1;
						last;
					}
					push( @indexes, $option->coordinate_string() );
					push( @previous, $option->coordinate_string() );
				}
			}
		}
	}

	if ( defined $parameter_numbers ) {
		my @parameter_numbers = @{$parameter_numbers};
		my @part_indexes = ();
		foreach my $num ( @parameter_numbers ) {
			if ( $num < 1 or $num > scalar @indexes ) {
				croak("$parameter_type number " . $num . " does not exist in this model::problem\n" .
					"(" . scalar @indexes . " exists)\n" );
			}
			push( @part_indexes, $indexes[$num -1] );
		}
		@indexes = @part_indexes;
	} else {
		carp("Model::problem -> indexes: parameter_numbers undefined, using all." );
	}

	return \@indexes;
}

sub covariance
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		enabled => { isa => 'Bool', optional => 1 }
	);
	my $enabled = $parm{'enabled'};
	my $indicator = 0;

	my @records;
	if( defined $self->covariances ) {
		@records = @{$self->covariances} ;
	}
	if ( defined $enabled ) {
		if ( $enabled and $#records < 0 ) {
			$self -> add_records( type           => 'covariance',
				record_strings => [''] );
		} elsif ( not $enabled and $#records >= 0 ) {
			$self->covariances(undef);
		}
	} else {
		if ( $#records >= 0 ) {
			$indicator = 1;
		} else {
			$indicator = 0;
		}
	}

	return $indicator;
}

sub eigen
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		enabled => { isa => 'Str', optional => 1 }
	);
	my $enabled = $parm{'enabled'};
	my $indicator = 0;

	my ( $print_ref, $position ) = $self -> _option_val_pos( record_name => 'covariance',
		name => 'PRINT' );

	return $indicator;
}

sub _option_val_pos
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		record_name => { isa => 'Str', optional => 1 },
		instance_numbers => { isa => 'ArrayRef[Int]', optional => 1, default => [] },
		name => { isa => 'Str', optional => 1 },
		new_values => { isa => 'ArrayRef[Str]', optional => 1, default => [] },
		exact_match => { isa => 'Bool', default => 1, optional => 1 },
		MX_PARAMS_VALIDATE_NO_CACHE => 1,
	);
	my $record_name = $parm{'record_name'};
	my @instance_numbers = @{$parm{'instance_numbers'}};
	my $name = $parm{'name'};
	my @new_values = @{$parm{'new_values'}};
	my $exact_match = $parm{'exact_match'};
	my @values;
	my @positions;

	#
	# _option_val_pos( record_name => 'subroutine',
	#                  name => 'OTHER',
	#                  val => 'get_cov')
	#

	# _option_val_pos sets, or gets, the value of an option (given
	# as the 'name' parameter. Name must be uppercase) in a record
	# (given as the 'record_name' parameter. Record name should be
	# the record class name in the model diagram.)

	my $accessor = $record_name.'s';
	unless( $self -> can($accessor) ){
		croak("Unknown record name: $record_name" );
	}

	my @records;
	if( defined $self->$accessor ) {
		@records = @{$self->$accessor} ;
	} else {
		carp("No records of type $accessor" );
		@records = ();
	}
	my @options = ();

	# {{{ Check that the size of instance_numbers and new_values match

	my %num_val;
	if ( $#instance_numbers >= 0 and $#new_values >= 0 ) {
		if ( $#instance_numbers == $#new_values ) {
			for ( my $i = 0; $i <= $#new_values; $i++ ) {
				$num_val{$instance_numbers[$i]} = $new_values[$i];
			}
		} else {
			croak("Model::problem -> _option_val_pos: The number of specified " .
				"parameters " . $#instance_numbers+1 . " and values " .
				$#new_values+1 . " do not match" );
		}
	}

	# }}}

	if ( scalar @new_values > 0 ) {
		# {{{ Update values

		my $opt_num = 1;
		foreach my $record ( @records ) {
			foreach my $option ( @{$record -> options} ) {
				my $test_name = $exact_match ? uc($option -> name) :
				uc(substr($option -> name,0,length($name)));
				if ( $test_name eq $name) {
					if ( scalar @instance_numbers > 0 ) {
						foreach my $num ( @instance_numbers ) {
							$option -> value( $num_val{$num} ) if $num == $opt_num;
						}
					} else {
						$option -> value( shift( @new_values ) );
					}
					$opt_num++;
				}
			}
		}

		# }}} Update values
	} else {
		# {{{ Retrieve values

		foreach my $record ( @records ) {
			my $i = 1;
			if ( defined $record -> options ) {
				foreach my $option ( @{$record -> options} ) {
					my $test_name = $exact_match ? uc($option -> name) :
					uc(substr($option -> name,0,length($name)));
					if ( $test_name eq $name) {
						push( @values, $option -> value );
						push( @positions, $i );
					}
					$i++;
				}
			}
		}
		if ( $#instance_numbers > 0 ) {
			my @part_vals = ();
			my @part_pos = ();
			foreach my $num ( @instance_numbers ) {
				push( @part_vals, $values[$num -1] );
				push( @part_pos, $positions[$num -1] );
			}
			@values = @part_vals;
			@positions = @part_pos;
		} 

		# }}} Retrieve values
	}

	return \@values ,\@positions;
}

sub remove_records
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 type => { isa => 'Str', optional => 0 },
		 keep_last => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $type = $parm{'type'};
	my $keep_last = $parm{'keep_last'};

	my $rec_class = "model::problem::$type";
	my $accessor = $type.'s';
	if( $self -> can($accessor) ){
		if ($keep_last){
			my @recs;
			my $last_rec = undef;
			@recs = @{$self -> $accessor} if (defined $self -> $accessor); 
			$last_rec = $recs[-1] if (defined $recs[-1]);
			$self -> $accessor([$last_rec]);
		}else {
			$self->$accessor(undef);
		}
	} else {
		die "Error in problem -> remove_records: Trying to remove unknown record: $type\n";
	}
}

sub _read_table_files
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		ignore_missing_files => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $ignore_missing_files = $parm{'ignore_missing_files'};

	$self->table_files([]);
	my ( $table_name_ref, $junk ) = $self -> _option_val_pos( record_name => 'table', name => 'FILE' );
	if ( defined $table_name_ref and scalar @{$table_name_ref} >= 0 ) {
		$self->table_files([]);
		foreach my $table_name ( @{$table_name_ref} ) {
			carp("Creating new table_file object from $table_name" );
			my $new_table = data -> new( directory            => $self->directory,
										 filename             => $table_name,
										 ignoresign => '@',
										 ignore_missing_files => $ignore_missing_files,
										 parse_header           => 1 );
			push( @{$self->table_files}, $new_table );
		}
	}
}

sub header
{
	my $self = shift;
	my @header;

	my $inp_ref = $self -> inputs;
	if ( defined $inp_ref and defined $inp_ref -> [0] ) {
		my $input = $inp_ref -> [0];
		my $opt_ref = $input -> options;
		if ( defined $opt_ref ) {
			my @options = @{$opt_ref};
			foreach my $option ( @options ) {
				push ( @header, [$option -> name, $option -> value] );
			}
		}
	}

	return \@header;
}



sub remove_option
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		record_name => { isa => 'Str', optional => 1 },
		record_number => { isa => 'Int', default => 0, optional => 1 },
		option_name => { isa => 'Str', optional => 1 },
		fuzzy_match => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $record_name = $parm{'record_name'};
	my $record_number = $parm{'record_number'};
	my $option_name = $parm{'option_name'};
	my $fuzzy_match = $parm{'fuzzy_match'};


	my $accessor = $record_name.'s';
	unless( $self -> can($accessor) ){
		croak("Unknown record name: $record_name" );
	}
	if( defined $self->$accessor ) {
		my @records = @{$self->$accessor};
		my @record_numbers;
		if ($record_number > 0){
			push(@record_numbers,$record_number);
		}elsif ($record_number == 0){
			@record_numbers = 1 .. scalar(@records);
		}elsif ($record_number == -1){
			#last
			push(@record_numbers,scalar(@records));
		}else {
			croak("illegal input record_number $record_number to remove_option");
		}

		foreach my $recnum ( @record_numbers ) {
			#numbering starts at 1, unlike array indices
			$records[$recnum-1] -> remove_option( name => $option_name,
				fuzzy_match => $fuzzy_match );
		}
	} else {
		carp("No records of type $accessor" );
	}
}

sub add_option
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 record_number => { isa => 'Int', default => 0, optional => 1 },
		 record_name => { isa => 'Str', optional => 1 },
		 option_name => { isa => 'Str', optional => 1 },
		 option_value => { isa => 'Maybe[Str]', optional => 1 },
		 add_record => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $record_number = $parm{'record_number'};
	my $record_name = $parm{'record_name'};
	my $option_name = $parm{'option_name'};
	my $option_value = $parm{'option_value'};
	my $add_record = $parm{'add_record'};

	my $accessor = $record_name.'s';
	unless( $self -> can($accessor) ){
		croak("Unknown record name: $record_name" );
	}
	if( defined $self->$accessor ) {
		my @records = @{$self->$accessor};
		my @record_numbers;

		if ($record_number > 0){
			push(@record_numbers,$record_number);
		} elsif ($record_number == 0) {
			@record_numbers = 1 .. scalar(@records);
		} elsif ($record_number == -1) {
			push(@record_numbers,scalar(@records));
		} else {
			croak("illegal input record_number $record_number to add_option");
		}

		foreach my $recnum ( @record_numbers ) {
    #numbering starts at 1, unlike array indices
    $records[$recnum-1]->add_option( init_data => { name  => $option_name, value => $option_value } );
		}
	} else {
		if( $add_record ) {
			$self -> add_records( type => $record_name,
					record_strings => ["$option_name=$option_value"] );
		} else {
			carp("No records of type $accessor and add_option set not to add one" );
		}
	}
}

sub add_marginals_code
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 nomegas => { isa => 'Int', optional => 1 }
	);
	my $nomegas = $parm{'nomegas'};


# add_marginals_code takes one argument.
#
# - nomegas which is the number of (diagonal-element)
# omegas.
#
# For each omega, verbatim code is added to make the marginals
# available for printing (e.g. to a table file). COM(1) will hold the
# nonparametric density, COM(2) the marginal cumulative value for the
# first eta, COM(2) the marginal cumulative density for the second eta
# and so on.
# The code is added to the $ERROR record

	my $record_ref = $self -> errors;
	if( defined $record_ref and defined $record_ref -> [0] ) {
		my ( @first_params, @last_params );
		$last_params[0] = '"   COM(1) = DENM';
		$first_params[0] = '"     X ';
		my $j = 0;
		my $comma;
		for( my $i = 1; $i <= $nomegas; $i++ ) {
			$comma = $i == $nomegas ? '' : ',';
			if( not ($i % 4) ) { # break line every fifth omega
				$j++;
				$first_params[$j] = '"     X ';
			}
			$first_params[$j] = $first_params[$j]."DEN$i$comma";
			push( @last_params, '"   COM('.($i+1).") = DEN$i" );
		}
		my $first_code = $record_ref -> [0] -> verbatim_first;
		push( @{$first_code}, ( '"  COMMON /ROCM18/ DENM,', @first_params,
					'"  DOUBLE PRECISION DENM,', @first_params ) );
		$record_ref -> [0] -> verbatim_first( $first_code );
		my $last_code = $record_ref -> [0] -> verbatim_last;
		push( @{$last_code}, @last_params );
		$record_ref -> [0] -> verbatim_last( $last_code );
		last; # Only insert the code in the first record found (of the ones specified above)
	} else {
		carp("No \$ERROR record was found. Can't add verbatim code".
				" to access nonparametric marginals" );
	}
}

sub nomegas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 with_priors => { isa => 'Bool', default => 0, optional => 1 },
		 with_correlations => { isa => 'Bool', default => 0, optional => 1 },
		 with_same => { isa => 'Bool', default => 1, optional => 1 }
	);
	my $nomegas;
	my $with_priors = $parm{'with_priors'};
	my $with_correlations = $parm{'with_correlations'};
	my $with_same = $parm{'with_same'};

	my $prev = undef;
	$nomegas=0;
	$self->omegas([]) unless defined $self->omegas;
	foreach my $omega ( @{$self->omegas} ) {
		last if ($omega->prior() and (not $with_priors));

		my $size = $omega -> size;
		my $type = $omega -> type;
		if ($omega->same()){
			croak("First \$OMEGA cannot be SAME")
				unless (defined $prev);
			$nomegas += $prev if $with_same;
		} elsif( defined $size ) {

			# If the record has a size, it is of block form with diagonal of
			# length given by $size. The actual values in the model file is
			# then the arithmetic sum: (n*(n+1))/2

			#Kajsa: size also for diagonal matrix! Added type check below.

			if( $with_correlations and ($type eq 'BLOCK')){
				$nomegas += ($size*($size+1))/2; 
				$prev = ($size*($size+1))/2;
			} else {
				$nomegas += $size;
				$prev = $size;
			}
		} elsif (defined $omega->options) {
			$nomegas += scalar @{$omega -> options};
			$prev = scalar @{$omega -> options};
		} else {
			croak("Failed to parse \$OMEGA." );
		}
	}

	return $nomegas;
}

sub nsigmas
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 with_correlations => { isa => 'Bool', default => 0, optional => 1 },
		 with_same => { isa => 'Bool', default => 1, optional => 1 }
	);
	my $nsigmas;
	my $with_correlations = $parm{'with_correlations'};
	my $with_same = $parm{'with_same'};

	my $prev = undef;
	$self->sigmas([]) unless defined $self->sigmas;
	foreach my $sigma ( @{$self->sigmas} ) {
		my $size = $sigma -> size;
		my $type = $sigma -> type;
		if ($sigma->same() and (defined $prev) ) {
			$nsigmas += $prev if $with_same;
		} elsif( defined $size ) {

			# If the record has a size, it is of block form with diagonal of
			# length given by $size. The actual values in the model file is
			# then the arithmetic sum: (n*(n+1))/2
			#Kajsa: size also for diagonal matrix! Added check below.

			if( $with_correlations and ($type eq 'BLOCK')){
				$nsigmas += ($size*($size+1))/2;
				$prev = ($size*($size+1))/2;
			} else {
				$nsigmas += $size;
				$prev = $size;
			}
		} elsif (defined $sigma->options) {
			$nsigmas += scalar @{$sigma -> options};
			$prev = scalar @{$sigma -> options};
		} else {
			croak("Failed to parse \$SIGMA." );
		}
	}

	return $nsigmas;
}

sub eta_shrinkage
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 model => { 'model', optional => 1 },
		 probnum => { isa => 'Num', optional => 1 },
		 eta_filename => { isa => 'Maybe[Str]', optional => 1 },
		 directory => { isa => 'Str', optional => 1 }
	);
	my $model = $parm{'model'};
	my $probnum = $parm{'probnum'};
	my $eta_filename = $parm{'eta_filename'};
	my $directory = $parm{'directory'};

	my @eta_shrinkage = @{$self->shrinkage_module->eta_shrinkage(
		model => $model, 
		probnum => $probnum,
		directory => $directory,
		eta_filename => $eta_filename) };

	return \@eta_shrinkage;
}

sub iwres_shrinkage
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		model => { 'model', optional => 1 },
		iwres_filename => { isa => 'Maybe[Str]', optional => 1 },
		probnum => { isa => 'Num', optional => 1 },
		directory => { isa => 'Str', optional => 1 }
	);
	my @iwres_shrinkage;
	my $model = $parm{'model'};
	my $iwres_filename = $parm{'iwres_filename'};
	my $probnum = $parm{'probnum'};
	my $directory = $parm{'directory'};

	@iwres_shrinkage = @{$self->shrinkage_module -> iwres_shrinkage( model => $model, 
		probnum => $probnum,
		directory => $directory,
		iwres_filename => $iwres_filename)};

	return \@iwres_shrinkage;
}

sub add_records
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		record_strings => { isa => 'ArrayRef[Str]', optional => 0 },
		type => { isa => 'Str', optional => 0 }
	);
	my @record_strings = defined $parm{'record_strings'} ? @{$parm{'record_strings'}} : ();
	my $type = $parm{'type'};

	# add_records( type => 'subroutine',
	#               record_strings => ['OTHER=get_cov', 'OTHER=read'] )
	#
	# To read add a record, we figure out what its full class name
	# is. Then we check if we have an accessor for the record type,
	# if we do then the record is valid and we call the appropriate
	# contructor. Both record_strings and type are mandatory.

	my $rec_class = "model::problem::$type";
	my $accessor = $type.'s';
	my $n_previous_rows = 0;
	if ($type eq 'omega') {
		$n_previous_rows = $self->nomegas('with_correlations' => 0,'with_same' => 1);
	} elsif ($type eq 'sigma') {
		$n_previous_rows = $self->nsigmas('with_correlations' => 0,'with_same' => 1);
	} elsif ($type eq 'theta') {
		#this will be with priors
		$n_previous_rows = $self->record_count('record_name' => 'theta');
	}

	if ($self->can($accessor)) {
		my $record;
		$self->$accessor([]) unless defined $self->$accessor;
		if (($type eq 'omega') or ($type eq 'sigma') or ($type eq 'theta')) {
			$record = $rec_class->new(record_arr => \@record_strings, n_previous_rows => $n_previous_rows);
		}elsif ($type eq 'data'){
			$record = $rec_class->new(record_arr => \@record_strings, model_directory => $self->directory);
		} else {
			$record = $rec_class->new(record_arr => \@record_strings);
		}
		push(@{$self->$accessor}, $record);
		return $record;
	} else {
		croak("Trying to add unknown record: $type");
	}
}

sub set_records
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		record_strings => { isa => 'ArrayRef', default => [] },
		type => { isa => 'Str', optional => 0 },
		MX_PARAMS_VALIDATE_NO_CACHE => 1,
	);
	my @record_strings = @{$parm{'record_strings'}};
	my $type = $parm{'type'};

	my $rec_class = "model::problem::$type";
	my $accessor = $type.'s';
	if( $self->can($accessor) ){
		$self->$accessor([$rec_class->new ( record_arr => \@record_strings) ]);
	} else {
		die "Error in problem -> set_records: Trying to set unknown record: $type\n";
	}
}

sub _read_records
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		type => { isa => 'Str', optional => 1 }
	);
	my $type = $parm{'type'};

	# We parse the lines of a problem by looping over the them and
	# look for records(lines starting with a $). When a record is
	# found we set its index in the array as the end of the previous
	# record we found. We then know which lines to send to the
	# record object constructor. Then we set the end index of the
	# previous record as the start index of the next record. It is
	# assumed that the first record starts at line zero. The end of
	# the last record is the last line.

	my $start_index = 0;
	my $record_index = 0;
	my $end_index;
	my $first = 1;

	my @local_print_order=();
	my %found_records;

	# It may look like the loop takes one step to much, but its a
	# trick that helps parsing the last record.
	$self->prob_arr([]) unless defined $self->prob_arr;
	for( my $i = 0; $i <= @{$self->prob_arr}; $i++ ){

		# This if statement makes sure we dont access the array in the
		# last iteration of the loop. In all other iterations we need
		# a line of code to look for records starting lines.

		if( $i <= $#{$self->prob_arr} ){
			$_ = $self->prob_arr -> [$i];
		}

		# In this if statement we use the lazy evaluation of logical
		# or to make sure we only execute search pattern when we have
		# a line to search. Which is all cases but the very last loop
		# iteration.

		if( $i > $#{$self->prob_arr} or /^\s*\$(\w+)/ ){
			$end_index = $i;

			# The if statement here is only necessary in the first loop
			# iteration. When start_index == end_index == 0 we want to
			# skip to the next iteration looking for the actual end of
			# the first record.

			if( $end_index > $start_index and not $first){
				# extract lines of code:
				my @record_lines = @{$self->prob_arr}[$start_index .. $end_index-1];
				# extract the record name and get its long name:
				$self->prob_arr -> [$record_index] =~ /^\s*\$(\w+)/;
				my $record_name = $1;
				my $record_type = $self -> _normalize_record_name( record_name => $record_name );

				unless( length($record_type) > 0 ){
					croak("Record $record_name is not valid" );
				}

				unless ($found_records{$record_type} == 1){
					push(@local_print_order,$record_type);
					$found_records{$record_type}=1;
				}

				# reset the search for records by moving the record start
				# forwards:
				$start_index = $i;

				# let add_records create the object if appropriate

				if( $record_type eq 'warnings' ) {
					print "\nWarning: Record \$WARNINGS is deleted by PsN.\n";
				}elsif( $record_type eq 'finedata' ) {
					print "\nWarning: Record \$FINEDATA is deleted by PsN.\n";
				} elsif( $record_type eq 'table' ) {
					my $et_found = 0;
					my $wr_found = 0;
					if (defined $self->shrinkage_module){
						my $eta_name  = $self->shrinkage_module -> eta_tablename;
						my $wres_name = $self->shrinkage_module -> wres_tablename;
						foreach my $row ( @record_lines ) {
							$et_found++ if( $row =~ /$eta_name/ );
							$wr_found++ if( $row =~ /$wres_name/ );
						}
					}
					if( $et_found or $wr_found ) {
						$self->shrinkage_module -> enable;
					} else {
						$self -> add_records( record_strings => \@record_lines, 
							type => $record_type );
					}
				} else {
					$self -> add_records( record_strings => \@record_lines, 
										  type => $record_type );
				}
			}
			$first = 0;
			$record_index = $i;
		}  
	}
	$self->own_print_order(\@local_print_order);

}

sub check_start_eta
{
	#in frem check that start_eta parameter is acceptable
	# return order number (starts at 1) of omega record that starts with start eta
	my $self = shift;
	my %parm = validated_hash(\@_,
							  start_eta => {isa => 'Int', optional => 0}
		);
	my $start_eta = $parm{'start_eta'};
	my $omega_record_count=0;
	my $start_omega_record;
	croak ("start_eta must be positive") if ($start_eta < 1);
	my $netas = 0;
	my $prev;
	foreach my $omega (@{$self->omegas} ) {
		last if ($omega->prior());
		$omega_record_count++;
		if ($netas == ($start_eta -1)){
			$start_omega_record=$omega_record_count;
			last;
		}elsif($netas > ($start_eta -1)){
			croak("start_eta value illegal, start_eta must be first eta of an \$OMEGA record. Rewrite model.");
		}
		my $size = $omega -> size;
		my $type = $omega -> type;
		if ($omega->same()){
			croak("First \$OMEGA cannot be SAME")
				unless (defined $prev);
			$netas += $prev;
		} elsif( defined $size ) {
			$netas += $size;
			$prev = $size;
		} elsif (defined $omega->options) {
			$netas += scalar @{$omega -> options};
			$prev = scalar @{$omega -> options};
		} else {
			croak("Failed to parse \$OMEGA." );
		}
		
	}
	unless (defined $start_omega_record){
		croak("could not determine start omega record");
		#should be allowed to have start eta > than neta?
	}
	return $start_omega_record;
}

sub get_record_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  type => {isa => 'Str', optional => 0},
							  row_format => {isa => 'Bool', optional => 0},
							  record_number => {isa => 'Int', optional => 0}
		);
	my $type = $parm{'type'};
	my $record_number = $parm{'record_number'};
	my $row_format = $parm{'row_format'};
	my $accessor;
	if ($type eq 'omega'){
		$accessor = 'omegas';
	}elsif($type eq 'sigma'){
		$accessor = 'sigmas';
	}else{
		croak('Unknown parameter type '.$type.' in problem->get_record_matrix');
	}

	my @records =  @{$self -> $accessor};
	if ($record_number > scalar(@records)){
		croak("illegal input to get_record_matrix $record_number when number of records is ".scalar(@records));
	}
	if ($record_number < 1){
		croak("illegal input to get_record_matrix $record_number");
	}

	my $record_index = $record_number-1;
	if ($records[$record_index]->same()){
		croak("illegal input to get_record_matrix, record is SAME, record_number $record_number");
	}
	my $size = $records[$record_index]->size();
	unless (defined $size and $size > 0){
		$size =1;
	}
	my @new_matrix =();
	for (my $i=0; $i< $size; $i++){
		push(@new_matrix,[(0) x $size]);
	}
	unless (defined $records[$record_index] -> options()){
		croak("$type record has no values");
	}
	my @options = @{$records[$record_index] -> options()};
	my $name = $options[0] -> coordinate_string();
	croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
	my $startrow = $1;
	my $col = $2;
	if ($options[0]->on_diagonal()){
		croak("col and row in $name not diagonal element") unless ($startrow == $col );
	}else{
		croak("first element not on diagonal");
	}

	foreach my $option (@options) {
		my $name = $option -> coordinate_string();
		croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
		my $value = $option ->init();
		if ($row_format){
			$new_matrix[($1-$startrow)][($2-$startrow)] = $value;
		}else{
			$new_matrix[($2-$startrow)][($1-$startrow)] = $value;
		}
		if ($option->on_diagonal()){
			croak("col and row in $name not diagonal element") unless ($2 == $1 );
		}
	}

	return \@new_matrix;

}

sub get_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  type => {isa => 'Str', optional => 0},
							  start_row => {isa => 'Int', optional => 0},
							  end_row => {isa => 'Int', optional => 1}
		);
	my $type = $parm{'type'};
	my $start_row = $parm{'start_row'};
	my $end_row = $parm{'end_row'};
	my $accessor;
	if ($type eq 'omega'){
		$accessor = 'omegas';
	}elsif($type eq 'sigma'){
		$accessor = 'sigmas';
	}else{
		croak('Unknown parameter type '.$type.' in problem->get_matrix');
	}
	my $sa = 'n'.$accessor;
	my $old_size = $self->$sa(with_correlations => 0, with_same => 1);
	if ($old_size < $start_row){
		croak ("Illegal input to get_matrix, start_row $start_row larger than size $old_size");
	}
	$end_row = $old_size unless (defined $end_row);
	if ($end_row < $start_row){
		croak ("Illegal input to get_matrix, end_row $end_row smaller than start_row $start_row");
	}
	my $new_size = ($end_row - $start_row +1);
	my @old_matrix=();
	for (my $i=0; $i < $old_size; $i++){
		push(@old_matrix,[(0) x $old_size]);
	}

	my @records =  @{$self -> $accessor};

	my $block_size;
	my $prev_rows=0;
	for (my $i=0; $i<scalar(@records); $i++){
		#print "$i\n";
		if  ($records[$i]->same() ){
			#store values in old_matrix
			my $old_start = $prev_rows - $block_size;
			for (my $row=0; $row< $block_size; $row++){
				for (my $col=0; $col<=$row; $col++){
					my $value = $old_matrix[$old_start+$row][$old_start+$col];
					$old_matrix[$prev_rows+$row][$prev_rows+$col] = $value;
					#print "same $value prev_rows $prev_rows\n";
				}
			}
			$prev_rows += $block_size;
			next;
		}
		unless (defined $records[$i] -> options()){
			croak("$type record has no values");
		}
		if (defined $records[$i] -> size() and ($records[$i] -> size() > 0)){
			$block_size = $records[$i]->size();
		}else{
			$block_size = 1;
		}
		foreach my $option (@{$records[$i] -> options()}) {
			my $name = $option -> coordinate_string();
			croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
			croak("row in $name outside size $old_size") if ($1 > $old_size );
			croak("col in $name outside size $old_size") if ($2 > $old_size );
			my $value = $option ->init();
			$old_matrix[($1-1)][($2-1)] = $value;
			if ($option->on_diagonal()){
				croak("col and row in $name not diagonal element") unless ($2 == $1 );
				$prev_rows++;
			}
		}
	}

	my $diff=$start_row-1;
	my @new_matrix=();
	for (my $i=0; $i < $new_size; $i++){
		push(@new_matrix,[(0) x $new_size]);
		for (my $j=0; $j < $new_size; $j++){
			$new_matrix[$i][$j] = $old_matrix[$i+$diff][$j+$diff];
		}
	}

	unless ($new_matrix[$new_size-1][$new_size-1] > 0){
		print "\n";
		for (my $i=0; $i< scalar(@new_matrix); $i++){
			print join(' ',@{$new_matrix[$i]})."\n";
		}

		croak ("Error in problem->get_matrix: new matrix not filled up. start_row $start_row");
	}

	return \@new_matrix;

}


sub get_filled_omega_matrix
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  covmatrix => { isa => 'Ref', optional => 1 },
							  start_eta => { isa => 'Int', optional => 0 }
		);
	my $covmatrix = $parm{'covmatrix'};
	my $start_eta = $parm{'start_eta'};
	my $smallval = 0.0001;

	#create one big full omega block (lower triangular) as new_omega->[$row][$col]
	#input is optional covmatrix to be used for 0 off-diags
	#if old off-diagonals not present then set small values

	my $new_full_omega = $self->get_matrix( type=> 'omega',
											start_row => $start_eta);
	
	##Determine minimum difference between off-diagonal absolute sum and diagonal,
	#needed to determine appropriate values to fill in
	my $minimum_difference;
	my $new_size = scalar(@{$new_full_omega});

	if (defined $covmatrix and (not scalar(@{$covmatrix}) == $new_size)){
		croak("Error input get_filled_omega_matrix, size existing is $new_size after start_eta $start_eta ".
			"while size covmatrix is ".scalar(@{$covmatrix}));
	}
	my @off_diagonal_sum = 0 x $new_size; 
	my @diagonal_value = 0 x $new_size; 

	for (my $row=0; $row< $new_size; $row++){
		my $val = abs($new_full_omega->[$row][$row]);
		$diagonal_value[$row] = $val;
		for (my $col=0; $col < $row; $col++){
			my $val = abs($new_full_omega->[$row][$col]);
			$off_diagonal_sum[$row] += $val;
			$off_diagonal_sum[$col] += $val;
		}
	}

	$minimum_difference = $diagonal_value[0]-$off_diagonal_sum[0];
	for (my $i=1; $i<$new_size; $i++){
		my $diff = $diagonal_value[$i]-$off_diagonal_sum[$i];
		$minimum_difference = $diff if ($diff< $minimum_difference and ($diff>0));
	}

	my $max_off_diagonal = $smallval; #check Ron's hands on for typical value here
	my $temp = $minimum_difference;
	if ($new_size > 1){
		$temp = ($minimum_difference/($new_size-1));
	}
	$max_off_diagonal = abs($temp)*(0.9) if (abs($temp) < $max_off_diagonal);
	#print "max off diag is $max_off_diagonal\n";
	#fill off-diagonals in new_omega
	my $k=1;
	for (my $row=0; $row< $new_size; $row++){
		for (my $col=0; $col<$row; $col++){
			if ($new_full_omega->[$row][$col] == 0){
				if (defined $covmatrix and (abs($covmatrix->[$row][$col]) > 0.000001)  ){
					$new_full_omega->[$row][$col] = $covmatrix->[$row][$col];
				}else{
					$new_full_omega->[$row][$col] = $max_off_diagonal;
					$k++;
				}
			}
		}
	}

	return $new_full_omega;
}

sub add_omega_block
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 new_omega => { isa => 'Ref', optional => 0 },
		 labels => { isa => 'Ref', optional => 1 }
	);
	my $new_omega = $parm{'new_omega'};
	my $labels = $parm{'labels'};

	#input is $new_omega as $new_omega->[$row][$col]
	#
	# add new BLOCK(size)

	my $size = scalar(@{$new_omega});
	return if ($size < 1);
	my @record_lines=();
	push(@record_lines,'BLOCK('.$size.') ');
	my $form = '  %.6G';
	for (my $row=0; $row< $size; $row++){
		my $line;
		for (my $col=0; $col<=$row; $col++){
			my $str= sprintf("$form",$new_omega->[$row][$col]); 
			$line = $line.' '.$str;
		}
		my $comment ='';
		$comment = '; '.$labels->[$row] if (defined $labels and scalar(@{$labels}) > $row);
		push(@record_lines,$line.$comment);
	}
	$self -> add_records( record_strings => \@record_lines, 
		type => 'omega' );
}

sub _format_problem
{
	my $self = shift;
	my %parm = validated_hash(\@_,
							  filename => { isa => 'Str', optional => 1 },
							  problem_number => { isa => 'Int', optional => 1 },
							  relative_data_path => { isa => 'Bool', optional => 0 },
							  write_directory => { isa => 'Str', optional => 0 },
							  local_print_order => { isa => 'Bool', default => 0, optional => 1 },
							  number_format => { isa => 'Maybe[Int]', optional => 1 }
		);
	my $filename = $parm{'filename'};
	my $problem_number = $parm{'problem_number'};
	my $local_print_order = $parm{'local_print_order'};
	my $number_format = $parm{'number_format'};
	my $write_directory = $parm{'write_directory'};
	my $relative_data_path = $parm{'relative_data_path'};
	my @formatted;

	# problem::_format_problem()

	# format_problem will return an array of strings of the
	# problem in NONMEM modelfile format.

	# Loop over the print_order array that contains strings of
	# valid record types in the order they should appear in a
	# NONMEM modelfile. So if the order of some records are
	# interchangable and the file from which the object was
	# initialized has records in an order different from
	# print_order, the file will still be valid, but will look
	# different from what it used to.
	my $record_order = \@print_order;
	if ($local_print_order and defined $self->own_print_order and scalar(@{$self->own_print_order})>0){
		$record_order = $self->own_print_order;
	}elsif ($self->sde){
		$record_order = \@sde_print_order;
	}elsif ($self->omega_before_pk){
		$record_order = \@print_order_omega_before_pk;
	}
	foreach my $type ( @${record_order} ) {
	  # Create an accessor string for the record being formatted
	  my $accessor = $type.'s';

	  # Se if we have one or more records of the type given in
	  # print_order
	  if ( defined $self->$accessor ) {
	    # Loop over all such records and call on the record object
	    # to format itself.

	    foreach my $record ( @{$self->$accessor} ){

			my $arr;
			if ($type eq 'data'){
				$arr = $record -> _format_record(write_directory => $write_directory,
												 relative_data_path => $relative_data_path);
			}else{
				$arr = $record ->  _format_record( number_format => $number_format) ;
			}
			push( @formatted,  @{$arr} );
	    }
	  }
	  if( $self->shrinkage_module -> enabled and $type eq 'table' ) {
	    push( @formatted,
		  @{$self->shrinkage_module -> format_shrinkage_tables } );
	  }	
	}

	if( $self->cwres_modules ){
	  $self->cwres_modules -> [0] -> post_process;
	}
	
	return \@formatted;
}

sub ensure_diagonal_dominance
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		verbose => { isa => 'Bool', default => 0, optional => 1 }
	);
	my $verbose = $parm{'verbose'};

	#check here that omega and sigma strictly diagonally dominant
	#otherwise decrease magnitude of diagonal elements 
	# by enough to achieve strict diagonal dominance
	# do row by row
	#check number of diagonal elements
	#make array of zeros for off_diagonal_sums, one per diagonal value
	#go through records
	#check that block, and not fixed or same
	#need only check values wihtin block, row sums not affected by anything outside
	#go through all options, if on-diagonal then skip. get row, col indexes
	#add value  to sum for diagonal(row) and diagonal (col)
	#loop through options again, if not on-diagonal, skip.
	#if on-diagonal, check that value strictly greater than sum.
	#otherwise compute deflation factor

	#dont touch priors
	foreach my $param ('omega','sigma') {
		my $adjusted = 0;
		my $accessor = $param.'s';
		my $size_accessor = 'n'.$param.'s';
		my @records;
		if (defined $self -> $accessor()) {
			@records = @{$self -> $accessor()};
		}
		next unless (scalar(@records) > 0); #no parameter in this problem
		my $size = $self->$size_accessor('with_correlations' => 0,'with_same' => 1);
		my @off_diagonal_sum = 0 x $size; 
		foreach my $record (@records){
			next unless ($record -> type() eq 'BLOCK');
			if  ($record->same() or $record->fix() or $record->prior()){
				next;
			}
			next if ($record->size() < 2);
			unless (defined $record -> options()){
				croak("$param record has no values");
			}
			foreach my $option (@{$record -> options()}) {
				next if ($option->on_diagonal());
				my $name = $option -> coordinate_string();
				croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
				croak("row in $name outside size $size") if ($1 > $size );
				croak("col in $name outside size $size") if ($2 > $size );
				my $val = abs($option ->init());
				$off_diagonal_sum[($1-1)] += $val;
				$off_diagonal_sum[($2-1)] += $val;
			}
			my %adjust_row = {};
			foreach my $option (@{$record -> options()}) {
				next unless ($option->on_diagonal());
				my $name = $option -> coordinate_string();
				croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
				croak("row in $name outside size $size") if ($1 > $size );
				croak("col and row in $name not diagonal element") unless ($2 == $1 );
				my $val = $option ->init();
				unless ($val > $off_diagonal_sum[($1-1)] ){
					my $ratio = $val/$off_diagonal_sum[($1-1)]; # less than 1, larger than 0 (abs sum, pos diag)
					$adjust_row{$1} = $ratio*(0.99);
				}
			}
			#new loop here to decrease off-diag
			foreach my $option (@{$record -> options()}) {
				next if ($option->on_diagonal());
				my $name = $option -> coordinate_string();
				croak("unknown coord $name ") unless ($name =~ /A\((\d+),(\d+)\)/ );
				croak("row in $name outside size $size") if ($1 > $size );
				croak("col in $name outside size $size") if ($2 > $size );
				my $deflate = 1;
				foreach my $row (keys %adjust_row){
					if ($row == $1 or $row == $2){
						$deflate = $adjust_row{$row} if ($adjust_row{$row} < $deflate);
					}
				}
				next unless ($deflate < 1);
				my $val = $option ->init();
				my $value = $val*$deflate;
				if ($value < 1 and $value > 0){
					$value = sprintf "%.5f", $value; #need to control so dont get e notation
					$value     = '0' if eval($value) == 0;
				}elsif ($value > -1 and $value < 0){
					$value = sprintf "%.4f", $value; #need to control so dont get e notation
					$value     = '0' if eval($value) == 0;
				}else{
					$value = sprintf "%6.2f", $value; #need to control so dont get e notation
					my ($big,$small) = split('\.',$value);
					$small           = substr($small,0,3);
					if ((length($big)+ length($small)) > 7){
						$value = $big;
					}else{
						$value     = $big.'.'.$small;
					}
					$value     = '0' if eval($value) == 0;
				}

				$adjusted = 1;
				$option -> check_and_set_init( new_value => $value );

			}
		}
		if ($adjusted and $verbose){
			print "Decreased off-diagonal values of $param from input to ensure strict diagonal dominance in output model.\n";
		}
	}
}

sub update_prior_information
{
	my $self = shift;

	#if records THETAP or THETAPV or OMEGAP or OMEGAPD exists together with NTHETA or NETA then die
	# if new records exist then do not set prior flag for any records. Otherwise proceed to prior flag setting
	#if $PRIOR NWPRI then get NTHETA and NETA unless have new records NM7.3
	#loop through $THETA and $OMEGA and set prior=1 for 
	#all params after the estimated ones

	if ((defined $self->priors()) and scalar(@{$self -> priors()})>0 ){
		my $nwpri=0;
		foreach my $rec (@{$self -> priors()}){
			unless ((defined $rec) &&( defined $rec -> options )){
				carp("No options for rec \$PRIOR" );
			}
			foreach my $option ( @{$rec -> options} ) {
				if ((defined $option) and 
						(($option->name eq 'NWPRI') || (index('NWPRI',$option ->name ) == 0))){
					$nwpri=1;
				}
			}
		}
		if ($nwpri){
			my $neta;
			my $ntheta;
			foreach my $rec (@{$self -> priors()}){
				foreach my $option ( @{$rec -> options} ) {
					if (defined $option){
						if  ($option->name eq 'NETA'){ #NONMEM does not allow abbrev
							if ( (defined $option->value) and ($option->value ne '')){
								#now we must split on ,
								my @rest;
								($neta,@rest) = split(',',$option->value);
							}
						}elsif  ($option->name eq 'NTHETA'){ #NONMEM does not allow abbrev
							if ( (defined $option->value) and ($option->value ne '')){
								#now we must split on ,
								my @rest;
								($ntheta,@rest) = split(',',$option->value);
							}
						}
						if ( (defined $option->value) and ($option->value ne '')){
							if ($option->value =~  /NETA/){
								my @opts = split(',',$option->value);
								foreach my $opt (@opts){
									if ($opt =~ /^NETA=([0-9]+)$/){
										$neta = $1;
									}
								}
							}
							if ($option->value =~  /NTHETA/){
								my @opts = split(',',$option->value);
								foreach my $opt (@opts){
									if ($opt =~ /^NTHETA=([0-9]+)$/){
										$ntheta = $1;
									}
								}
							}
						}
					}
				}
			}
			my $any_new_prior_record=0;
			my @newrecs = ('THETAP','THETAPV','OMEGAP','OMEGAPD','SIGMAP','SIGMAPD');
			foreach my $newprec (@newrecs){
				my $acc = lc($newprec).'s';
				if ((defined $self->$acc) and scalar(@{$self->$acc})>0){
					$any_new_prior_record=1;
					last;
				}
			}
			if ($any_new_prior_record and ((defined $neta) or (defined $ntheta))){
				croak("PsN does not support NETA or NTHETA in \$PRIOR in combination with new prior defining records ".
					  join(',',@newrecs));
			}

			
			#if we have any_new_prior_record it is vital that nwpri_ntheta and nwpri_neta remain undef,
			#since these attributes are used as an indication that $THETA and $OMEGA contain prior information
			return if $any_new_prior_record;

			unless (defined $neta){
				$neta=0;
			}
			unless (defined $ntheta){
				$ntheta=0;
			}
			$self->nwpri_ntheta($ntheta);
			$self->nwpri_neta($neta);

			#set prior in params
			if( defined $self->thetas ){
				my $counter=0;
				foreach my $record ( @{$self->thetas} ){
					if( defined $record -> options ){
						foreach my $opt( @{$record -> options}){
							$counter++;
							if ($counter > $ntheta){
								#set prior
								$opt->prior(1);
							}
						}
					}
				}
			}
			if( defined $self->omegas ){
				my $prevdiag =undef;
				my $ndiag=0;
				my $exact=0;
				foreach my $omega ( @{$self->omegas} ) {
					my $size = $omega -> size;
					my $type = $omega -> type;

					#assume do not start priors in the middle of a block.
					#Only check after each new record if reached $neta
					if ($omega->same()) {
						croak("First \$OMEGA cannot be SAME")
							unless (defined $prevdiag);
						$ndiag += $prevdiag;
					} elsif( defined $size ) {
						$ndiag += $size;
						$prevdiag = $size;
					} elsif (defined $omega->options) {
						$ndiag += scalar @{$omega -> options};
						$prevdiag = scalar @{$omega -> options};
					} else {
						croak("Failed to parse \$OMEGA." );
					}
					if ($ndiag == $neta){
						$exact = 1;
					}elsif ($ndiag > $neta){
						croak("It seems that one \$OMEGA record contains both ".
								"estimated and prior OMEGAs. PsN requires that this ".
								"\$OMEGA is split into two so that priors are separate.")
							unless ($exact);
						$omega->prior(1);
					}
				}
			}
		}
	}
}

sub tbs_transform
{
	my $self = shift;

	if ((defined $self->priors()) and scalar(@{$self -> priors()})>0 ){
		croak("The current version of PsN does not support \$PRIOR and ".
				"option -tbs or -dtbs in combination");
	}      

	my $zeta_line='';
	my $newthetanum;
	$newthetanum=$self -> record_count( record_name => 'theta' )+1; #this is the lambda
	$self->tbs_thetanum($newthetanum);
	if (defined $self->tbs_lambda()){
		$self->add_records( type => 'theta',
				record_strings => [$self->tbs_lambda().' ; tbs_lambda'] );
	}else{
		$self->add_records( type => 'theta',
				record_strings => ['1 ; tbs_lambda'] );
	}
	
	#PRED or ERROR
	my @code;
	@code = @{$self -> errors()->[0]->code()} if (defined $self -> errors());
	my $use_pred = 0;
	unless ( $#code > 0 ) {
		@code = @{$self -> preds()->[0]->code()} if (defined $self -> preds());
		$use_pred = 1;
	}
	if ( $#code <= 0 ) {
		croak("Neither ERROR or PRED defined, cannot use -tbs\n" );
	}

	#locate first use of IWRES in right-hand-side, same for W, IPRED. Check Uppsala style
	#TODO cannot handle IF ELSE around IPRED, IWRES and Y
	my $zeta_default=1;
	my $found=0;
	my $W_line='';
	for (my $i=$#code; $i>=0;$i--){
		if (($code[$i] =~ /[^a-zA-Z_0-9]Y\s*=/) or
			($code[$i] =~ /^Y\s*=/)){
			if ($found){
				croak("Cannot handle multiple-line definitions of Y with option -tbs.");
			}else{
				#remove Y=... line
				@code =  (@code[0..$i-1],
						  @code[$i+1..$#code]);
				$found=1;
			}
		}elsif($code[$i] =~ /^\s*W\s*=\s*(.*)$/){
			my $line = $1;
			if (length($W_line)>0){
				croak("Cannot handle multiple-line definitions of W with option -tbs or -dtbs.");
			}else{
				#store W=... line
				#reformat W line
				my $iprtheta;
				my $addtheta;
				if ($line =~ /SQRT/){
					#find the theta coupled to IPRED and remove it from the string
					if ($line =~ s/\bIPRED\*\*2\s*\*\s*THETA\((\d+)\)//){
						$iprtheta = $1;
					}elsif($line =~ s/\bTHETA\((\d+)\)\*\*2\s*\*\s*IPRED\b//){
						$iprtheta = $1;
					}else{
						croak ("Could not parse IPRED*THETA part of $line when transforming for tbs\n");
					} 
					#find the additive theta
					if($line =~ s/\bTHETA\((\d+)\)//){
						$addtheta = $1;
					}else{
						croak ("Could not parse additive THETA part of $line when transforming for tbs\n");
					} 

					my $zerofixtheta = $iprtheta; #if tbs
					$zerofixtheta = $addtheta if ($self->dtbs());

					#find $zerofixtheta theta in list of theta records. Need to loop since do not know which record
					my $thetacount = 0;
					my $setfix=0;
					foreach my $rec (@{$self->thetas}){
						if( defined $rec -> options and ($thetacount + scalar(@{$rec -> options}))>=$zerofixtheta ){
							$rec->options()->[$zerofixtheta-$thetacount-1]->lobnd(undef);
							$rec->options()->[$zerofixtheta-$thetacount-1]->upbnd(undef);
							$rec->options()->[$zerofixtheta-$thetacount-1]->init(0);
							$rec->options()->[$zerofixtheta-$thetacount-1]->fix(1);
							$setfix=1;
							last;
						}else{
							$thetacount += scalar(@{$rec -> options});
						}
					}
					croak("Could not set THETA $zerofixtheta to 0 FIX") unless $setfix;
					#construct new W string
					if ($self->dtbs()){
						$W_line = ' W = THETA('.$iprtheta.')*IPRED**ZETA+THETA('.$addtheta.')'."\n";
					}else{
						$W_line = ' W = THETA('.$iprtheta.')+THETA('.$addtheta.')'."\n";
					}
				}elsif ($line =~ /\bIPRED\b/){
					if ($self->dtbs()){
						#replace every occurrence of IPRED with IPRED**ZETA
						$line =~ s/\bIPRED\b/IPRED\*\*ZETA/g ;
						$W_line = ' W = '.$line."\n";
					}else{
						#remove IPRED dependence.
						$line =~ s/\*\s*IPRED\b//g ;
						$line =~ s/\bIPRED\s*\*//g ;
						$line =~ s/\+\s*IPRED\b//g ;
						$line =~ s/\bIPRED\s*\+//g ;
						if ($line =~ /\bIPRED\b/){
							croak ("Failed to remove IPRED from $line for tbs");
						}
						$W_line = ' W = '.$line."\n";
					}
				}else{
					#some additive error
					if ($self->dtbs()){
						#remove leading whitespace
						$line =~ s/^\s*//;
						#prepend IPRED**ZETA*
						$W_line = ' W = (IPRED**ZETA)*'.$line."\n";
						$zeta_default=0.001; #close to zero but not exactly 0 since NM will not allow it
					}else{
						#tbs, leave as is 
						$W_line = ' W = '.$line."\n";
					}

				}

				@code =  (@code[0..$i-1],
						  @code[$i+1..$#code]);
			}
		} 
	}
	croak("Failed to find Y definition row in \$PK/\$ERROR") unless ($found);
	croak("Failed to find W definition row in \$PK/\$ERROR") unless (length($W_line)>0);


	#if $self->tbs rather than $self->dtbs then do not add anything more
	if (defined $self->tbs_delta()){
		$zeta_line = ' DELTA = THETA('.($newthetanum+1).')'."\n".' ZETA = LAMBDA + DELTA'."\n";
		$self->add_records( type => 'theta',
							record_strings => [$self->tbs_delta().' ; tbs_delta'] );
	}elsif (defined $self->tbs_zeta()){
		$zeta_line = ' ZETA = THETA('.($newthetanum+1).')'."\n";
		$self->add_records( type => 'theta',
							record_strings => [$self->tbs_zeta().' ; tbs_zeta'] );
	}elsif ($self->dtbs()){
		#default ZETA
		$zeta_line = ' ZETA = THETA('.($newthetanum+1).')'."\n";
		$self->add_records( type => 'theta',
							record_strings => [$zeta_default.' ; tbs_zeta'] );
	}

	
	#Find last occurrence of IPRED = ... by scanning from the end.
	#Then keep the IPRED row, and then insert extra stuff directly after
	#insert modified W as first insertion after IPRED
	my $ipredrow=0;
	for (my $i=$#code; $i>=0;$i--){
		if (($code[$i] =~ /[^a-zA-Z_0-9]IPRED\s*=/) or
			($code[$i] =~ /^IPRED\s*=/)){
			# $zeta_line and $W_line can be empty
			@code =  (@code[0..$i],
					  " LAMBDA = THETA($newthetanum)\n",
					  $zeta_line,
					  $W_line,
					  " IPRTR=IPRED\n",
					  " IF (LAMBDA .NE. 0 .AND. IPRED .NE.0) THEN\n",
					  "    IPRTR=(IPRED**LAMBDA-1)/LAMBDA\n",
					  " ENDIF\n",
					  " IF (LAMBDA .EQ. 0 .AND. IPRED .NE.0) THEN\n",
					  "    IPRTR=LOG(IPRED)\n",
					  " ENDIF\n",
					  " IF (LAMBDA .NE. 0 .AND. IPRED .EQ.0) THEN\n",
					  "    IPRTR=-1/LAMBDA\n",
					  " ENDIF\n",
					  " IF (LAMBDA .EQ. 0 .AND. IPRED .EQ.0) THEN\n",
					  "    IPRTR=-1000000000\n",
					  " ENDIF\n",
					  " IPRED=IPRTR\n",
					  @code[$i+1..$#code]);
			$ipredrow = $i+14;
			$found=1;
			last;
		}
	}
	croak("Failed to find IPRED definition row in \$PK/\$ERROR. ".
			"Do not know where in code to add IPRED transformation.") unless ($found);
	$found=0;

	#find last occurrence of IWRES=... by scanning from the end
	#keep that line and then insert stuff directly after it
	for (my $i=$#code; $i>=0;$i--){
		if (($code[$i] =~ /[^a-zA-Z_0-9]IWRES\s*=/) or
				($code[$i] =~ /^IWRES\s*=/)){
			my $row=$i;
			$row=$ipredrow if ($ipredrow > $i);
			@code =  (@code[0..$row],
		    " IWRTR=IWRES\n",
				" IF (LAMBDA.NE.0 .AND. DV.NE.0 .AND. W.NE.0) THEN\n",
				"    IWRTR=((DV**LAMBDA-1)/LAMBDA-IPRED)/W\n",
				" ENDIF\n",
				" IF (LAMBDA.EQ.0 .AND. DV.NE.0 .AND. W.NE.0) THEN\n",
				"    IWRTR=(LOG(DV)-IPRED)/W\n",
				" ENDIF\n",
				" IF (LAMBDA.NE.0 .AND. DV.EQ.0 .AND. W.NE.0) THEN\n",
				"    IWRTR=(-1/LAMBDA-IPRED)/W\n",
				" ENDIF\n",
				" IF (LAMBDA.EQ.0 .AND. DV.EQ.0 .AND. W.NE.0) THEN\n",
				"    IWRTR=(-1000000000-IPRED)/W\n",
				" ENDIF\n",
				" IWRES=IWRTR\n",
				" Y=IPRED+EPS(1)*W\n",
				@code[$row+1..$#code]);
			$found=1;
			last;
		}
	}
	croak("Failed to find IWRES definition row in \$PK/\$ERROR. ".
			"Do not know where in code to add IWRES transformation.") unless ($found);

	if ( $use_pred ) {
		$self -> preds ->[0] ->code (\@code );
	} else {
		$self -> errors ->[0] ->code (\@code );
	}

#$SUBS. check NM major version. Option to modelfit so do not need to print copy 
# everywhere?
#need to copy ccontr etc also.

	$self-> add_option(option_name  => 'CONTR',
			option_value => 'contr.txt',
			record_name => 'subroutine',
			add_record => 1,
			record_number => -1);

	my $val = 'ccontra_nm7.txt';
	if ($PsN::nm_major_version < 7){
		$val = 'ccontra_nm6.txt';
	}
	$self-> add_option(option_name  => 'CCONTR',
			option_value => $val,
			record_name => 'subroutine',
			record_number => -1);
}

sub _normalize_record_name
{
	my $self = shift;
	my %parm = validated_hash(\@_,
		 record_name => { isa => 'Str', optional => 0 }
	);
	my $record_name = $parm{'record_name'};
	my $normalized_name;

	# This code takes a recordname (which likely is uppercase and
	# semilong), creates its short uppercase format and looks up the
	# long, lowercase, name in the abbreviations hash that was
	# initialized in "new". The name is assumed to be valid, if its
	# not, an empty string will be returned, but no error produced (
	# a warning might be nice though ) (Errorhandling is now done in
	# "read_records".



	if ($unsupported_records{uc($record_name)} > 0){
		croak("\nPsN does not yet support record \$".$record_name." in the control stream, but adding support is on the todo-list.\n");
	}
	my $uc_short_type = _get_uc_short_type($record_name);

	$normalized_name = $abbreviations{$uc_short_type};

	unless (length($normalized_name)>0){
		croak("\nPsN does not support record \$".$record_name." in the control stream\n");
	}

	return $normalized_name;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
