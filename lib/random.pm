package random;

use strict;
use Digest::SHA qw(sha1_hex);
use Exporter qw(import);
use PsN;
use linear_algebra;

our @EXPORT = qw(random_set_seed_from_phrase random_uniform_integer random_normal random_uniform random_permuted_index random_get_seed random_set_seed);
our @EXPORT_OK = qw(random_multivariate_normal);

our $old;
our @global_seed;


if (PsN::use_old_math_random) {
    require Math::Random;
    $old = 1;
} else {
    require Math::Random::Free;
    $old = 0;
}


sub random_set_seed
{
    if ($old) {
        Math::Random::random_set_seed(@_);
    } else {
        my ($s1, $s2) = @_;
        my $seed = $s1 ^ $s2;
        $global_seed[0] = $seed;
        $global_seed[1] = 0;
        srand $seed;
    }
}


sub random_get_seed
{
    if ($old) {
        return Math::Random::random_get_seed();
    } else {
        return @global_seed;
    }
}


sub random_set_seed_from_phrase
{
    if ($old) {
        Math::Random::random_set_seed_from_phrase(@_); 
    } else {
        my ($seed) = @_;
        my $value = hex substr(sha1_hex($seed), 0, 6);
        $global_seed[0] = $value;
        $global_seed[1] = 0;
        srand $value;
    }
}


sub random_uniform_integer
{
    if ($old) {
        return Math::Random::random_uniform_integer(@_); 
    } else {
        return Math::Random::Free::random_uniform_integer(@_);
    }
}


sub random_uniform
{
    if ($old) {
        return Math::Random::random_uniform(@_); 
    } else {
        return Math::Random::Free::random_uniform(@_);
    }
}


sub random_normal
{
    if ($old) {
        return Math::Random::random_normal(@_); 
    } else {
        return Math::Random::Free::random_normal(@_);
    }
}


sub random_permuted_index
{
    if ($old) {
        return Math::Random::random_permuted_index(@_); 
    } else {
        return Math::Random::Free::random_permuted_index(@_);
    }
}


sub random_multivariate_normal
{
    if ($old) {
        return Math::Random::random_multivariate_normal(@_);
    } else {
        my $n = shift;
        my @results;
        my $size = scalar(@_) / 2;
        my @mu = @_[0..$size-1];
        my @cov = @_[$size..scalar(@_) - 1];
        use Storable qw(dclone);
        my @covcopy = @{dclone(\@cov)};
        linear_algebra::cholesky(\@covcopy);
        for (my $i = 0; $i < $n; $i++) {
            my @standard_normal = random_normal($size, 0, 1);
            my @vec;
            for (my $row = 0; $row < $size; $row++) {
                my $s = $mu[$row];
                for (my $col = 0; $col <= $row; $col++) {
                    $s += $covcopy[$col]->[$row] * $standard_normal[$col];
                }
                push @vec, $s;
            }
            push @results, \@vec;
        }
        return @results;
    }
}


1;
