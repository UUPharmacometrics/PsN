package random;

use strict;
use Exporter qw(import);
use PsN;

our @EXPORT = qw(random_set_seed_from_phrase random_uniform_integer random_uniform random_permuted_index);

our $old;

if (PsN::use_old_math_random) {
    require Math::Random;
    require Math::Random::Free;     # FIXME: Remove this
    $old = 1;
} else {
    require Math::Random::Free;
    require Math::Random;           # FIXME: Remove this
    $old = 0;
}


sub random_set_seed
{
    if ($old) {
        Math::Random::set_seed(@_);
    } else {
        my ($s1, $s2) = @_; 
        my $seed = $s1 ^ $s2;
        srand $seed;
    }
}


sub random_set_seed_from_phrase
{
    if ($old) {
        Math::Random::random_set_seed_from_phrase(@_); 
        Math::Random::Free::random_set_seed_from_phrase(@_);    # FIXME: Remove this
    } else {
        Math::Random::Free::random_set_seed_from_phrase(@_);
        Math::Random::random_set_seed_from_phrase(@_);      # FIXME: Remove this
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


sub random_permuted_index
{
    if ($old) {
        return Math::Random::random_permuted_index(@_); 
    } else {
        return Math::Random::Free::random_permuted_index(@_);
    }
}


1;
