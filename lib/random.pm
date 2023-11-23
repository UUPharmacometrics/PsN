package random;

use strict;
use Exporter qw(import);
use PsN;

our @EXPORT = qw(random_set_seed_from_phrase random_uniform_integer random_permuted_index);

our $old;

if (PsN::use_old_math_random) {
    require Math::Random;
    $old = 1;
} else {
    require Math::Random::Free;
    $old = 0;
}


sub random_set_seed_from_phrase
{
    if ($old) {
        Math::Random::random_set_seed_from_phrase(@_); 
    } else {
        Math::Random::Free::random_set_seed_from_phrase(@_);
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


sub random_permuted_index
{
    if ($old) {
        return Math::Random::random_permuted_index(@_); 
    } else {
        return Math::Random::Free::random_permuted_index(@_);
    }
}


1;
