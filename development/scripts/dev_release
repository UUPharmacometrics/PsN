#!/usr/bin/perl

my $version = $ARGV[0];

my $user = 'rikno764';
my $host = 'farmbio-n43.farmbio.uu.se';

chdir "../..";

system "make";
system "make release";
system "ssh $user\@$host 'rm PsN-Source -rf'";
system "scp -r PsN-Source $user\@$host:.";
system "ssh -t $user\@$host 'cd PsN-Source;perl setup.pl'";
system "ssh $user\@$host './postinstall $version -dev'";
