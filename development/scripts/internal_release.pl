#!/usr/bin/perl

# Argument: version-tag to release

if (scalar(@ARGV) != 1) {
    die "Must specify version tag name\n";
}

my $tag = $ARGV[0];

my $access_token = readpipe("zenity --entry --text \"Enter your github access token\" 2> /dev/null");
chomp $access_token;

my $cmd = <<"EOF";
curl --data '{"tag_name": "$tag","target_commitish": "master","name": "PsN $tag internal release","body": "internal release","draft": false,"prerelease": true}' https://api.github.com/repos/UUPharmacometrics/PsN/releases?access_token=$access_token
EOF

my $response = readpipe($cmd);

$response =~ /\s\s\"id\":\s(\d+)/m;
my $id = $1; 

chdir "../.." or die "Could not change directory";

my @files = ("PsN-$tag.tar.gz", "nmoutput2so-$tag.zip");

for my $filename (@files) {
    my $upload_cmd = <<"EOF";
curl -# -XPOST -H "Authorization:token $access_token" -H "Content-Type:application/octet-stream" --data-binary \@$filename https://uploads.github.com/repos/UUPharmacometrics/PsN/releases/$id/assets?name=$filename
EOF

    $response = readpipe($upload_cmd);
}

chdir("doc");
my @docs = glob("*.pdf");
for my $filename (@docs) {
    my $upload_cmd = <<"EOF";
curl -# -XPOST -H "Authorization:token $access_token" -H "Content-Type:application/octet-stream" --data-binary \@$filename https://uploads.github.com/repos/UUPharmacometrics/PsN/releases/$id/assets?name=$filename
EOF

    $response = readpipe($upload_cmd);
}
