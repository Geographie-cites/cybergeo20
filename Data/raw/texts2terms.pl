#!/usr/bin/perl
use strict;
use warnings;

my @files = glob "./texts/*text*";

print "id;term;count\n";

for my $file (@files) {
  warn "Parsing $file\n";
  my ($id) = $file =~ /(\d+)/;
  warn "Found id $id\n";
  open my $fh, '<', $file or die "No such file $file";
  my %terms;
  while(<$fh>) {
    chomp;
    my @terms = split /[.,;\s!?'"(){}\[\]«»…’<>]+/;
    for (@terms) {
      next if /^\d+$/;
      next unless $_;
      ++$terms{$_} 
    }
  }
  close $fh;
  printf "$id;%s;%d\n", $_, $terms{$_} for keys %terms;
}