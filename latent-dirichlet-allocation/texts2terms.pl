#!/usr/bin/perl
use strict;
use warnings;

my @files = glob "~/Sync/Data/cybergeo-articles-1996-2015/texts/*text*";

open my $out, '>:encoding(utf8)', 'terms.csv' or die;
open my $s, '>:encoding(utf8)', 'sentences.csv' or die;

print $out "id;term;count\n";
print $s "id|sentence\n";

for my $file (@files) {
  warn "Parsing $file\n";
  my ($id) = $file =~ /(\d+)_/;
  warn "Found id $id\n";
  open my $fh, '<:encoding(utf8)', $file or die "No such file $file";
  my %terms;
  my $nb;
  while(<$fh>) {
    chomp;
    my @terms = grep { /[^_]/ } grep { /[A-Za-z]/ } map { s/\d+$//; $_ } /([\w-]+)/ug;
    my @sentences = split /\./;
    for (@terms) {
      next if /^\d+$/;
      next unless $_;
      ++$terms{$_};
      ++$nb;
    }
    for (@sentences) {
      next unless $_;
      s/^\W*//;
      s/\|/:/g;
      printf $s "%d|%s\n", $id, $_;
    }
  }
  warn "$nb terms found\n" if $nb;
  close $fh;
  printf $out "%d;%s;%d\n", $id, $_, $terms{$_} for keys %terms;
}

close $out;
close $s;
