#!/usr/bin/perl
# Copyright (c) 2006 Robert G.M. Hausmann
# This script parses the output of assoc-count.pl into an excel readable format.
#
# usage: perl parse_assoc.plx assoc.xls

# Open the output from assoc-count.pl and build an array from it.
open (FILE, @ARGV[0]) || die ("Could not open file: $!");
while (<FILE>) { chomp; push @line, $_; }

# Open a file to store the parsed assoc.
my $file = "assoc2.xls";
open(INFO, ">>$file");

# Loop through the array and split it so that each line is 
# broken into columns in excel. 
foreach (@line) {

  # If there is a sub-type of the assoc, list it.
  if ( $_ =~ /op|error|step|parse|entry/ ) {
    ($count, $assoc, $assoc_type, $tag) = split(/\s+/, $_, 4);
    print INFO "$count\t$assoc\t$assoc_type\t$tag\n";
  } else {
    ($count, $assoc, $tag) = split(/\s+/, $_, 3);
    print INFO "$count\t$assoc\t\t$tag\n";
  }

} # Next entry (row)

# Close the file that is being written to.
close INFO;

print "\ndone!\n"