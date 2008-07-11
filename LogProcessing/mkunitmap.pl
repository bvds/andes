#!/usr/bin/perl
#
# mkunitmap -- create unit map file from .aps files
#
# Usage:   reads one or more aps files from standard input, writes 
#          tab-delimited list of problem id, problem set pairs to stdout.
#
# This can be used to generate the mapping file needed by log2xml
use File::Basename;
while (<>) 
{
	chomp;
	next if (/ANDES Problem Set/);
	next if (/\.wmv/);
	$problem = $_;
        $problem =~ tr/a-z/A-Z/;
	$problem =~ s/\r//;
	next if $problem eq "";
         ($setname,$dir,$ext) = fileparse($ARGV, qr/\..*/);
	print "$problem\t$setname\n"; 
}
