#!/usr/bin/perl
#
# unmapid -- map anonymized USNA mid ids in input file using 
#            external tab-delimited mapping file
#
# changes occurrences of an apparent anonymized mid id, once per line.
# If multiple user ids map to same anonymized id, which one is chosen
# is arbitrary

# load mapping from "idmap.txt" in current working directory
open MAPFILE, "<idmap.txt" or die "open: $!";
%map = map /(.*)\t(.*)\r/, <MAPFILE>;
close MAPFILE;
%idmap = reverse %map;

while (<>)
{
	s/\r$//;

	# Match any five hex-digit string
	if (/([0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F])/) {
		if ($newid = $idmap{$1}) {
			# print "changing $& to $newid\n";
			s/$&/$newid/;	
		} else {
			print STDERR "no log idmap entry for $&\n";
		}
	}
	print; 
}

