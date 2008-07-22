#/usr/bin/perl
#
# rosters-to-classmap -- form classmapping file from set of roster files
#
# usage: rosters-to-classmap.pl classid1-roster.txt classid2-roster.txt ...
#
# Roster filenames must be specified on the command line, can't read from a 
# pipe or redirected input.
#
# Named roster files are as copied and pasted from OLI class roster pages. 
# The fields may be space-delimited as when copied and pasted into a plain 
# text file. Or they may have been converted to tab-delimited format. The
# userid is found as the second-to-last column when splitting on spaces,
# so there may or may not be an initial empty field corresponding to the 
# check-box column in certain versions of the OLI roster display.
#
# Writes a set of lines of the form
#      userid\tclassid
# where classid is the roster filename with "-roster.txt" deleted
#
while (<>)
{
	@fields = split('[\s]');
	# userid should be second to last, no matter how many
	$userid = $fields[ @fields - 2];
	$class = $ARGV;
	$class =~ s/-roster.txt//;
        if ($userid) {
		print "$userid\t$class\n";
	}
}
