#!/usr/bin/perl
#
# mapid --  map list of student ids in input file using external 
#           tab-delimited mapping file, generating new id if not found
#           and updating 
#
# Usage:   mapid.pl mapfile.txt [idfile.txt]
# Reads id list from second argument, standard input if none. Writes
# anonymized list to stdout, updating mapfile with newly generated
# mappings if any. Also writes newly generated pairs to stderr.
#
# Mapfile is tab-delimited list of canonical-name anon-id pairs. It
# should contain a special entry for "PREFIX" specifying the prefix
# to use when generating ids. 
#

my $filename = $ARGV[0]; shift @ARGV;
if (! $filename) { die "usage: mapid.pl mapfile.txt [inputfile]\n"; }

# count of ids generated with this prefix
my $id_counter = 1;

# load existing mapping from specified mapping file
open MAPFILE, "<$filename" or die "Couldn't open $filename for reading: $!\n"  ;
%idmap = map /(.*)\t([^\r]*)/, <MAPFILE>; # gulp in hash; Allow DOS mode \r at eol
close MAPFILE;
if (1) { # debugging printout
	$maplength = (keys idmap);
	print STDERR "Input mapping ($maplength) entries\n";
	while( ($k, $v) = each %idmap) {
        	print STDERR "  |$k| |$v|\n";
	}
	print STDERR "End input mapping\n\n";
}
# ensure file includes prefix. Upper case ensures it can't conflict with
# any canonicalized id.
my $PREFIX_ID = "PREFIX";
($prefix = $idmap{$PREFIX_ID}) or die "missing $PREFIX_ID entry in $filename\n";

# set counter to one more than number of existing entries. Since
# map contains a dummy prefix entry, this will be number of keys
$id_counter = (keys idmap); 
#print STDERR "loaded id map, counter= $id_counter\n";

while (<>)
{
	s/\r$//;  # cygwin perl may include CR from DOS-mode text files
        # ids may have spaces, dots, or odd chars. Assume begin and
	# end with a word character.
	if (/[\w].*[\w]/) {
		# to ignore case differences, use canonical lower case
		# form in mapping table. 
		$canon_id = $id = $&;
		$canon_id =~ tr/[A-Z]/[a-z]/;
		if (! ($anonid = $idmap{$canon_id})) { 
			$anonid = $idmap{$canon_id} = $prefix . $id_counter++;
			print STDERR "$canon_id\t$anonid\n";
			$modified_map = 1;
		}
		# NB: substitute for original, not canonical, form 
		s/$id/$anonid/;	
	}
	print; 
}

# update mapping file if mapping has been extended.
if ($modified_map) {
 open MAPFILE, ">$filename" or die "Couldn't open $filename for updating: $!\n";
 while( ($k, $v) = each %idmap) {
        print MAPFILE "$k\t$v\r\n";
 }
 close MAPFILE;
}
