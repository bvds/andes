# assoc-countbat -- batch count assoc tags in all mid logs
# reports no. of events of each type over all logs 
#
# run in root of tree with m* folders containing student logs
%count= ();	# event name => count. 

# comparison to sort event names by descending count
sub by_count {
    (- ($count{$a} <=> $count{$b}))
    || ($a cmp $b); # break ties by alphabetical order
}

# loop over each mid folder
foreach $dir (<m*>) 
{
  print STDERR "$dir\n";   	# trace progress through folders

  # loop over each log in folder
  foreach $log (<$dir/*.log>) 
  {
    open (LOG, $log) || next;
    
    # loop over each line in log
    while (<LOG>) 
    {
       if (/DDE-COMMAND assoc/) # assoc line
       {
    	     ($timestamp, $DDE, $assoc, $tag) = split(/\s+/, $_, 4);

             # be intelligent about structured assoc entries
	     if (  $tag =~ /^\(/  )  # list beginning with ( 
	     {
	        # print "list tag: $tag\n";
	        ($tag, $subtag, $rest) = split(/\s+/, $tag, 3);
		# print "splits into $tag+$subtag+$rest\n";

		# some list items are of form (IEA subtag [args]) 
		# or (NSH subtag [args])
		# Others just use (NSH-foo-bar [args]). 
		if (($tag eq "(IEA") || ($tag eq "(NSH")) { 
		   # take (IEA subtag [...]) as full tag
		   $tag = "$tag $subtag";
		   # Show ... if any omitted args
		   if ($rest) { 
		   	$tag = "$tag ...)";
		   } # else no args, rparen in $subtag when split
		} elsif ($subtag)  {
		   $tag = "$tag ...)"; 
		} 
	     }

	     # print "bumping count of $tag\n";
    	     ++$count{$tag};	
      }
    } # end of one log
    close (LOG);
  } # end of one folder
} # end all logs

# report results:
format =
@<<<<<<  @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$count{$e},  $e
.
foreach $e (sort by_count keys(%count)) {
    write;
}
