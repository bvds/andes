# assoc-countbat -- batch count assoc tags in all mid logs
# reports no. of events of each type over all logs 
#
# Run using a file (files.txt) containing the directory and name of the m* folders
# containing student logs.
# usage: perl assoc-countv2.pl files.txt

%count= ();	# event name => count. 

# comparison to sort event names by descending count
sub by_count {
    (- ($count{$a} <=> $count{$b}))
    || ($a cmp $b); # break ties by alphabetical order
}

# Open the list of files and build an array from them.
open (FILENAMES, @ARGV[0]) || die ("Could not open file: $!");
while (<FILENAMES>) { chomp; push @filenames, $_; }

# Loop through array of file names, putting their contents into one large array.
foreach (@filenames) {
  open FILES, $_;
  print "Open file: $_ ";
  @line_array = <FILES>;
  $line_count = @line_array;
  print "Number of lines: $line_count\n";

  # Loop through each line of the log file and generate counts for each variable.
  foreach (@line_array) {

    if (/DDE-COMMAND assoc/) # assoc line
    {
             ($timestamp, $DDE, $assoc, $tag) = split(/\s+/, $_, 4);
    
         # be intelligent about structured assoc entries
         if (  $tag =~ /^\(/  )  # list beginning with ( 
         {
             print "list tag: $tag\n";
            ($tag, $subtag, $rest) = split(/\s+/, $tag, 3);
    	# print "splits into $tag+$subtag+$rest\n";
    
    	# some list items are of form (IEA subtag [args]) 
    	# or (NSH subtag [args])
    	# Others just use (NSH-foo-bar [args]). 
    	if (($tag eq "(IEA") || ($tag eq "(NSH")) { 
               print "IEA or NSH found\n";
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
    } # next line in the log

  } # next log
} # end all logs

# Open a file to store the assoc.
my $file = "assoc.xls";
open(INFO, ">>$file");

# report results:
foreach $e (sort by_count keys(%count)) {
    print INFO $count{$e} . "\t" . $e;
}

close INFO;

print "\ndone!\n"