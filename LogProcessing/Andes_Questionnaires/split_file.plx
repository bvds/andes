#!/usr/bin/perl
#  Copyright (c) 2006 Robert G.M. Hausmann
#
# The purpose of this script is to split one email file into many files.
#
# To run this script, you need a single email file that contains all of the reponses.
# In MacOS X's mail.app, select all of the questionnaires and use "File/Save as..." 
# to save all the emails to a single file. Then use that file as input into this 
# script <file.txt>.
#
# Usage: perl split_file.plx file.txt

my $counter = 1;

# Open the file & store in an array.
open (File1, @ARGV[0]) || die ("Could not open file: $!");

$/ = '';  		# read in more whole paragraph, not just one line
while ( <File1> ) {
  $_ =~ s/\n/ /gi;
  $_ =~ s/Question_/\nQuestion_/gi;
  $_ =~ s/Open-Ended_/\nOpen-Ended_/gi;
  $_ =~ s/From:/\nFrom:/gi;
  $_ =~ s/Date:/\nDate:/gi;
  $_ =~ s/To:/\nTo:/gi;
  $_ =~ s/Subject:/\nSubject:/gi;
  $_ =~ s/Name=/\nName=/gi;
  $_ =~ s/Section=/\nSection=/gi;
  $single_line .= $_;
}

@line_array  = split (/\n/, $single_line);

foreach my $line (@line_array) {
  if ($line =~ /From:/) {
    # Create a file to store the output.
    my $file_out = "Andes Questionnaire" . $counter . ".txt";
    open(INFO, ">$file_out");
    print INFO "$line\n";
    $counter++;
  }
  else {
    print INFO "$line\n";
  }
}

print "\nDone!\n";