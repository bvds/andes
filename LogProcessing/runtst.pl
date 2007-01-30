#!/usr/bin/perl
#
# runtst -- read an ANDES test file containing a sequence of help system calls and results,
#           sending API calls over TCP connection to a help system process, creating a 
#           new file containing the current sequence for comparison w/old
#
# An Andes test file contains just the sequence of DDE call records from an ANDES
# log file, with no timestamps, as extracted by log2tst.pl
#
# reads log from stdin, writes new log to stdout.
# usage:  runtst < oldlog.tst > newlog.tst
#
# Diff'ing the two logs can be used for regression testing. 
#
# A similar process could also be used to repair omissions in information in the old logs, 
# by playing back to an enhanced version of the original help system. Would have to 
# copy non-DDE lines and preserve timestamps, though. 

use IO::Socket;

# 
# open client socket to help system
#
my $sock = new IO::Socket::INET (
                                   PeerAddr => 'localhost',
                                   PeerPort => '12345',
                                   Proto => 'tcp',
                                  );
die "Couldn't connect to help system:$!\n" unless $sock;

# We exchange newline-terminated messages of the following form:
#
#   Message form             Log entry      Direction        Notes
# ?ID:command-text\n 	    DDE          client -> server  call w/result
# <ID:result\n              DDE-RESULT   client <- server  result of cmd ID
# *ID:\n                    DDE-FAILED   client <- server  error on cmd ID
# !command-text\n           DDE-POST     client -> server  no result expected
# !command-text\n           DDE-COMMAND  client <- server  async command.
# 
# ID is an arbitrary numeric command id.  Doesn't matter if they wrap around 
# since replies should come in sequence, so no mismatching should be possible.
#
# Note DDE-FAILED can also show up in the logs when a call times out with no
# response. 

my $nCalls = 0;		# call counter for numeric cmd ids

sub Send_Msg ()    	# send given msg string over socket. Adds trailing \n
{
    my ($msg) = @_;
    print STDERR "sending: |$msg|\n";
    print $sock "$msg\n" or die "Socket write failed\n";
}

sub Do_DDE()		# send given Lisp cmd as DDE
{
    my ($lispcmd) = @_;

    # just use one-digit id for brevity
    $nCalls = ($nCalls + 1) % 10;  
    &Send_Msg("?$nCalls:$lispcmd");  

    # read lines from socket until we get the result
    # !!! call blocks w/no provision for timeout here
    while (<$sock>) {
	 chomp();
	 print STDERR "read: |$_|\n";
	 if (/^!(.*)$/)      # command from help sys
	 {
		 print "DDE-COMMAND\t$1\n";
	 }
	 elsif (/^\<[\d]+:(.*)$/) # returned result
	 {  
		 # !! verify id matches nCalls
		 print "DDE-RESULT\t|$1|\n";
		 return;
	 } 
	 elsif (/\*[\d]+:(.*)$/)  # 
	 { 
		 # !! verify id matches nCalls
		 print "DDE-FAILED\n";
		 return;
	 }
   }
}

sub Do_DDE_Post ()	# send given lisp cmd as DDE-POST
{
    my ($lispcmd) = @_;
   
    &Send_Msg("!$lispcmd");
}

while (<>) 
{
    # Our log files are DOS-mode CRLF-delimited text files, though most log processing should be able 
    # to process data even if converted to unix-mode LF-delimited text. We delete any line delimiters
    # here so we don't have to worry about their presence. Note on printing, a \n should get 
    # translated to CRLF on windows, but not if run on unix.
    chomp();	# remove trailing newline
    s/\r//;	# remove any trailing CR if present

    # echo the line
    print "$_\n";

    # ensure we have a DDE line, ignoring others (maybe blanks?)
    next if (! (/^(DDE.*)\t(.*)/));

    # get here -> we just matched a DDE line
   if ($1 eq "DDE") {
	   &Do_DDE($2);
   } elsif ($1 eq "DDE-POST") {
	   &Do_DDE_Post($2);
   }

} # end while (<>)

# finish up
close($sock);
