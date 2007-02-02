#!/usr/bin/perl
#
# Read an Andes log file through the help system.
#
# run-help-from-log.pl -v [-h hostname] < input.log > output.log
#
# The default hostname is 'localhost' and -v is verbose output.
# Diff'ing the old and new log files can be used for regression testing. 
#

use Getopt::Long;
&GetOptions("h=s" => \$help_host,"v" => \$debug);
 
# open client socket to help system
use IO::Socket;
my $sock = new IO::Socket::INET (
                                   PeerAddr => $help_host || 'localhost',
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
    $debug && warn "sending: |$msg|\n";
    print $sock "$msg\n" or die "Socket write failed\n";
}

sub get_reply()		# send given Lisp cmd as DDE
{
    my $timestamp = shift;  # time stamp from beginning of line
    my $end = shift;  # end of line, including newline
    # read lines from socket until we get the result
    # !!! call blocks w/no provision for timeout here
    while (<$sock>) {
      chomp();
      $debug && warn "receive: |$_|\n";
	 if (/^!(.*)/)      # command from help sys
	 {
		 print "$timestamp\tDDE-COMMAND $1$end";
	 }
	 elsif (/^\<(\d+):(.*)/) # returned result
	 {  
		 print "$timestamp\tDDE-RESULT |$2|$end";
		 return $1;
	 } 
	 elsif (/\*(\d+):(.*)/)  # 
	 { 
		 print "$timestamp\tDDE-FAILED$end";
		 return $1;
	 }
   }
}

while (<>) # loop over Andes sessions
{
  unless (/\tSTART-HELP/) {   # echo header lines
    print $_;
    next;
  }  
  print $_; # print START-HELP line
  while (<>) {   # loop over lines in Andes session
    last if /\tEND-LOG/;  # end of Andes session
    if (/^([\d:]+)\tDDE ([^\r]+)/){
      print $_;
      $nCalls=($nCalls+1) % 10;      # just use one-digit id for brevity
      &Send_Msg("?$nCalls:$2");  
    } elsif (/^([\d:]+)\tDDE-POST ([^\r]+)/) {
      print $_;
      &Send_Msg("!$2");
    } elsif (/^([\d:]+)\tDDE-COMMAND /) {
      # do nothing
    # match timestamp and end of line, including newline
    } elsif (/^([\d:]+)\tDDE-(RESULT|FAILED) [^\r]+(.*)/s) {
      &get_reply($1,$3);  #should check return value against $nCalls
    } else {
      print $_;  #just echo anything else
    }    
  } #loop over lines in session
  print $_; #just echo END-LOG line
} #end loop over sessions

# finish up
close($sock);
