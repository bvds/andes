#!/usr/bin/perl
#
#  valgrind.  No errors.
# valgrind --leak-check=yes --show-reachable=yes solver > & junk.out
#
#  (rhelp)
#  (progn (setf *debug-help* nil) (setf *ignore-errors* t) (andes-start))
# Start up helpsystem:
# printf "(rhelp)\n(setf *debug-help* nil)\n(setf *ignore-errors* t)\n(andes-start)\n" | sbcl >& run-help.log
# Read an Andes log file through the help system.
#
# run-help-from-log.pl -v [-h hostname] < input.log > output.log
#
# The default hostname is 'localhost' and -v is verbose output.
# Diff'ing the old and new log files can be used for regression testing. 
#
# Adding a test for previously memoized seems to fix the problem.
# However, there is some remaining memory leak.  Need to see if it
# is associated with the solver or with SBCL.


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

sub get_replies()		# send given Lisp cmd as DDE
  {
    my @replies=();
    while(<$sock>)
      {
	chomp();
	$debug && warn "receive: |$_|\n";
	if (/^!(.*)/)   # command from help sys
	  {
	    push @replies, "DDE-COMMAND $1";
	  }
	elsif (/^\<(\d+):(.*)/) # returned result
	  {  
	    push @replies, "DDE-RESULT |$2|";
	    last;
	  } 
	elsif (/\*(\d+):(.*)/)  # 
	  { 
	     push @replies, "DDE-FAILED";
	     last;
	   }
      }
    return @replies;
  }

while (<>) # loop over Andes sessions
  {
    my @replies;
    print; #echo header lines
    next unless /^.* Log of Andes session begun/;  
    
    while (<>) {   # loop over lines in Andes session
      # note that we don't do any thing special for START-HELP
      last if /\tEND-LOG/;  # end of Andes session
      if (/^([\d:]+)\tDDE ([^\r]+)/){
	print;
	$nCalls=($nCalls+1) % 10;      # just use one-digit id for brevity
	&Send_Msg("?$nCalls:$2");  
	@replies=&get_replies($nCalls);
      } elsif (/^([\d:]+)\tDDE-POST ([^\r]+)/) {
	#echo and send to helpsystem
	print;
	# skip any (exit-andes) message, since we don't start it up, either
	# but we don't expect a reply
	unless ($2 =~ /\(exit-andes\)/){&Send_Msg("!$2");}
      } elsif (/^([\d:]+)\tDDE-COMMAND [^\r]+(.*)/s) {
	# try to print this now, so we get the right time stamp
	if(@replies){
	  my $reply=shift @replies;
	  print "$1\t$reply$2";
	}
      } elsif (/^([\d:]+)\t(DDE-RESULT|DDE-FAILED) [^\r]+(.*)/s) {
	foreach $reply (@replies) {
	  print "$1\t$reply$3";
	}
      } else {
	print;  #just echo anything else
      }    
    } #loop over lines in session
    print; #just echo END-LOG line
  } #end loop over sessions

# finish up
close($sock);
