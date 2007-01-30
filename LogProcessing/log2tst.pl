#
# log2tst -- extract a test sequence of help system transaction records from Andes log
#
# reads log from stdin, writes test sequence to stdout
#
# Removes timestamps so output contains the sequence of the DDE* records only, suitable
# for use as a test file by runtst.pl

while (<>)
{
     # Cygwin perl seems to translate logical linefeed marker \n to DOS-mode \r\n on output
     # exactly as one would want, but doesn't seem to do the inverse translation on input.  
     # Workaround this by removing any \r's left in the line.
     s/\r//;

     # copy only DDE lines minus timestamp
     if (/^.*\t(DDE.*)/) {
	  print "$1\n";
     }
}
