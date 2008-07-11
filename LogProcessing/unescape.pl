# copy stdin to stdout, changing CR-slash-n sequences to CR-LF
# Needed to handle logs extracted by certain database query which 
# returns entire logs as single lines, with newlines escaped, using
# a single newline char between logs:

while (<>) { # gulps a whole log as a single line with LF at end
  chomp($_);  # remove final NL separating logs
  s/\r\\n/\r\n/g;   # CR bslash n => CR LF    used at end of lines
  s/\\t/\t/g;       # bslash t => TAB 	     used after time stamps
  # following occurs in escape sequence inside some hints, e.g \l, \v, \n
  s/\\\\/\\/g;      # bslash bslash => bslash  
  print "$_";
}
