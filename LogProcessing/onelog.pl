#!/usr/bin/perl -w
#
# onelog --- analyze a concatonated sequence of Andes log files
# as retrieved from OLI database query or from stand-along Andes.
#
# Usage:     onelog.pl access.csv
#
# OLI's data extraction tool returns the logs as database records in comma 
# separated values form. Which columns are included depends on the query.
# The entire contents of each andes session log file will be included as 
# the last column in one of these database records.  The log file text itself 
# includes CRLFs delimiting lines within it.
#
# So if we asked for date and info fields from the database, the
# combined log file will start with a header column line:
#
# 	time,info
# 
# followed by record lines which look like this:
#
#    	2006-05-04 11:30:04.0,# Log of Andes session begun Thursday, May 04, 2006 11:11:46 by m094530 on 09MOORE-CJ30A
# 	# Version 1
# 	# Help-flags 01f Procedural Conceptual Example
# 	0:00	Andes-Version 10.1.3
# 	0:00	FBD-Version 04 05 06^M
#         ... rest of log1 ...
# 	18:19	END-LOG
#	
# 	2006-05-04 11:31:34.0,# Log of Andes session begun Thursday, May 04, 2006 11:30:13 by m094530 on 09MOORE-CJ30A
#         ... rest of log2 ...
#        4:40 END-LOG
#

#
#  Impose cutoffs on accepted data points.
#
$score_cut_off=0;  # minimum score to count a problem as attempted
# doesn't contribute
$minimum_problem_attempts=0.5;   # cutoff on list of students
# there is a significant number of problems (not assigned)
# that were only attempted by a few students.  Including these
# might introduce a selection bias.
$minimum_student_attempts=0.5;  # cutoff on list of problems

# Had to install this on FC6.
# On OS X, run existing cpan and install DateTime::Format::Strptime
use DateTime::Format::Strptime;
my $dateformat1 = DateTime::Format::Strptime->new( pattern => '%F %T' );
my $dateformat2 = DateTime::Format::Strptime->new( pattern => '%Y/%m/%d %T' );
my $dateformat3 = DateTime::Format::Strptime->new( pattern => '%B %d, %Y %T' );

# parse the time stamp at the beginning of a line
sub timestamp {
  if ($_[0] =~ /^(\d+):(\d+):(\d+)$/) {
    return $1*3600+$2*60+$3;
  } elsif($_[0] =~ /^(\d+):(\d+)$/) {
    return  $1*60+$2;
  }
  die "bad time $_[0]";
}

# read in file with meta-operators
# the format of the file is that each line begins with the meta-operator 
# and then all regular operators that it contains 
use Getopt::Long;
&GetOptions("meta=s" => \$meta_operator_file);
if ($meta_operator_file) {
    open(META,$meta_operator_file) or die "Can't open $meta_operators.\n";
    while (chomp($line=<META>)) {
	my @normals = reverse split / /,$line;
        my $meta = pop @normals;
	foreach $normal (@normals) {
	    push @{$meta_operators{$normal}}, $meta;
	}
    }
    close(META);
}

my $this_header="";  #first line of the most recent Andes session.
my $date;
my $last_date=DateTime->new(year=>1000); 
my $last_file = "";
my $week;

while (<>) { # loop over andes files/sessions

    # new file, only demand sorting file-wise
    if($ARGV ne $last_file) {
      $last_date=DateTime->new(year=>1000);
      $last_file = $ARGV;
    }

    # Use timestamp supplied by OLI, if possible
    if (/^(.*)# Log of Andes session begun \w+, (.+) by /) { 
      $this_header = $_;
      if ($1) {
	my $tmp=$1; 
	chop $tmp; # remove trailing comma
	$date = ($dateformat1->parse_datetime($tmp)
	        or $dateformat2->parse_datetime($tmp));
      } else {
	$date=$dateformat3->parse_datetime($2);
        warn "Using local computer time $date for timestamp";
      } 
      warn "$this_header    Can't parse date $1" unless $date;
      $week = $date->strftime("%U") if $date;  # week
      # some fall semester logs had spring activity
      $week =~ /^0[0123456]/ and 0 and warn 
	"$this_header     Off-season for $week $date\n";

      # Test that sessions have been sorted by date
      # use sort-by-time.pl to create a sorted version of the log file.
      die "Sessions in log file are not sorted in $ARGV" 
	if $date < $last_date;
      $last_date = $date;
    } 

    if (/read-student-info .(\w+)/) {
      $student = $1;  # session label should start with student id
      $student =~ s/^m(\d+)$/$1/;  # remove leading m if it is a number
    }
    elsif (/set-session-id .(\w+)/) {
      $session_userid = $1;
      $session_userid =~ s/^m(\d+)$/$1/; 
    } 

    # new problem now started.
    # in previous versions, started earlier.  This will affect the 
    # pause histogram and the decrease the total time slightly.
    next unless /read-problem-info .(\w+)/;
    $problem = $1;
    $problem =~ tr/A-Z/a-z/;  #set to lower case
    
    if ($student ne $session_userid) {
	warn "warning: session label $session_userid doesn't match $student\n";
    }
    
    my $dt=0;
    my $score=0;
    my $loss_of_focus=0;  #accumulate pauses associated with loss of focus
    my $intervening_errors=0;
    my @error_interp=();
    my $intervening_hints=0;
    my $adjusted_time=0;
    my $last_adjusted_time=0;
    my $check_entries=0;
    my $time_stamp="";
    my $this_time=0;
    my $first_hint_flag=1;
    my $first_action_flag=1;
    
    /^([0-9:]+)\t/ or die "no time stamp for $_";
    my $last_time_stamp=$1;      
    my $last_time=timestamp($1); #time (seconds) of most recent line
    my $start_time=$last_time; 
    my $last_assist_time=0;  # bracket instances of receiving "assistance"
    
    while (<>) {   # loop over lines within an Andes problem
        # don't confuse with close-lesson
        # can't use END-LOG because score is reset first
	last if /\tClose[ \r]/;  # end of Andes problem

	die "Unexpected END-LOG" if /\tEND-LOG[ \r]/;
	
	# skip evaluation of any pre-defined quantities/equations
	if (/\tCheck-Entries (\d)/) {$check_entries=$1;}
	next if $check_entries;
	
	if(/^([0-9:]+)\t/) {
	    $time_stamp = $1;	    
	    $this_time = timestamp($1);  # total time in seconds
	    my $dt = $this_time - $last_time;
	    if($dt<0 or $dt > 10*3600) {
		warn "$this_header     Times $this_time $last_time, skip rest";
		$this_time = $last_time;  # discard new (bad) time.
		last;
	    }
            # sometimes we measure time with respect to $start_time
	    $last_time = $this_time;
	    
	    # in pause histogram, might ignore pauses associated with 
	    # loss of focus:      "if" or "unless" or "if 1 and"
	    $dt_histogram{$dt}++ if /\tApp-activate/ or /\tApp-deactivate/;
	    if (/\tApp-activate/ or /\tApp-deactivate/) {
	      $loss_of_focus += $dt;
	    }
	} else {
	  warn "$this_header     Time stamp missing for $_";
	  next;
	}

	# histogram time of first hint or first action in session
	if($first_hint_flag && /\tHelp-Hint /){
	    $first_hint{$this_time-$loss_of_focus}++;
	    $first_hint_flag=0;
	}
	if($first_action_flag && (/\tSelect-tool / || /\tSelect-Variable /
	   || /\tVariable-Menu / || /\tNew-Variable /)){
	    $first_action{$this_time-$loss_of_focus}++;
	    $first_action_flag=0;
	}
	
	next unless /\tDDE/;  # skip non DDE lines
	# reset operator list and record time
	if (/\tDDE /) { # student action sent to help system
	  $#step_list = -1;
	  $#operator_list = -1;
	  $adjusted_time = $this_time - $loss_of_focus - $start_time;
	  die "$this_header     Negative adjusted time" 
	    if $adjusted_time < $last_adjusted_time;
	  $error_name = 0;  #delete any old error names
	}
	if (/\tDDE-COMMAND set-score (\d+)/) {
	    $score = $1; 
	}
	# use ^\r so we don't include CR in names
	elsif (/\tDDE-COMMAND assoc step ([^\r]+)/) {
	    @step_list = split /,/,$1;
	}
	elsif (/\tDDE-COMMAND assoc op ([^\r]+)/) {
	    @operator_list = split /,/,$1;
	}
	elsif (/\tDDE-COMMAND assoc error ([^\r]+)/) {
	    $error_name = $1;
	}
        # instances of Bottom-out hints:  student is given explicit step
	# at end of a hint sequence
	elsif (/\tDDE-COMMAND assoc \(\w+ BOTTOM-OUT /) {
	  ($student and $week) or 
	    die "$this_header   has student=$student week=$week";
	  $assistance{$student}{$week}{'bottom'} += 1;
	}
	# This way of doing things does not address the case where 
	# a student tries one thing, fails, and then does (successfully)
	# something else.
	# Sometimes Andes has a guess for the associated operator, but
	# it is not very reliable.
	elsif (/\tDDE-RESULT \|NIL\|/) {
	    $intervening_errors++;
	    $assistance{$student}{$week}{'hint'} += 1;
	    $confusion_time{$student}{$week} += $adjusted_time-$last_assist_time
	      if $last_assist_time;
	    $last_assist_time=$adjusted_time;
	    push @error_interp,  ($error_name or "assoc-error");
	}
	elsif (/\tDDE-RESULT \|!show-hint .*\|/) {
	    $intervening_hints++;
	    $assistance{$student}{$week}{'hint'} += 1;
	    $confusion_time{$student}{$week} += $adjusted_time-$last_assist_time
	      if $last_assist_time;
	    $last_assist_time=$adjusted_time;
	}
	# This condition arises from either not expressing a vector
	# equation in terms of its components or from combining equations
	# pre-maturely.  Many students go and do something else, some
	# ignore the hint, and a few try to fix the equation. 
	#
	# Even though the equation turns green, we will treat it 
	# as an error since the student is not given full credit for 
	# the entry.  We will ignore the guesses Andes gives
	# for the operator because the list is often too large
	elsif (1 and /\tDDE-RESULT \|T!show-hint .*\|/) {
	    $intervening_hints++;
	    $intervening_errors++;
	    $assistance{$student}{$week}{'error'} += 1;
	    $confusion_time{$student}{$week} += $adjusted_time-$last_assist_time
	      if $last_assist_time;
	    $last_assist_time=$adjusted_time;
	    push @error_interp, ($error_name or "assoc-error-t-show-hint");
	}
	# error with unsolicited hint.  
	# It is not clear how to count this
	elsif (/\tDDE-RESULT \|NIL!show-hint .*\|/) {
	    $intervening_hints++;
	    $intervening_errors++;
	    $assistance{$student}{$week}{'error'} += 1;
	    $confusion_time{$student}{$week} += $adjusted_time-$last_assist_time
	      if $last_assist_time;
	    $last_assist_time=$adjusted_time;
	    push @error_interp, ($error_name or "assoc-error-nil-show-hint");
	}
	# Helpsystem returns a "green" for this entry, now we record 
	# parameters that have been collected
	elsif (/\tDDE-RESULT \|T\|/ or 
	       (0 and /\tDDE-RESULT \|T!show-hint .*\|/)) { 
	    if (/\tDDE-RESULT \|T!show-hint .*\|/) {
		$intervening_hints++;
		$intervening_errors++;
		push @error_interp, ($error_name or "assoc-error-t-show-hint");
	    }
	    # confusion_time is time bracketed on both sides by assistance
	    $last_assist_time=0;  

	    my $facts={errors => $intervening_errors,
		       error_names => [@error_interp],
		       hints => $intervening_hints,
		       time => $adjusted_time-$last_adjusted_time,
		       problem => $problem};

	    # Sanity test:  these should have the same length
	    @operator_list == @step_list or
		warn "assoc op and assoc step don't match\n";
    
	    my $new_step = 0; #see if entry involves any new part of solution
	    while (@operator_list) {
		my $operator = pop @operator_list;
		my $step = pop @step_list;
		# skip implicit eqn's unless associated operator is 
		# WRITE-IMPLICIT-EQN.  In a few cases, it is the only
		# operator (and is redundant with a previous step) in
		# whic case, we associate it with that operator
		next if @step_list > 1 and $step =~ /^\(IMPLICIT-EQN / 
		    and $operator !~ /^WRITE-IMPLICIT-EQN/;

		$new_step = 1 unless 
		    $op_inst{$student}{$problem}{$operator}{$step};
		$op_inst{$student}{$problem}{$operator}{$step} += 1;
		push @{$mastery{$operator}{$student}}, $facts;
		foreach $meta_op (@{$meta_operators{$operator}}) {
		    push @{$mastery{$meta_op}{$student}}, $facts;
		}
		# for print out list of locations in file
		push @{$location{$operator}{$student}},
		"\t\t$last_time_stamp\t$time_stamp\t$intervening_errors $intervening_hints\n";
	    }
	    # only want to count entry if it adds something to solution
	    # (we don't count steps entered a second time)
            $assistance{$student}{$week}{'correct'} += 1 if $new_step;
	    $correct_entries{$student}{$problem} += 1 if $new_step;
	    $intervening_errors=0;
	    $intervening_hints=0;
	    $last_adjusted_time = $adjusted_time;
	    $last_time_stamp = $time_stamp;
	    @error_interp=();
	}	    
    }

    unless ($_) {
	warn "End of file encountered during Andes session\n";
	last;
    }

    # Can't do too much analysis here since a problem might
    # be solved over multiple sessions.
    # Accumulate time used, throwing out time where Andes is not in focus.
    $times{$student}{$problem} += $adjusted_time; 
    $scores{$student}{$problem} = $score;
    # problems attempted. Don't bother counting here because
    # a problem might be solved over multiple sessions.
    if($score > $score_cut_off) {$problems{$problem}=1}; 
    # This will be for finding usage as a function of time.
    # We collapse over problems, assuming that a session is less than a day
    $usage{$student}{$week}{'raw'} += $this_time-$start_time;
    warn "$this_header   Raw usage huge for $_" 
	if $usage{$student}{$week}{'raw'} > 7*24*3600;
    $usage{$student}{$week}{'adjusted'} += $adjusted_time;
    my $week_day = $date->strftime("%U,%w");  #week, day of week
    $total_usage{$week_day}{'raw'} += $this_time-$start_time;
    $total_usage{$week_day}{'adjusted'} += $adjusted_time;
}


#############################################################################
#
#                     Analysis of accumulated data
#
#############################################################################


# print out score histogram in Mathematica notation
# This is not affected by any of the cutoffs
if (0) {
    foreach $student (keys %times) {
	foreach $problem (keys %{$times{$student}}) {
	    $score_histogram{$scores{$student}{$problem}}++;
	}
    }
    print "\nscorehistogram={";
    foreach $score (sort {$a <=> $b} (keys %score_histogram)) {
	print "{$score,$score_histogram{$score}}",$score<100?",":"";
    }
    print "};\n";
}    

# print out histogram of first hint and first action in Mathematica notation
# This is not affected by any of the cutoffs
if (0) {
    print "\nfirstHintHistogram={";
    my $count=0;
    foreach $time (sort {$a <=> $b} (keys %first_hint)) {
      if ($count++) {print ",";}
	print "{$time,$first_hint{$time}}";
    }
    print "};\n";
    print "\nfirstActionHistogram={";
    $count=0;
    foreach $time (sort {$a <=> $b} (keys %first_action)) {
      if ($count++) {print ",";}
	print "{$time,$first_action{$time}}";
    }
    print "};\n";
}    
 
# printout histogram of pauses
# This is not affected by any of the cutoffs

if (0) {
    print "pausehistogram={";
    my $sep="";
    foreach $delay (sort {$a <=> $b} (keys %dt_histogram)) {
	print $sep,"{$delay,$dt_histogram{$delay}}";
	$sep=",";
    }
    print "};\n";
    
    # make a log-log histogram of the pauses
    my $step = 10;  #number of steps per decade
    foreach $delay (keys %dt_histogram) {
	next if $delay<1;
	$log_dt_hist{int (0.5 + log($delay)*$step/log(10.0))} += 
	    $dt_histogram{$delay};
    }
    # find bin centers and renormalize to compensate for bin widths
    foreach $i (keys %log_dt_hist) {
	$log_dt_bin{$i}=exp($i*log(10.0)/$step);
	# calculate the number of bins that have been merged.
	$log_dt_hist{$i} /= int(exp(($i+0.5)*log(10.0)/$step))-
	    int(exp(($i-0.5)*log(10.0)/$step));
    }
    # print out log binned pause histogram in Mathematica notation
    print "\nlogpausehistogram={";
    my $count=0;
    foreach $i (sort {$a <=> $b} (keys %log_dt_hist)) {
        if ($count++) { print ",";}
	print "{$log_dt_bin{$i},$log_dt_hist{$i}}";
    }
    print "};\n";
}

# Print out time usage by student and day, both adjusted and raw time

if (0) {
  local $"=",";  # for Mathematica formatted lists
  my @student_quoted = map {"\"" . $_ . "\""} (sort keys %usage);
  print "students={@student_quoted};\n";
  foreach $student (keys %usage) {
    print "usage[\"$student\"]={";
    my $count=0;
    foreach $day (sort keys %{$usage{$student}}) {
      if ($count++) {print ",";}
      print "{$day,$usage{$student}{$day}{'raw'}}";
    }
    print "};\n";
  }
  print "totalusage={";
  my $count=0;
  foreach $day (sort keys %total_usage) {
      if ($count++) {print ",";}
      print "{$day,$total_usage{$day}{'raw'}}";
    }
  print "};\n";
  foreach $student (sort keys %usage) {
    print "adjustedusage[\"$student\"]={";
    my $count=0;
    foreach $day (sort keys %{$usage{$student}}) {
      if ($count++) {print ",";}
      print "{$day,$usage{$student}{$day}{'adjusted'}}";
    }
    print "};\n";
  }
  print "adjustedtotalusage={";
  $count=0;
  foreach $day (sort keys %total_usage) {
    if ($count++) {print ",";}
    print "{$day,$total_usage{$day}{'adjusted'}}";
  }
  print "};\n";
}

# Print out time and fraction of time "spent in frustration" (time intervals
# bracketed on both sides by assistance (hints or errors)) along with 
# total assistance score.

if (0) {
  print "(* format:  {week, confusion time (time bracketed by assistance),\n";
  print "incorrect entries, hints, bottom-out hints, correct entries} *)\n";
  foreach $student (keys %usage) {
    print "assistance[\"$student\"]={";
    my $count=0;
    foreach $day (sort keys %{$usage{$student}}) {
      # only record if they did something.
      next unless $usage{$student}{$day}; 
      if ($count++) {print ",";}
      my $time = ($confusion_time{$student}{$day} or 0);
      my $error = ($assistance{$student}{$day}{'error'} or 0);
      my $hint = ($assistance{$student}{$day}{'hint'} or 0);
      my $bottom = ($assistance{$student}{$day}{'bottom'} or 0);
      my $correct = ($assistance{$student}{$day}{'correct'} or 0);
      print "{$day,$time,$error,$hint,$bottom,$correct}";
    }
    print "};\n";
  }
}

#    
#  Remove students that solved only a few problems.
#  This should be done semester-wise
#
foreach $student (sort keys %times) {
    $i=0;
    foreach $problem (sort keys %problems) {
	 if ($times{$student}{$problem} and 
	     $scores{$student}{$problem} > $score_cut_off) {$i++;}
     }
    $problem_attempts{$i}++;
    $total_problem_attempts+= $i;
    if($i<=$minimum_problem_attempts) {delete $times{$student};}
}
#
#  Print number of students that attempted to solve a given number of problems.
#
if (1) {
  my $count_students=scalar(keys %times);
  my $mean_problems = $total_problem_attempts/$count_students;
  print "students=$count_students; avgproblems=$mean_problems\n";
    print "problemattempts={";
    my $count=0;
    foreach $i (sort {$a <=> $b} keys %problem_attempts) {
        if ($count++) {print ",";}
	print "{$i,$problem_attempts{$i}}";}
    print "};\n";
}
#
#     Remove problems that were solved by only a few students
#  This should be done semester-wise
foreach $problem (sort keys %problems) {
    $i=0;
    foreach $student (sort keys %times) {
	 if ($times{$student}{$problem} and 
	     $scores{$student}{$problem} > $score_cut_off) {$i++;}
     }
    $student_attempts{$i}++;
    if($i<=$minimum_student_attempts) {delete $problems{$problem};}
}

#
#  Print number of problems solved by a given number of students
#
if (0) {
    print "studentattempts={";
    my $count=0;
    foreach $i (sort {$a <=> $b} keys %student_attempts) {
        if ($count++) { print ",";}
	print "{$i,$student_attempts{$i}}";}
    print "};\n";
}
#
#  Print out timestamps for work leading to a given operator
#
if (0) {
    print "List of instances of each operator, showing:\n";
    print "operator\n\tstudent\n\t\t start\tend\terrors hints\n";
    foreach $operator (sort keys %mastery) {
	print "$operator\n";
	foreach $student (sort {$a cmp $b} (keys %{$location{$operator}})) {
	    print "\t$student\n@{$location{$operator}{$student}}";
	}
    }
    print "\n";
}

#
#                        Problem times
#
# print out problem time matrix in CSV format
#
# I used:
#   onelog.pl Fall2005-treacy.dat Fall2005-wintersgill.dat > times-Fall2005.csv
#   onelog.pl Spring2006-treacy.dat Spring2006-wintersgill.dat > times-Spring2006.csv
#
if (0) {
    print " ";
    foreach $problem (sort keys %problems) {print ",$problem";}
    print "\n";
    foreach $student (sort keys %times) {
	print "$student";
	foreach $problem (sort keys %problems) {
	    if ($times{$student}{$problem} and 
		$scores{$student}{$problem} > $score_cut_off) {
                # could print out times, scores, and correct entries
		0 && print ",$times{$student}{$problem}";
		1 && print ",$scores{$student}{$problem}";
		0 && print ",$correct_entries{$student}{$problem}";
	    } else {
		print ",";
	    } 
	}
	print "\n";
    }
}

# print out median time to solve and median score for each problem
# trying to output to spreadsheet did not work because there were too
# many columns.

if (0) {
    foreach $problem (sort keys %problems) {
      my @timelist=();
      my @scorelist=();
      foreach $student (sort keys %times) {
	if ($times{$student}{$problem} and 
	    $scores{$student}{$problem} > $score_cut_off) {
	  push @timelist, $times{$student}{$problem};
	  push @scorelist, $scores{$student}{$problem};
	}
      }
      @timelist = sort { $a <=> $b } @timelist;
      @scorelist = sort { $a <=> $b } @scorelist;
      my $mediantime=($timelist[int(@timelist/2)]+
		      $timelist[int((@timelist-1)/2)])/2;
      my $medianscore=($scorelist[int(@scorelist/2)]+
		       $scorelist[int((@scorelist-1)/2)])/2;
      print "($problem $mediantime $medianscore)\n";
    }
}

# Print out time, errors, hints for each application of a principle.

if (0) {
    local $"=",";  # for Mathematica formatted lists
    print "(* In the following, FNA (first no assistance) means that\n",
    "the student has, for the first time, successfully applied\n",
    "the principle without any assistance (hints given or errors made\n", 
    "since the last successful entry. *)\n";
    my @op_quoted = map {"\"" . $_ . "\""} (sort keys %mastery);
    print "operators={@op_quoted};\n";
    foreach $operator (sort keys %mastery) {
	print "(* $operator  *)\n";  # print as mathematica comment
	@op_time=();
	@op_hints=();
	@op_errors=();
	@op_error_free=();
	@op_students=();
	@op_problems=();
	@op_error_names=();
	$nevermastery=0;
	%first_mastery_attempts=();
	@first_mastery_times=();
	# using %times includes any cutoff in students
	foreach $student (keys %times) {
	    next unless $mastery{$operator}{$student};
	    
	    # Debug print of raw data
	    if(0) {
		print "  $student\n";
		for $application (@{$mastery{$operator}{$student}}) {
		    print "    @$application\n";
		}
	    }
	    
	    $previous_error_free=0;
	    $total_time_spent=0;
	    for ($i=0; $i<@{$mastery{$operator}{$student}}; $i++) {
                # Include any cutoff on set of allowed problems.  However,
		# removing certain problems can cause a "hole" in the arrays.
		# next unless $problems{$mastery{$operator}{$student}[$i]{problem}};
		#push @{$op_problems[$i]}, $mastery{$operator}{$student}[$i]{problem};
		$op_problems[$i]{$mastery{$operator}{$student}[$i]{problem}}++;
		$op_errors[$i]+=$mastery{$operator}{$student}[$i]{errors};
		if (@{$mastery{$operator}{$student}[$i]{error_names}}) {
		    foreach $err 
			(@{$mastery{$operator}{$student}[$i]{error_names}}) {
			$op_error_names[$i]{$err}++;
		    }
		}
		$op_hints[$i]+=$mastery{$operator}{$student}[$i]{hints};
		# make sure there are no "holes" in the array
		$op_error_free[$i] or $op_error_free[$i]=0;
		$total_time_spent += $mastery{$operator}{$student}[$i]{time};
		if($mastery{$operator}{$student}[$i]{errors} == 0 and
		   $mastery{$operator}{$student}[$i]{hints} == 0) {
		    $op_error_free[$i]++;
		    unless ($previous_error_free) {
		      $first_mastery_attempts{$i}++;
		      push @first_mastery_times, $total_time_spent;
		      $previous_error_free=1;
		    }
		} 
		$op_time[$i] += $mastery{$operator}{$student}[$i]{time};
		$op_students[$i]++;
	    }
	    unless ($previous_error_free) {
	      $nevermastery++; # student never gets it right
	    }
	}
	for ($i=0; $i<@op_students; $i++) {
	    $op_errors[$i] /= $op_students[$i];
	    $op_hints[$i] /= $op_students[$i];
	    $op_error_free[$i] /= $op_students[$i]; 
	    $op_time[$i] /= $op_students[$i]; 
	}

	$op_arg="\"" . $operator . "\"";
	print " problemlist[$op_arg]={";
	for($count=0; $count<@op_problems; $count++) {
	    if ($count) {print ",";}
	    print "{";
	    my $count2=0;
	    foreach $prob (sort keys %{$op_problems[$count]}) {
		if($count2++) {print ",";}
		print "{\"$prob\",$op_problems[$count]{$prob}}";
	    }
	    print "}";
	}
	print "};\n";

	print " errornames[$op_arg]={";
	for ($count=0; $count<@op_error_names; $count++) {
	    if ($count) {print ",";}
	    print "{";
	    my $count2=0;
	    foreach $error_name (sort keys %{$op_error_names[$count]}) {
		if($count2++) {print ",";}
		print "{\"$error_name\",$op_error_names[$count]{$error_name}}";
	    }
	    print "}";
	}
	print "};\n";

	print " avgerrors[$op_arg]={@op_errors};\n";
	print " avghints[$op_arg]={@op_hints};\n";
	print " avgtime[$op_arg]={@op_time};\n";
	# fraction of students who completed this step without using
	# hints or making errors
	print " noassistance[$op_arg]={@op_error_free};\n";
	print " numberstudents[$op_arg]={@op_students};\n";
	# print out histogram for first no assistance
	print " neverFNA[$op_arg]=$nevermastery;\n";
	print " attemptsbeforeFNA[$op_arg]={";
	my $count=0;
	foreach $attempt (sort {$a <=> $b} (keys %first_mastery_attempts)) {
	  if ($count++) {print ",";}
	  print "{$attempt,$first_mastery_attempts{$attempt}}";
	}
	print "};\n";
	# Map[{Mean[N[timebeforeFNA[#]]],#}&,operators]
	print " timebeforeFNA[$op_arg]={@first_mastery_times};\n";
    }
}

# For each problem solution, print problem times and a list of operators
# applied.  This can be used to construct a linear model.
# Include student cutoff, score cut-off, but not problem cut-off.
# Express in Mathematica format as a list containing each 
# problem solution.  Each member of the list is of the format:
#  {student, problem, time, {{op1,n1},{op2,n2}, ...}}
# where ni is the number of distinct instances of opi in that solution.
if (0) {
    local $"=",";  # for Mathematica formatted lists
    my @each_student=();
    my @each_time=();
    my @op_list=sort keys %mastery;
    my @op_quoted = map {"\"" . $_ . "\""} (sort keys %mastery);
    my %opindex=();
    my $i=1;  # Mathematica/Fortran indexing starts at one
    foreach $op (sort keys %mastery) {
	$opindex{$op}=$i++;
    }
    print "operators={@op_quoted};\n";
    print "operatorinstances={";
    my $count=0;
    my $stprob=1;  # Mathematica style indexing
    # %times includes the cut-off on the list of students
    foreach $student (sort keys %times) {
	# no cut-off on problems
	foreach $problem (sort keys %{$op_inst{$student}}) {
	    if ($op_inst{$student}{$problem} and
		$times{$student}{$problem} and
		$scores{$student}{$problem} > $score_cut_off) {
		push @each_student, $student;
		push @each_time, $times{$student}{$problem};
		if($count++){ print ",\n";}		
		print "{";
		if (1) {
		    # sparse version
		    my $count2=0;
		    foreach $op (sort keys %{$op_inst{$student}{$problem}}) {
			my $inst=scalar(keys 
					%{$op_inst{$student}{$problem}{$op}});
			if($count2++){ print ",";}
			print "{$stprob,$opindex{$op},$inst}";
		    }
		} else {
		    # dense version
		    my @inst_list = map {scalar keys 
					     %{$op_inst{$student}
					       {$problem}{$_}}} @op_list;
		    print "@inst_list";
		}
		print "}";
		$stprob++;
	    }
	}
    }
    print "};\n";
    print "eachstudent={@each_student};\n";
    print "eachtime={@each_time};\n";
}
