#Pdat.pl: processing tool for Andes midshipman output log files.
#Requires perl be installed on the system.
#Run in a parent directory that has midshipman folders (m*) in it.
#Command Line Execution: perl pdat.pl
#Windows Execution: Double click on file pdat.pl.
#Expected run time: 15 min on Pentium 3 650 MHz.

#Expects each midshipman folder to start with m, and for each folder
#to contain any number of output log files that end in .log.

#Outputs (and overwrites) 13 files in the parent directory.  Each file
#consists of lines (rows) of comma delimited values (columns) that can be
#read by a spreadsheet program like Excel.
#A description of each output file follows.
#1. problem.out: Each row is a summation of all events (column headings)
#      in all folders (and all aliases) involving that problem.
#2. problem-valid.out: Same as problem, but events only count if time 
#      to 1st correct answer is > 20 sec.
#3. problem-access.out:  Each row is a single problem access (open-close 
#      cycle).  The rows are perl-sorted by problem, then by folder, 
#      then by alias.  All events in a row occurred in the logfile 
#      indicated by the source column.
#4. alias.out:  Each row is a summation of all events done by the alias 
#      in col A in the folder in col B.  Hence, an alias may have 
#      multiple rows corresponding to work done in different folders.
#5. alias-valid.out:  Same as alias, but events only count if the time 
#      to 1st correct answer is > 20 sec.  NOTE:  The number of rows 
#      does NOT correspond to the number of rows in alias, because an 
#      alias may not do valid work in all the folders in which he opened
#      a problem.
#6. alias-folder.out: A simple listing of each alias and the number of 
#      folders that alias used.
#7. alias-folder-valid.out:  Same listing as alias-folder, except only 
#      folders where > 20sec to 1st correct answer are counted.
#8. folder.out: Each row is a summation of all events in that folder 
#      (all problems, all aliases).
#9. folder-valid.out: Same as folder, but events only count if the time 
#      to 1st correct answer is > 20 sec.
#10. folder-access.out: Each row is a single problem access (open-close 
#      cycle).  The rows are sorted by folder, then by problem, then by 
#      alias.  All events in a row occurred in the logfile indicated by 
#      the source column.
#11. folder-alias.out: A simple listing of each folder, the number of 
#      aliases using that folder, and the number of aliases doing valid 
#      (>20sec to 1st correct answer) work in that folder.
#12. comments.out:  All non-empty comments, indexed by alias and with 
#      source logfile and timestamp info.
#13.corrupt.out : Number of corrupt log files. 

$index = 0;  #index is access (problem open-close) counting variable.
$numcorrupt = 0;
$numfiles = 0;


open (outfile,">comments.out");
print outfile "Alias;; Comment;; Date;; Time Logfile Opened;; Source;; Timestamp\n";
foreach $dir(<m*>) {
   chdir $dir;
   $folder=$dir;
   $folder =~ s/m//; #remove leading m from folder name
   
   LOGFILE:
   foreach $log(<*.log>){
      open (infile,$log)
         or die "Couldn't open : $log\n";
      $secondsbefore=0;
      $checkanswer=0;
      $line=0;
      $timestamp = "0:00";
      $timeopen=0.;
      while (<infile>){ #<infile> is line by line scan until EOF
         $line++;
#Get 1st line DTG, etc. Expects something like the following:
#   "--, June 08, 2001 08:10:07 by Heath on Something"
#Parses on the commas, then interprets date, then parses
#on spaces to get year, time, and id. id is used in tag below.
         if($line == 1) { 
            ($junk, $date, $rest) = split(/, /,$_,3);
            ($month, $day) = split(/\s+/,$date,2);
            $month=substr($month,0,3);
            ($year, $realtime, $id) = split(/\s+/,$rest,3);
            substr($id, -2) = ""; #remove end of line ^M#
            $id = substr($id, 3); #remove by at front
            $date="$day-$month-$year"; #acceptable date format in Excel
         } else {
#Expects timestamp to be in HOUR:MIN:SEC or MIN:SEC format.
#Inital parsing done on spaces after timestamp and before and after 1st word.
            ($timestamp, $event, $args) = split(/\s+/,$_,3);  
         }
#read and output non-empty comments
         if((/Comment/)&&($args ne "")){ 
            $comment=$args;
            substr($comment, -2) = ""; #remove end of line ^M#
            print outfile "$alias;; $comment;; $date;; $realtime;; $dir\\$log;; $timestamp\n";
         }
#get alias info (parse args using " character)
         if(/read-student-info/){
            ($junk1, $alias, $junk2) = split(/"/,$args,3);
            $alias =~ s/,/-/g;
         }
#get new problem info, increment index variable and initialize counted stuff
         if(/read-problem-info/){
            $index=$index+1;
            $nocommalog = $log;
            $nocommalog =~ s/,/-/g;
            $nocommaid = $id;
#replaces comma in id with -
            $nocommaid =~ s/,/-/g; 
            $tag[$index]= "$dir\\$nocommalog, $date, $realtime , $nocommaid";
            $timeopen=$timestamp;
            $worktime=0;
            $wastetime=0;
            ($junk1, $problem, $junk2) = split(/"/,$args,3);
            $problist[$index]=$problem;
            $probalias[$index]=$alias;
            $probfolder[$index]=$folder;
            $probnumrights[$index]=0;
            $probnumanswers[$index]=0;
            $checkanswer=0;
            $helpwhy[$index]=0;
            $helphow[$index]=0;
            $helpexplain[$index]=0;
            $helphint[$index]=0;
            $whywrongequation[$index]=0;
            $unusualaxes[$index]=0;
            $notinterpret[$index]=0;
            $invalidwork[$index]=0;
         }
         #read hour, min, and sec for each line
         ($hour, $min, $sec) = split(/:/,$timestamp,3);
         #accounts for no hour mark during 1st hour
         if (! $sec) { 
            $sec = $min;
            $min = $hour;
            $hour = 0;
         }
         $seconds = $sec + $min*60. + $hour*60.*60.;
         if ($timeopen eq $timestamp) {
            $timeopen=$seconds;
         }

         $timediff=($seconds-$secondsbefore)/60.; # in minutes
         $secondsbefore=$seconds;
         if ($timediff < 10.) {
            $worktime = $worktime + $timediff; # in minutes
         }
         else {
            $wastetime=$wastetime + $timediff;
         }

         if(/close-problem/ ){
            $probtime[$index]=$worktime;
            $probwaste[$index]=$wastetime;
            $checkanswer=0;
         }
         if(eof){
            $probtime[$index]=$worktime;
            $probwaste[$index]=$wastetime;
            $checkanswer=0;
         }
#keys on check-answer to know if student submitted answer box.
         if(/check-answer/) {
            $checkanswer=1;
            $probnumanswers[$index]++;
         }
#If student did, checks next line for a T=true.
#If time since opening problem is < 20 secs, marks as invalid work.
#Note that this makes the whole access invalid, which may overcount invalids.
         if($checkanswer==1){
            if(/\|T\|/){
               $probnumrights[$index]=$probnumrights[$index]+1;
               $checkanswer=0;
               if(($seconds - $timeopen) < 20.0) {
                  $invalidwork[$index] = 1;
               }
            }
         }
         if(/Why-wrong-equation/) {
            $whywrongequation[$index]++;
         }
         if(/unusual choice for the axes/) {
            $unusualaxes[$index]++;
         }
         if(/could not interpret this equation/) {
            $notinterpret[$index]++;
         }
         if(/Help-Hint/) {
            $helphint[$index]++;
         }
         if(/Help-Explain/) {
            $helpexplain[$index]++;
         }
         if(/Help-How/) {
            $helphow[$index]++;
         }
         if(/Help-Why/) {
            $helpwhy[$index]++;
         }
         next LOGFILE if (($timediff<0) && !(/Comment/));
      }
   } continue {
      close(infile);
      if (($timediff<0) && !(/Comment/)) {
         $numcorrupt++;
      }
      $numfiles++;
   }
   chdir "..";
}
close(outfile);

open (outfile,">corrupt.out")
   or die "Couldn't open corrupt.out:\n";
   print outfile "$numcorrupt out of $numfiles logfiles corrupt.\n";
close(outfile);

@sortproblem = @problist[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortalias = @probalias[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorttime = @probtime[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortwaste = @probwaste[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortfolder = @probfolder[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortnumrights = @probnumrights[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortnumanswers = @probnumanswers[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortwhywrongequation = @whywrongequation[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortunusualaxes = @unusualaxes[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortnotinterpret = @notinterpret[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorthelphint = @helphint[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorthelpexplain = @helpexplain[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorthelphow = @helphow[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorthelpwhy = @helpwhy[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorttag = @tag[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortinvalidwork = @invalidwork[
   sort {
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];


open (outfile,">problem-access.out")
   or die "Couldn't open problem-access.out:\n";
$ind=1;
print  outfile "Problem, <20sec to ans if 1, Folder, Alias, Time Worked (min), Time Wasted (min), Num Right, Number Answered, Number Help-Why, Number Help-How, Number Help-Explain, Number Help-Hint, Number WhyWrongEquation, Number Unusual Axes, Number Not Interpret Equation, Source, Date Written, Time Logfile Opened, Logfile ID \n";
while($ind<=$index) {
   print  outfile "$sortproblem[$ind], $sortinvalidwork[$ind], $sortfolder[$ind], $sortalias[$ind], $sorttime[$ind], $sortwaste[$ind], $sortnumrights[$ind], $sortnumanswers[$ind], $sorthelpwhy[$ind], $sorthelphow[$ind], $sorthelpexplain[$ind], $sorthelphint[$ind], $sortwhywrongequation[$ind], $sortunusualaxes[$ind], $sortnotinterpret[$ind], $sorttag[$ind]\n";
   $ind++;
}
close(outfile);


open (outfile,">problem.out")
   or die "Couldn't open problem.out:\n";
print  outfile "Problem , Number Folders where Worked, Total Times Accessed, Total Time Spent (min), Total Time Wasted (min), Total Number Corrects, Total Number Answered, Number Help-Why, Number Help-How, Number Help-Explain, Number Help-Hint, Number WhyWrongEquation, Number Unusual Axes, Number Not Interpret Equations\n";
$ind=1;
$problem=$sortproblem[$ind];
$folder=$sortfolder[$ind];
$countaccess=0;
$countfolder=1;
$time=0;
$waste=0;
$numrights=0;
$numanswers=0;
$numhelpwhy=0;
$numhelphow=0;
$numhelpexplain=0;
$numhelphint=0;
$numwhywrongequation=0;
$numunusualaxes=0;
$numnotinterpret=0;
while($ind<=$index) {
   if(($problem) ne ($sortproblem[$ind])) {  
      print  outfile "$problem , $countfolder, $countaccess, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
      $problem=$sortproblem[$ind];
      $countaccess=0;
      $countfolder=0;
      if(($folder) eq ($sortfolder[$ind])) {
         $countfolder++;
      }
      $time=0;
      $waste=0;
      $numrights=0;
      $numanswers=0;
      $numhelpwhy=0;
      $numhelphow=0;
      $numhelpexplain=0;
      $numhelphint=0;
      $numwhywrongequation=0;
      $numunusualaxes=0;
      $numnotinterpret=0;
   }
   if(($folder) ne ($sortfolder[$ind])) {
      $countfolder++;
      $folder=$sortfolder[$ind];
   }
   $countaccess++;
   $time=$time+$sorttime[$ind];
   $waste=$waste+$sortwaste[$ind];
   $numrights=$numrights+$sortnumrights[$ind];
   $numanswers=$numanswers+$sortnumanswers[$ind];
   $numhelpwhy=$numhelpwhy + $sorthelpwhy[$ind];
   $numhelphow=$numhelphow + $sorthelphow[$ind];
   $numhelpexplain = $numhelpexplain + $sorthelpexplain[$ind];
   $numhelphint = $numhelphint + $sorthelphint[$ind];
   $numwhywrongequation = $numwhywrongequation + $sortwhywrongequation[$ind];
   $numunusualaxes = $numunusualaxes + $sortunusualaxes[$ind];
   $numnotinterpret = $numnotinterpret + $sortnotinterpret[$ind];
   if ($ind == $index) {
      print  outfile "$problem , $countfolder, $countaccess, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
   }
$ind++;
}
close(outfile);

open (outfile,">problem-valid.out")
   or die "Couldn't open problem-valid.out:\n";
print  outfile "Problem , Number Folders where Worked, Total Times Accessed, Total Time Spent (min), Total Time Wasted (min), Total Number Corrects, Total Number Answered, Number Help-Why, Number Help-How, Number Help-Explain, Number Help-Hint, Number WhyWrongEquation, Number Unusual Axes, Number Not Interpret Equations\n";
$ind=1;
$problem=$sortproblem[$ind];
$folder=$sortfolder[$ind];
$countaccess=0;
$countfolder=1;
$time=0;
$waste=0;
$numrights=0;
$numanswers=0;
$numhelpwhy=0;
$numhelphow=0;
$numhelpexplain=0;
$numhelphint=0;
$numwhywrongequation=0;
$numunusualaxes=0;
$numnotinterpret=0;
while($ind<=$index) {
   if($sortinvalidwork[$ind] == 0) {
      if(($problem) ne ($sortproblem[$ind])) {  
         print  outfile "$problem , $countfolder, $countaccess, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
         $problem=$sortproblem[$ind];
         $countaccess=0;
         $countfolder=0;
         if(($folder) eq ($sortfolder[$ind])) {
            $countfolder++;
         }
         $time=0;
         $waste=0;
         $numrights=0;
         $numanswers=0;
         $numhelpwhy=0;
         $numhelphow=0;
         $numhelpexplain=0;
         $numhelphint=0;
         $numwhywrongequation=0;
         $numunusualaxes=0;
         $numnotinterpret=0;
      }
      if(($folder) ne ($sortfolder[$ind])) {
         $countfolder++;
         $folder=$sortfolder[$ind];
      }
      $countaccess++;
      $time=$time+$sorttime[$ind];
      $waste=$waste+$sortwaste[$ind];
      $numrights=$numrights+$sortnumrights[$ind];
      $numanswers=$numanswers+$sortnumanswers[$ind];
      $numhelpwhy=$numhelpwhy + $sorthelpwhy[$ind];
      $numhelphow=$numhelphow + $sorthelphow[$ind];
      $numhelpexplain = $numhelpexplain + $sorthelpexplain[$ind];
      $numhelphint = $numhelphint + $sorthelphint[$ind];
      $numwhywrongequation = $numwhywrongequation + $sortwhywrongequation[$ind];
      $numunusualaxes = $numunusualaxes + $sortunusualaxes[$ind];
      $numnotinterpret = $numnotinterpret + $sortnotinterpret[$ind];
      if ($ind == $index) {
         print  outfile "$problem , $countfolder, $countaccess, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
      }
   }
   $ind++;
}
close(outfile);

@sortproblem = @problist[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortalias = @probalias[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorttime = @probtime[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortwaste = @probwaste[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortfolder = @probfolder[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortnumrights = @probnumrights[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortnumanswers = @probnumanswers[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortwhywrongequation = @whywrongequation[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortunusualaxes = @unusualaxes[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortnotinterpret = @notinterpret[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorthelphint = @helphint[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorthelpexplain = @helpexplain[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorthelphow = @helphow[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorthelpwhy = @helpwhy[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sorttag = @tag[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];
@sortinvalidwork = @invalidwork[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
   } 0..$index];

open (outfile,">folder-access.out")
   or die "Couldn't open folder-access.out:\n";
$ind=1;
print  outfile "Folder, <20sec to ans if 1, Problem, Alias, Time Worked (min), Time Wasted (min), Number Corrects, Number Answered, Number Help-Why, Number Help-How, Number Help-Explain, Number Help-Hint, Number WhyWrongEquation, Number Unusual Axes, Number Not Interpret Equation, Source, Date Written, Time Logfile Started, Logfile ID\n";
while($ind<=$index) {
   print  outfile "$sortfolder[$ind], $sortinvalidwork[$ind], $sortproblem[$ind], $sortalias[$ind], $sorttime[$ind], $sortwaste[$ind], $sortnumrights[$ind], $sortnumanswers[$ind], $sorthelpwhy[$ind], $sorthelphow[$ind], $sorthelpexplain[$ind], $sorthelphint[$ind], $sortwhywrongequation[$ind], $sortunusualaxes[$ind], $sortnotinterpret[$ind], $sorttag[$ind]\n";
   $ind++;
}
close(outfile);



open (outfile,">folder.out")
   or die "Couldn't open folder.out:\n";
print  outfile "Folder, Number Problems Worked , Total Times any Problem Accessed, Total Time Spent (min), Total Time Wasted (min), Total Number Corrects, Total Number Answered, Number Help-Why, Number Help-How, Number Help-Explain, Number Help-Hint, Number WhyWrongEquation, Number Unusual Axes, Number Not Interpret Equations \n";
$ind=1;
$folder=$sortfolder[$ind];
$problem=$sortproblem[$ind];
$countaccess=0;
$countproblem=1;
$time=0;
$waste=0;
$numrights=0;
$numanswers=0;
$numhelpwhy=0;
$numhelphow=0;
$numhelpexplain=0;
$numhelphint=0;
$numwhywrongequation=0;
$numunusualaxes=0;
$numnotinterpret=0;
while($ind<=$index) {
   if(($folder) ne ($sortfolder[$ind])) {
      print  outfile "$folder, $countproblem , $countaccess, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
      $folder=$sortfolder[$ind];
      $countaccess=0;
      $countproblem=0;
      if(($problem) eq ($sortproblem[$ind])) {
         $countproblem++;
      }
      $time=0;
      $waste=0;
      $numrights=0;
      $numanswers=0;
      $numhelpwhy=0;
      $numhelphow=0;
      $numhelpexplain=0;
      $numhelphint=0;
      $numwhywrongequation=0;
      $numunusualaxes=0;
      $numnotinterpret=0;
   }
   if(($problem) ne ($sortproblem[$ind])) {
      $countproblem++;
      $problem=$sortproblem[$ind];
   }
   $countaccess++;
   $time=$time+$sorttime[$ind];
   $waste=$waste+$sortwaste[$ind];
   $numrights=$numrights+$sortnumrights[$ind];
   $numanswers=$numanswers+$sortnumanswers[$ind];
   $numhelpwhy=$numhelpwhy + $sorthelpwhy[$ind];
   $numhelphow=$numhelphow + $sorthelphow[$ind];
   $numhelpexplain = $numhelpexplain + $sorthelpexplain[$ind];
   $numhelphint = $numhelphint + $sorthelphint[$ind];
   $numwhywrongequation = $numwhywrongequation + $sortwhywrongequation[$ind];
   $numunusualaxes = $numunusualaxes + $sortunusualaxes[$ind];
   $numnotinterpret = $numnotinterpret + $sortnotinterpret[$ind];
   if ($ind == $index) {
      print  outfile "$folder, $countproblem , $countaccess, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
   }
   $ind++;
}
close(outfile);

open (outfile,">folder-valid.out")
   or die "Couldn't open folder-valid.out:\n";
print  outfile "Folder, Number Problems Worked , Total Times any Problem Accessed, Total Time Spent (min), Total Time Wasted (min), Total Number Corrects, Total Number Answered, Number Help-Why, Number Help-How, Number Help-Explain, Number Help-Hint, Number WhyWrongEquation, Number Unusual Axes, Number Not Interpret Equations \n";
$ind=1;
$folder=$sortfolder[$ind];
$problem=$sortproblem[$ind];
$countaccess=0;
$countproblem=1;
$time=0;
$waste=0;
$numrights=0;
$numanswers=0;
$numhelpwhy=0;
$numhelphow=0;
$numhelpexplain=0;
$numhelphint=0;
$numwhywrongequation=0;
$numunusualaxes=0;
$numnotinterpret=0;
while($ind<=$index) {
   if($sortinvalidwork[$ind] == 0) {
      if(($folder) ne ($sortfolder[$ind])) {
         print  outfile "$folder, $countproblem , $countaccess, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
         $folder=$sortfolder[$ind];
         $countaccess=0;
         $countproblem=0;
         if(($problem) eq ($sortproblem[$ind])) {
            $countproblem++;
         }
         $time=0;
         $waste=0;
         $numrights=0;
         $numanswers=0;
         $numhelpwhy=0;
         $numhelphow=0;
         $numhelpexplain=0;
         $numhelphint=0;
         $numwhywrongequation=0;
         $numunusualaxes=0;
         $numnotinterpret=0;
      }
      if(($problem) ne ($sortproblem[$ind])) {
         $countproblem++;
         $problem=$sortproblem[$ind];
      }
      $countaccess++;
      $time=$time+$sorttime[$ind];
      $waste=$waste+$sortwaste[$ind];
      $numrights=$numrights+$sortnumrights[$ind];
      $numanswers=$numanswers+$sortnumanswers[$ind];
      $numhelpwhy=$numhelpwhy + $sorthelpwhy[$ind];
      $numhelphow=$numhelphow + $sorthelphow[$ind];
      $numhelpexplain = $numhelpexplain + $sorthelpexplain[$ind];
      $numhelphint = $numhelphint + $sorthelphint[$ind];
      $numwhywrongequation = $numwhywrongequation + $sortwhywrongequation[$ind];
      $numunusualaxes = $numunusualaxes + $sortunusualaxes[$ind];
      $numnotinterpret = $numnotinterpret + $sortnotinterpret[$ind];
      if ($ind == $index) {
         print  outfile "$folder, $countproblem , $countaccess, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
      }
   }
   $ind++;
}
close(outfile);

@sortproblem = @problist[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortalias = @probalias[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sorttime = @probtime[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortwaste = @probwaste[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortfolder = @probfolder[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortnumrights = @probnumrights[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortnumanswers = @probnumanswers[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortwhywrongequation = @whywrongequation[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortunusualaxes = @unusualaxes[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortnotinterpret = @notinterpret[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sorthelphint = @helphint[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sorthelpexplain = @helpexplain[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sorthelphow = @helphow[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sorthelpwhy = @helpwhy[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortinvalidwork = @invalidwork[
   sort {
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];

open (outfile,">folder-alias.out")
   or die "Couldn't open folder-alias.out:\n";
print  outfile "Folder, Number Aliases in Folder, Number Aliases Doing >20sec Work in Folder\n";
$ind=1;
$folder=$sortfolder[$ind];
$alias=$sortalias[$ind];
$countalias=1;
if($sortinvalidwork[$ind]==0) {
   $countaliasvalid=1;
   $aliasvalid = $alias;
} else {
   $aliasvalid = "";
   $countaliasvalid=0;
}
while($ind<=$index) {
   if(($folder) ne ($sortfolder[$ind])) {
      print  outfile "$folder, $countalias, $countaliasvalid\n";
      $folder=$sortfolder[$ind];
      $countalias=0;
      $countaliasvalid=0;
      if(($alias) eq ($sortalias[$ind])) {
         $countalias++;
      }
      if(($aliasvalid eq $sortalias[$ind]) && ($sortinvalidwork[$ind]==0)) {
         $countaliasvalid++;
      }
   }
   if(($alias) ne ($sortalias[$ind])) {
      $countalias++;
      $alias=$sortalias[$ind];
   }
   if(($aliasvalid ne $sortalias[$ind]) && ($sortinvalidwork[$ind]==0)) {
      $countaliasvalid++;
      $aliasvalid=$sortalias[$ind];
   }
   if($ind == $index) {
      print  outfile "$folder, $countalias, $countaliasvalid\n";
   }
   $ind++;
}
close(outfile);


@sortproblem = @problist[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortalias = @probalias[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sorttime = @probtime[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortwaste = @probwaste[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortfolder = @probfolder[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortnumrights = @probnumrights[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortnumanswers = @probnumanswers[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortwhywrongequation = @whywrongequation[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortunusualaxes = @unusualaxes[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortnotinterpret = @notinterpret[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sorthelphint = @helphint[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sorthelpexplain = @helpexplain[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sorthelphow = @helphow[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sorthelpwhy = @helpwhy[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];
@sortinvalidwork = @invalidwork[
   sort {
      $probalias[$a] <=> $probalias[$b]
      ||
      $probalias[$a] cmp $probalias[$b]
      ||
      $probfolder[$a] cmp $probfolder[$b]
      ||
      $problist[$a] <=> $problist[$b]
      ||
      $problist[$a] cmp $problist[$b]
   } 0..$index];

open (outfile,">alias-folder.out")
   or die "Couldn't open alias-folder.out:\n";
open (outalias,">alias.out")
   or die "Couldn't open alias.out:\n";
print  outfile "Alias, Number Folders Used by Alias\n";
print  outalias "Alias, Folder, Number Problems Worked in Folder , Total Time Spent (min), Total Time Wasted (min), Total Number Corrects, Total Number Answered, Number Help-Why, Number Help-How, Number Help-Explain, Number Help-Hint, Number WhyWrongEquation, Number Unusual Axes, Number Not Interpret Equations \n";
$ind=1;
$alias=$sortalias[$ind];
$folder=$sortfolder[$ind];
$problem=$sortproblem[$ind];
$countfolder=1;
$countprobsinfolder=1;
$time=0;
$waste=0;
$numrights=0;
$numanswers=0;
$numhelpwhy=0;
$numhelphow=0;
$numhelpexplain=0;
$numhelphint=0;
$numwhywrongequation=0;
$numunusualaxes=0;
$numnotinterpret=0;
while($ind<=$index) {
   if(($alias) ne ($sortalias[$ind])) {
      print  outfile "$alias, $countfolder\n";
      $countfolder=0;
      if(($folder) eq ($sortfolder[$ind])) {
         $countfolder++;
      }
   }
   if($folder ne $sortfolder[$ind] || $alias ne $sortalias[$ind]) {
      print outalias "$alias, $folder, $countprobsinfolder, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
      $countprobsinfolder = 0;
      $time=0;
      $waste=0;
      $numrights=0;
      $numanswers=0;
      $numhelpwhy=0;
      $numhelphow=0;
      $numhelpexplain=0;
      $numhelphint=0;
      $numwhywrongequation=0;
      $numunusualaxes=0;
      $numnotinterpret=0;
      if ($problem eq $sortproblem[$ind]) {
         $countprobsinfolder++;
      }
   }
   if($problem ne $sortproblem[$ind]) {
      $countprobsinfolder++;
      $problem = $sortproblem[$ind];
   }
   if(($folder) ne ($sortfolder[$ind])) {
      $countfolder++;
      $folder=$sortfolder[$ind];
   }
   if(($alias) ne ($sortalias[$ind])) {
      $alias=$sortalias[$ind];
   }
   $time=$time+$sorttime[$ind];
   $waste=$waste+$sortwaste[$ind];
   $numrights=$numrights+$sortnumrights[$ind];
   $numanswers=$numanswers+$sortnumanswers[$ind];
   $numhelpwhy=$numhelpwhy + $sorthelpwhy[$ind];
   $numhelphow=$numhelphow + $sorthelphow[$ind];
   $numhelpexplain = $numhelpexplain + $sorthelpexplain[$ind];
   $numhelphint = $numhelphint + $sorthelphint[$ind];
   $numwhywrongequation = $numwhywrongequation + $sortwhywrongequation[$ind];
   $numunusualaxes = $numunusualaxes + $sortunusualaxes[$ind];
   $numnotinterpret = $numnotinterpret + $sortnotinterpret[$ind];
   if( $ind == $index) {
      print  outfile "$alias, $countfolder\n";
      print outalias "$alias, $folder, $countprobsinfolder, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
   }
   $ind++;
}
close(outfile);
close(outalias);

open (outfile,">alias-valid.out")
   or die "Couldn't open alias-valid.out:\n";
open (outvalid,">alias-folder-valid.out")
   or die "Couldn't open alias-valid.out:\n";
print  outfile "Alias, Folder, Number Problems Worked in Folder , Total Time Spent (min), Total Time Wasted (min), Total Number Corrects, Total Number Answered, Number Help-Why, Number Help-How, Number Help-Explain, Number Help-Hint, Number WhyWrongEquation, Number Unusual Axes, Number Not Interpret Equations \n";
print  outvalid "Alias, Number Folders Used by Alias\n";
$ind=1;
$alias=$sortalias[$ind];
$folder=$sortfolder[$ind];
$problem=$sortproblem[$ind];
$countfolder=1;
$countprobsinfolder=1;
$time=0;
$waste=0;
$numrights=0;
$numanswers=0;
$numhelpwhy=0;
$numhelphow=0;
$numhelpexplain=0;
$numhelphint=0;
$numwhywrongequation=0;
$numunusualaxes=0;
$numnotinterpret=0;
while($ind<=$index) {
   if($sortinvalidwork[$ind]==0) {
      if(($alias) ne ($sortalias[$ind])) {
         print  outvalid "$alias, $countfolder\n";
         $countfolder=0;
         if(($folder) eq ($sortfolder[$ind])) {
            $countfolder++;
         }
      }
      if($folder ne $sortfolder[$ind] || $alias ne $sortalias[$ind]) {
         print outfile "$alias, $folder, $countprobsinfolder, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
         $countprobsinfolder = 0;
         $time=0;
         $waste=0;
         $numrights=0;
         $numanswers=0;
         $numhelpwhy=0;
         $numhelphow=0;
         $numhelpexplain=0;
         $numhelphint=0;
         $numwhywrongequation=0;
         $numunusualaxes=0;
         $numnotinterpret=0;
         if ($problem eq $sortproblem[$ind]) {
            $countprobsinfolder++;
         }
      }
      if($problem ne $sortproblem[$ind]) {
         $countprobsinfolder++;
         $problem = $sortproblem[$ind];
      }
      if(($folder) ne ($sortfolder[$ind])) {
         $countfolder++;
         $folder=$sortfolder[$ind];
      }
      if(($alias) ne ($sortalias[$ind])) {
         $alias=$sortalias[$ind];
      }
      $time=$time+$sorttime[$ind];
      $waste=$waste+$sortwaste[$ind];
      $numrights=$numrights+$sortnumrights[$ind];
      $numanswers=$numanswers+$sortnumanswers[$ind];
      $numhelpwhy=$numhelpwhy + $sorthelpwhy[$ind];
      $numhelphow=$numhelphow + $sorthelphow[$ind];
      $numhelpexplain = $numhelpexplain + $sorthelpexplain[$ind];
      $numhelphint = $numhelphint + $sorthelphint[$ind];
      $numwhywrongequation = $numwhywrongequation + $sortwhywrongequation[$ind];
      $numunusualaxes = $numunusualaxes + $sortunusualaxes[$ind];
      $numnotinterpret = $numnotinterpret + $sortnotinterpret[$ind];
      if( $ind == $index) {
         print outvalid "$alias, $countfolder\n";
         print outfile "$alias, $folder, $countprobsinfolder, $time, $waste, $numrights, $numanswers, $numhelpwhy, $numhelphow, $numhelpexplain, $numhelphint, $numwhywrongequation, $numunusualaxes, $numnotinterpret\n";
      }
   }
   $ind++;
}
close(outfile);
close(outvalid);
