#!/usr/bin/perl -w
# Copyright (c) 2006 Robert G.M. Hausmann
#
# The purpose of this script is to generate an Andes Questionnaires that is customizable to a particular school.
# Usage: perl short_questionnaire.pl

use strict;

# define contextual variables
my $school    = &promptUser("Enter the name of the school ");
my $year      = &promptUser("Enter the year ");
my $semester  = &promptUser("Enter the semester ");
my $courseID  = &promptUser("Enter the course number ");
my $name      = &promptUser("Enter the student ID name ");

#my $file = "andes_questionnaire_" . $school . "_" . $semester . "_" . "$year.html";
my $file = "index.html";

open HTML_OUT, "> $file" or die "Can't open $file : $!";

print HTML_OUT <<HTMLEND;

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
"http://www.w3.org/TR/html4/loose.dtd">
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html;charset=ISO-8859-1" >
    <STYLE type="text/css">
      OL.withroman { list-style-type: lower-alpha }
    </STYLE>
    <title>Andes Questionnaire</title>
  </head>
  <body>
    <form method="post" action="http://www.pitt.edu/htbin/cgiemail/~andes2/questionnaire_template_SP211.txt">

<INPUT TYPE=HIDDEN NAME="school" VALUE="$school" type="text">
<INPUT TYPE=HIDDEN NAME="year" VALUE="$year" type="text">
<INPUT TYPE=HIDDEN NAME="semester" VALUE="$semester" type="text">
<INPUT TYPE=HIDDEN NAME="courseID" VALUE="$courseID" type="text">

<table align=center><tr><td><b><font size="+2">ANDES QUESTIONNAIRE - $semester $year</font></b></td></tr></table><br>

$name: <input tabindex="1" name="required-name" size="10" type="text">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
Section: <input tabindex="1" name="required-section" size="10" type="text"><br><br><br>

Thank you for your participation in an evaluation of Andes. We expect to learn a great deal from your suggestions about what works and what doesn't work in the Andes system from your responses in this questionnaire.<br><br>

<OL>
<LI>Has the use of ANDES tutor helped you understand Physics principles? <br>
    Why? Or why not?<br>
<textarea TABINDEX="1" NAME="FR1" rows=2 cols=100></textarea><br><br>

<LI>Has ANDES tutor helped you in mastering problem solving techniques? <br>
    How? Or why not?<br>
<textarea TABINDEX="1" NAME="FR2" rows=2 cols=100></textarea><br><br>

<LI>Do you think you learned more or less using Andes than if you had done the same exercises on paper and handed them in?&nbsp; Explain.<br>
<textarea TABINDEX="1" NAME="FR3" rows=2 cols=100></textarea><br><br>

<li>Do you think you learned more or less using Andes than if you had done the same exercises on webassign?&nbsp; Explain.<br>
<textarea TABINDEX="1" NAME="FR4" rows=2 cols=100></textarea><br><br>

<li>Has the techniques learned in solving Andes problems helped in solving problems on other assignments, test, or quizzes?&nbsp; Explain.<br>
<textarea TABINDEX="1" NAME="FR5" rows=2 cols=100></textarea><br><br>

<LI>What was the greatest strength of Andes?<br>
<textarea TABINDEX="1" NAME="Q24" rows=8 cols=100></textarea><br><br>

<LI>What was the greatest weakness of Andes?<br>
<textarea TABINDEX="1" NAME="Q25" rows=8 cols=100></textarea><br><br>


<LI>Should all Physics students at WH be required to use Andes?<br>
<textarea TABINDEX="1" NAME="FR6" rows=2 cols=100></textarea><br><br>

<LI>Suggest some changes that would greatly improve Andes.<br>
<textarea TABINDEX="1" NAME="FR7" rows=8 cols=100></textarea><br><br>


<input type="submit"> 
<input type="reset"> 

</li></ol>

</form></body></html>

HTMLEND

close HTML_OUT;

print "$school, $year, $semester, $courseID\n";
print "\ndone!\n";

sub promptUser {
   # Adapted from: Copyright 1998 by DevDaily Interactive, Inc.  All Rights Reserved.
   (my $promptString, my $defaultValue) = @_;
   if ($defaultValue) {
      print $promptString, "[", $defaultValue, "]: ";
   } else {
      print $promptString, ": ";
   }

   $| = 1;               # force a flush after our print
   $_ = <STDIN>;         # get the input from STDIN (presumably the keyboard)
   chomp;
   if ("$defaultValue") {
      return $_ ? $_ : $defaultValue;    # return $_ if it has a value
   } else {
      return $_;
   }
}
