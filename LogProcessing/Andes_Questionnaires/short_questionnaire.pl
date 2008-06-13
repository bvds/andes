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

For each question, please select your answer on a scale of 1 to 5.  If you wish to comment on a particular question, please use the empty textboxes below each question. Your comments are encouraged and welcomed.<br><br>

<b>While you are taking this survey please try to separate your feelings as to whether you liked Andes from your assessment of how useful it was in helping you learn Physics.<br><br></b>

<OL>
<LI>Was Andes more effective for doing homework versus doing homework in a more traditional fashion and turning it in?
<table>
  <tr>
    <td width=100 align=center><b>More</b></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100 align=center><b>Less</b></td>
  </tr>
  <tr>
    <td align=center><INPUT TYPE="RADIO" NAME="Q1" VALUE="1"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q1" VALUE="2"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q1" VALUE="3"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q1" VALUE="4"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q1" VALUE="5"></td>
  </tr>
  <tr>
    <td align=center>1</td>
    <td align=center>2</td>
    <td align=center>3</td>
    <td align=center>4</td>
    <td align=center>5</td>
  </tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-1" rows=2 cols=100></textarea><br><br>

<LI>Do you think you learned more or less using Andes than if you had done the same exercises with pencil and paper?
<table>
  <tr>
    <td width=100 align=center><b>More (with Andes)</b></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100 align=center><b>Less</b></td>
  </tr>
  <tr>
    <td align=center><INPUT TYPE="RADIO" NAME="Q3" VALUE="1"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q3" VALUE="2"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q3" VALUE="3"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q3" VALUE="4"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q3" VALUE="5"></td>
  </tr>
  <tr>
    <td align=center>1</td>
    <td align=center>2</td>
    <td align=center>3</td>
    <td align=center>4</td>
    <td align=center>5</td>
  </tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-3" rows=2 cols=100></textarea><br><br>

<LI>Do you feel that the immediate feedback feature of Andes helped you complete correct solutions?
<table>
	<tr>
		<td width=100 align=center><b>Agree</b></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100 align=center><b>Disagree</b></td>
	</tr>
	<tr>
		<td align=center><INPUT TYPE="RADIO" NAME="Q5" VALUE="1"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q5" VALUE="2"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q5" VALUE="3"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q5" VALUE="4"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q5" VALUE="5"></td>
	</tr>
	<tr>
		<td align=center>1</td>
		<td align=center>2</td>
		<td align=center>3</td>
		<td align=center>4</td>
		<td align=center>5</td>
	</tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-5" rows=2 cols=100></textarea><br><br>

<LI>The solve function was helpful in doing the Andes problems.
<table>
	<tr>
		<td width=100 align=center><b>Agree</b></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100 align=center><b>Disagree</b></td>
	</tr>
	<tr>
		<td align=center><INPUT TYPE="RADIO" NAME="Q6" VALUE="1"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q6" VALUE="2"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q6" VALUE="3"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q6" VALUE="4"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q6" VALUE="5"></td>
	</tr>
	<tr>
		<td align=center>1</td>
		<td align=center>2</td>
		<td align=center>3</td>
		<td align=center>4</td>
		<td align=center>5</td>
	</tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-6" rows=2 cols=100></textarea><br><br>

<LI>What was the greatest strength of Andes?<br>
<textarea TABINDEX="1" NAME="Q24" rows=8 cols=100></textarea><br><br>

<LI>What was the greatest weakness of Andes?<br>
<textarea TABINDEX="1" NAME="Q25" rows=8 cols=100></textarea><br><br>


<LI>All $courseID students should be required to use Andes.
<table>
  <tr>
    <td width=100 align=center><b>Agree</b></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100 align=center><b>Disagree</b></td>
  </tr>
  <tr>
    <td align=center><INPUT TYPE="RADIO" NAME="Q28" VALUE="1"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q28" VALUE="2"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q28" VALUE="3"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q28" VALUE="4"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q28" VALUE="5"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q28" VALUE="NA"></td>
  </tr>
  <tr>
    <td align=center>1</td>
    <td align=center>2</td>
    <td align=center>3</td>
    <td align=center>4</td>
    <td align=center>5</td>
    <td align=center>NA</td>
  </tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-23" rows=2 cols=100></textarea><br><br>

<LI>Compare your experience with Andes with your knowledge of other learning tools used at $school like Mastering Physics, WebAssign, Blackboard, or the on-line Chemistry assignments.  Please select the tool used for comparison. (You may check more than one.)
  <OL class="withroman">
      <LI><input type="checkbox" name="Q30A">Mastering Physics
      <LI><input type="checkbox" name="Q30B">WebAssign
      <LI><input type="checkbox" name="Q30C">Blackboard
      <LI><input type="checkbox" name="Q30D">On-line Chemistry
  </OL>
<table>
  <tr>
    <td width=100><b>Andes was</b></td>
    <td width=100><b>More effective</b></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100 align=center><b>Less effective</b></td>
  </tr>
  <tr>
    <td></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q30" VALUE="1"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q30" VALUE="2"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q30" VALUE="3"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q30" VALUE="4"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q30" VALUE="5"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q30" VALUE="NA"></td>
  </tr>
  <tr>
    <td></td>
    <td align=center>1</td>
    <td align=center>2</td>
    <td align=center>3</td>
    <td align=center>4</td>
    <td align=center>5</td>
    <td align=center>NA</td>
  </tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-25" rows=2 cols=100></textarea><br><br>


<input type="submit"> 
<input type="reset"> 

</li></ol></form></body></html>

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
