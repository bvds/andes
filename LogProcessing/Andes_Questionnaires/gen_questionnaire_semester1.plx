# !/usr/bin/perl
# Copyright (c) 2006 Robert G.M. Hausmann
#
# The purpose of this script is to generate an Andes Questionnaires that is customizable to a particular school.
# Usage: perl gen_questionnaire_semester2.plx
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

<LI>The number of Andes problems assigned were
<table>
  <tr>
    <td width=100 align=center><b>Too few</b></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100 align=center><b>Too many</b></td>
  </tr>
  <tr>
    <td align=center><INPUT TYPE="RADIO" NAME="Q2" VALUE="1"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q2" VALUE="2"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q2" VALUE="3"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q2" VALUE="4"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q2" VALUE="5"></td>
  </tr>
  <tr>
    <td align=center>1</td>
    <td align=center>2</td>
    <td align=center>3</td>
    <td align=center>4</td>
    <td align=center>5</td>
  </tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-2" rows=2 cols=100></textarea><br><br>

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

<LI>Would you have done the same number of exercises correctly with pencil and paper?
<table>
  <tr>
    <td width=100 align=center><b>More (with paper)</b></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100 align=center><b>Less</b></td>
  </tr>
  <tr>
    <td align=center><INPUT TYPE="RADIO" NAME="Q4" VALUE="1"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q4" VALUE="2"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q4" VALUE="3"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q4" VALUE="4"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q4" VALUE="5"></td>
  </tr>
  <tr>
    <td align=center>1</td>
    <td align=center>2</td>
    <td align=center>3</td>
    <td align=center>4</td>
    <td align=center>5</td>
  </tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-4" rows=2 cols=100></textarea><br><br>

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

<LI>Andes would be a more effective learning tool without the solve function.

<table>
	<tr>
		<td width=100 align=center><b>Agree</b></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100 align=center><b>Disagree</b></td>
	</tr>
	<tr>
		<td align=center><INPUT TYPE="RADIO" NAME="Q7" VALUE="1"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q7" VALUE="2"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q7" VALUE="3"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q7" VALUE="4"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q7" VALUE="5"></td>
	</tr>
	<tr>
		<td align=center>1</td>
		<td align=center>2</td>
		<td align=center>3</td>
		<td align=center>4</td>
		<td align=center>5</td>
	</tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-7" rows=2 cols=100></textarea><br><br>

<LI>Rate Andes in the following two categories:
  <OL class="withroman">
    <LI>Andes helped me to understand the basic concepts of physics.
	<table>
		<tr>
			<td width=100 align=center><b>Agree</b></td>
			<td width=100></td>
			<td width=100></td>
			<td width=100></td>
			<td width=100 align=center><b>Disagree</b></td>
		</tr>
		<tr>
			<td align=center><INPUT TYPE="RADIO" NAME="Q8A" VALUE="1"></td>
			<td align=center><INPUT TYPE="RADIO" NAME="Q8A" VALUE="2"></td>
			<td align=center><INPUT TYPE="RADIO" NAME="Q8A" VALUE="3"></td>
			<td align=center><INPUT TYPE="RADIO" NAME="Q8A" VALUE="4"></td>
			<td align=center><INPUT TYPE="RADIO" NAME="Q8A" VALUE="5"></td>

		</tr>
		<tr>
			<td align=center>1</td>
			<td align=center>2</td>
			<td align=center>3</td>
			<td align=center>4</td>
			<td align=center>5</td>
		</tr>
	</table>

        <textarea TABINDEX="1" NAME="OPEN-8" rows=2 cols=95></textarea><br><br>

    <LI>Andes helped me develop more effective problem solving techniques.
	<table>
		<tr>
			<td width=100 align=center><b>Agree</b></td>
			<td width=100></td>
			<td width=100></td>
			<td width=100></td>
			<td width=100 align=center><b>Disagree</b></td>
		</tr>
		<tr>
			<td align=center><INPUT TYPE="RADIO" NAME="Q8B" VALUE="1"></td>
			<td align=center><INPUT TYPE="RADIO" NAME="Q8B" VALUE="2"></td>
			<td align=center><INPUT TYPE="RADIO" NAME="Q8B" VALUE="3"></td>
			<td align=center><INPUT TYPE="RADIO" NAME="Q8B" VALUE="4"></td>
			<td align=center><INPUT TYPE="RADIO" NAME="Q8B" VALUE="5"></td>
		</tr>
		<tr>
			<td align=center>1</td>
			<td align=center>2</td>
			<td align=center>3</td>
			<td align=center>4</td>
			<td align=center>5</td>
		</tr>
	</table>

        <textarea TABINDEX="1" NAME="OPEN-9" rows=2 cols=95></textarea><br><br>

</OL>

<LI>The techniques learned in solving Andes problems helped in solving book problems.
<table>
	<tr>
		<td width=100 align=center><b>Agree</b></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100 align=center><b>Disagree</b></td>
	</tr>
	<tr>
		<td align=center><INPUT TYPE="RADIO" NAME="Q9" VALUE="1"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q9" VALUE="2"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q9" VALUE="3"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q9" VALUE="4"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q9" VALUE="5"></td>
	</tr>
	<tr>
		<td align=center>1</td>
		<td align=center>2</td>
		<td align=center>3</td>
		<td align=center>4</td>
		<td align=center>5</td>
	</tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-10" rows=2 cols=100></textarea><br><br>

<LI>The techniques learned in solving Andes problems helped in solving test problems.
<table>
	<tr>
		<td width=100 align=center><b>Agree</b></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100 align=center><b>Disagree</b></td>
	</tr>
	<tr>
		<td align=center><INPUT TYPE="RADIO" NAME="Q10" VALUE="1"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q10" VALUE="2"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q10" VALUE="3"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q10" VALUE="4"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q10" VALUE="5"></td>
	</tr>
	<tr>
		<td align=center>1</td>
		<td align=center>2</td>
		<td align=center>3</td>
		<td align=center>4</td>
		<td align=center>5</td>
	</tr>

</table>

<textarea TABINDEX="1" NAME="OPEN-11" rows=2 cols=100></textarea><br><br>

<LI>Please evaluate the various modules in Andes.
<table border=1>
	<tr>
		<td width=200 align=center></td>
		<td width=100 align=center><b>Strong</b></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100 align=center><b>Weak</b></td>
	</tr>
	<tr>
        <td align=left width=200>a) Vectors</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11A" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11A" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11A" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11A" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11A" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11A" VALUE="NA">NA</td>
	</tr>
	<tr>
        <td align=left width=200>b) Linear Kinematics</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11B" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11B" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11B" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11B" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11B" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11B" VALUE="NA">NA</td>
	</tr>
	<tr>
        <td align=left width=200>c) Newton's Laws</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11C" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11C" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11C" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11C" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11C" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11C" VALUE="NA">NA</td>
	</tr>
	<tr>
        <td align=left width=200>d) Statics</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11D" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11D" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11D" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11D" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11D" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11D" VALUE="NA">NA</td>
	</tr>
	<tr>
        <td align=left width=200>e) Translational Dynamics</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11E" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11E" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11E" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11E" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11E" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11E" VALUE="NA">NA</td>
	</tr>
	<tr>
        <td align=left width=200>f) Dynamics + Circular Motion </td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11F" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11F" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11F" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11F" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11F" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11F" VALUE="NA">NA</td>
	</tr>
	<tr>
        <td align=left width=200>g) Work</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11G" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11G" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11G" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11G" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11G" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11G" VALUE="NA">NA</td>
	</tr>
	<tr>
        <td align=left width=200>h) Energy</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11H" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11H" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11H" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11H" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11H" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11H" VALUE="NA">NA</td>
	</tr>
	<tr>
        <td align=left width=200>i) Power</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11I" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11I" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11I" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11I" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11I" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11I" VALUE="NA">NA</td>
	</tr>
	<tr>
        <td align=left width=200>j) Linear Momentum</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11J" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11J" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11J" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11J" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11J" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11J" VALUE="NA">NA</td>
	</tr>
	<tr>
        <td align=left width=200>k) Rotational Kinematics</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11K" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11K" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11K" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11K" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11K" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11K" VALUE="NA">NA</td>
	</tr>
	<tr>
        <td align=left width=200>l) Torque </td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11L" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q11L" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11L" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11L" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11L" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q11L" VALUE="NA">NA</td>
	</tr>
</table><br>

<textarea TABINDEX="1" NAME="OPEN-12" rows=2 cols=100></textarea><br><br>

<LI>How helpful did you find the Andes tutor?  Answer NA if you did not use a particular kind of help.
<table border=1>
	<tr>
		<td width=200 align=center></td>
		<td width=100 align=center><b>Very helpful</b></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100 align=center><b>Not helpful</b></td>
	</tr>
	<tr>
                <td align=left width=200 colspan=7>a) Tutor's responses to "What's wrong with that?" queries?</td>
        </tr>
	<tr>
		<td align=center> </td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12A" VALUE="1">1</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12A" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12A" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12A" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12A" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12A" VALUE="NA">NA</td>
	</tr>
	<tr>
                <td align=left width=200 colspan=7>b) Tutor's responses to "light bulb" help?</td>
        </tr>
	<tr>
		<td align=center> </td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12B" VALUE="1">1</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12B" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12B" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12B" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12B" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12B" VALUE="NA">NA</td>
	</tr>
	<tr>
                <td align=left width=200 colspan=7>c) Tutor's response to "explain further"?</td>
        </tr>
	<tr>
		<td align=center> </td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12C" VALUE="1">1</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12C" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12C" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12C" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12C" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12C" VALUE="NA">NA</td>
	</tr>
	<tr>
                <td align=left width=200 colspan=7>d) The "Help Topics" from help menu?</td>
        </tr>
	<tr>
		<td align=center> </td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12D" VALUE="1">1</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12D" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12D" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12D" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12D" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12D" VALUE="NA">NA</td>
	</tr>
	<tr>
                <td align=left width=200 colspan=7>e) General instructions on the use of the Andes interface. </td>
        </tr>
	<tr>
		<td align=center> </td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12E" VALUE="1">1</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12E" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12E" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12E" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12E" VALUE="5">5</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q12E" VALUE="NA">NA</td>
	</tr>
</table><br>

<textarea TABINDEX="1" NAME="OPEN-13" rows=2 cols=100></textarea><br><br>

<LI>How easy/hard was it to learn how to use the various parts of Andes? 
<table border=1>
	<tr>
		<td width=200 align=center></td>
		<td width=100 align=center><b>Very Easy</b></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100 align=center><b>Very Difficult</b></td>
	</tr>
	<tr>
        <td align=left width=200>a) Writing/solving equations?</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13A" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q13A" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13A" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13A" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13A" VALUE="5">5</td>
	</tr>
	<tr>
        <td align=left width=200>b) Defining variables?</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13B" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q13B" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13B" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13B" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13B" VALUE="5">5</td>
	</tr>
	<tr>
        <td align=left width=200>c) Andes syntax?</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13C" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q13C" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13C" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13C" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13C" VALUE="5">5</td>
	</tr>
	<tr>
        <td align=left width=200>d) Line Tool?</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13D" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q13D" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13D" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13D" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q13D" VALUE="5">5</td>
	</tr>
</table><br>

<textarea TABINDEX="1" NAME="OPEN-14" rows=2 cols=100></textarea><br><br>

<LI>Andes helped to learn the use of symbolic vice numerical equations.

<table>
  <tr>
    <td width=100 align=center><b>Agree</b></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100 align=center><b>Disagree</b></td>
  </tr>
  <tr>
    <td align=center><INPUT TYPE="RADIO" NAME="Q14" VALUE="1"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q14" VALUE="2"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q14" VALUE="3"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q14" VALUE="4"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q14" VALUE="5"></td>
  </tr>
  <tr>
    <td align=center>1</td>
    <td align=center>2</td>
    <td align=center>3</td>
    <td align=center>4</td>
    <td align=center>5</td>
  </tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-15" rows=2 cols=100></textarea><br><br>

<LI>How often did you receive the "Cannot determine what's wrong with this" message?
<table>
  <tr>
    <td width=100 align=center><b>Frequently</b></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100 align=center><b>Infrequently</b></td>
  </tr>
  <tr>
    <td align=center><INPUT TYPE="RADIO" NAME="Q15" VALUE="1"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q15" VALUE="2"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q15" VALUE="3"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q15" VALUE="4"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q15" VALUE="5"></td>
  </tr>
  <tr>
    <td align=center>1</td>
    <td align=center>2</td>
    <td align=center>3</td>
    <td align=center>4</td>
    <td align=center>5</td>
  </tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-16" rows=2 cols=100></textarea><br><br>

<LI>How useful/worthless was it to: 
<table border=1>
	<tr>
		<td width=200 align=center></td>
		<td width=100 align=center><b>Useful</b></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100 align=center><b>Worthless</b></td>
	</tr>
	<tr>
        <td align=left width=200>a) Draw diagrams</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16A" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q16A" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16A" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16A" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16A" VALUE="5">5</td>
	</tr>
	<tr>
        <td align=left width=200>b) Define vectors by drawing</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16B" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q16B" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16B" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16B" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16B" VALUE="5">5</td>
	</tr>
	<tr>
        <td align=left width=200>c) Have Andes solve equations algebraically</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16C" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q16C" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16C" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16C" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16C" VALUE="5">5</td>
	</tr>
	<tr>
        <td align=left width=200>d) Have Andes substitute values in equations</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16D" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q16D" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16D" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16D" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16D" VALUE="5">5</td>
	</tr>
	<tr>
        <td align=left width=200>e) Use the equations page</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16E" VALUE="1">1
		<td align=center><INPUT TYPE="RADIO" NAME="Q16E" VALUE="2">2</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16E" VALUE="3">3</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16E" VALUE="4">4</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q16E" VALUE="5">5</td>
	</tr>
</table><br>

<textarea TABINDEX="1" NAME="OPEN-17" rows=2 cols=100></textarea><br><br>

<LI>Additional downloads of new versions of Andes were burdensome.

<table>
  <tr>
    <td width=100 align=center><b>Agree</b></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100 align=center><b>Disagree</b></td>
  </tr>
  <tr>
    <td align=center><INPUT TYPE="RADIO" NAME="Q17" VALUE="1"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q17" VALUE="2"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q17" VALUE="3"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q17" VALUE="4"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q17" VALUE="5"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q17" VALUE="NA"></td>
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

<textarea TABINDEX="1" NAME="OPEN-18" rows=2 cols=100></textarea><br><br>

<LI>If you had not been required to use Andes would you have used it 
<table border=0>
	<tr>
          <td align=left width=300>a) at the beginning of the semester</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q18A" VALUE="Y"><b>Yes</b>
		<td align=center><INPUT TYPE="RADIO" NAME="Q18A" VALUE="N"><b>No</b></td>
	</tr>
	<tr>
          <td align=left width=300>b) after two weeks</td>

		<td align=center><INPUT TYPE="RADIO" NAME="Q18B" VALUE="Y"><b>Yes</b>
		<td align=center><INPUT TYPE="RADIO" NAME="Q18B" VALUE="N"><b>No</b></td>
	</tr>
	<tr>
          <td align=left width=300>c) after eight weeks</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q18C" VALUE="Y"><b>Yes</b>
		<td align=center><INPUT TYPE="RADIO" NAME="Q18C" VALUE="N"><b>No</b></td>
	</tr>
	<tr>
          <td align=left width=300>d) after twelve weeks</td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q18D" VALUE="Y"><b>Yes</b>
		<td align=center><INPUT TYPE="RADIO" NAME="Q18D" VALUE="N"><b>No</b></td>
	</tr>
</table><br>

<textarea TABINDEX="1" NAME="OPEN-19" rows=2 cols=100></textarea><br><br>

<LI>On the average how long did you spend on an Andes problem?  <input tabindex="1" name="Q19" size="10" type="text"> minutes<br><br>

<LI>On the average how many hours/wk did you work with Andes?  <input tabindex="1" name="Q20" size="10" type="text"> hours<br><br>

<LI>What fraction of the Andes problems did you do substantially on your own?
<table>
  <tr>
    <td align=center><INPUT TYPE="RADIO" NAME="Q21" VALUE="1"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q21" VALUE="2"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q21" VALUE="3"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q21" VALUE="4"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q21" VALUE="5"></td>
  </tr>
  <tr>
    <td align=center width=100><25%</td>
    <td align=center width=100>25-40%</td>
    <td align=center width=100>40-60%</td>
    <td align=center width=100>60-80%</td>
    <td align=center width=100>>80%</td>
  </tr>
</table>

<textarea TABINDEX="1" NAME="OPEN-20" rows=2 cols=100></textarea><br><br>

<LI>When studying for quizzes and exams a review of Andes solutions played a major role.

<table>
	<tr>
		<td width=100 align=center><b>Agree</b></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100></td>
		<td width=100 align=center><b>Disagree</b></td>
	</tr>
	<tr>
		<td align=center><INPUT TYPE="RADIO" NAME="Q22" VALUE="1"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q22" VALUE="2"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q22" VALUE="3"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q22" VALUE="4"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q22" VALUE="5"></td>
		<td align=center><INPUT TYPE="RADIO" NAME="Q22" VALUE="NA"></td>
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

<textarea TABINDEX="1" NAME="OPEN-21" rows=2 cols=100></textarea><br><br>

<LI>As the semester progressed what did you find to be the greatest impediment(s) to doing Andes problems independently? (You may check more than one.)
  <OL class="withroman">
    <LI><input type="checkbox" name="Q23A">lack of Physics knowledge
    <LI><input type="checkbox" name="Q23B">lack of time
    <LI><input type="checkbox" name="Q23C">lack of motivation
    <LI><input type="checkbox" name="Q23D">difficulty in using Andes
    <LI><input type="checkbox" name="Q23E">availability of solutions from classmates
  </OL>

<textarea TABINDEX="1" NAME="OPEN-22" rows=2 cols=100></textarea><br><br>

<LI>What was the greatest strength of Andes?<br>
<textarea TABINDEX="1" NAME="Q24" rows=8 cols=100></textarea><br><br>

<LI>What was the greatest weakness of Andes?<br>
<textarea TABINDEX="1" NAME="Q25" rows=8 cols=100></textarea><br><br>

<LI>Was there anything that you wished you could do with Andes that it wouldn't let you do?<br>
<textarea TABINDEX="1" NAME="Q26" rows=8 cols=100></textarea><br><br>

<LI>Were there any features of the Andes interface that you would like to see changed?  If so, what and how?<br>
<textarea TABINDEX="1" NAME="Q27" rows=8 cols=100></textarea><br><br>

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

<LI>A system similar to Andes would be useful in some other $school courses.

<table>
  <tr>
    <td width=100 align=center><b>Agree</b></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100></td>
    <td width=100 align=center><b>Disagree</b></td>
  </tr>
  <tr>
    <td align=center><INPUT TYPE="RADIO" NAME="Q29" VALUE="1"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q29" VALUE="2"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q29" VALUE="3"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q29" VALUE="4"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q29" VALUE="5"></td>
    <td align=center><INPUT TYPE="RADIO" NAME="Q29" VALUE="NA"></td>
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

<textarea TABINDEX="1" NAME="OPEN-24" rows=2 cols=100></textarea><br><br>

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