#!/usr/bin/perl -w
#
#         Generate a set of lon-capa problems 
#
@problems=('vec1ay','vec2a');

foreach my $problem (@problems){
    open FILE, ">$problem.problem" || die "CANT OPEN";
    # Need to escape all $ and @ in here:
    print FILE <<ENDOUT;
<problem>    
    <parameter name="weight" id="12" type="float_pos" default="42" description="Weight" />
<script type="loncapa/perl">

# Name of the problem in ANDES
\$problem='$problem';

# Internal stuff to build a unique keys for student and seciton
\$user=&EXT('user.name').'_'.&EXT('user.domain');  # Andy Pawl used 'user.name'
\$class=&EXT('request.course.id').'_'.&sec();  # Andy Pawl used 'user.course.sec'
# Simulate a new submission
\$timestamp=time;
\$response="{\"user\": \"\$user\", \"class\": \"\$class\", \"problem\": \"\$problem\"}";

# create status to date.
\$weight=&EXT('resource.0.weight');
if ((!defined(\$weight)) || (\$weight eq '')) { \$weight=1; }
\$awarded=&EXT('user.resource.resource.0.awarded');
if (!defined(\$awarded)) { \$awarded=0; }
\$scoreformat=&EXT('resource.0.scoreformat');
if (!defined(\$scoreformat) || \$scoreformat eq '') { \$scoreformat="2f"; }
\$display='';
if (&EXT('resource.0.problemstatus')!~/^no/) {
   if (!defined(\$awarded)) {
      \$display=\$weight.' possible points.';
   } else {
      \$display='You have '.&format(\$awarded*\$weight,\$scoreformat).' out of '.
            \$weight.' possible points.';
   }
}
</script>

<startouttext />
<a href="http://gideon.eas.asu.edu/web-UI/index.html?s=\$class&amp;u=\$user&amp;p=\$problem&amp;e=">\$problem</a>
\$display
<endouttext />
<externalresponse answer="\$response" url="http://gideon.eas.asu.edu/get-score">
    <hiddensubmission value="\$timestamp" />

</externalresponse>
</problem>
ENDOUT
}
exit;







