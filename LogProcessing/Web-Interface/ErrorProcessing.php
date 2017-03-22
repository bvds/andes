<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
  <title>Andes Errors</title>
  <LINK REL=StyleSheet HREF="log.css" TYPE="text/css">

  <script type="text/javascript" src='xml-scripts.js'></script>
  <script type="text/javascript">
    function openTrace(url) {
      window.open(url);
    }
  </script>

</head>
<body>

<?php

require "db-login.php";

if(strcmp($dbuser,'open')==0) {
  $problem_attempt='OPEN_PROBLEM_ATTEMPT';
} else {
  $problem_attempt='PROBLEM_ATTEMPT';
} 

$adminSection = $_POST['adminSection'];
$adminName=$_POST['adminName'];
$startDate = $_POST['startDate'];
$endDate = $_POST['endDate'];
$errorType=$_POST['errorType'];
$problem=$_POST['problem'];

if($startDate) {
  $startDatec = "P1.startTime >= '$startDate' AND";
} else {
  $startDatec = "";
}
if($endDate) {
  $endDatec = "P1.startTime <= '$endDate' AND";
} else {
  $endDatec = "";
}
if($errorType) {
  $errorTypec = "P2.server REGEXP '\"error-type\":\"$errorType\"' AND";
} else {
  $errorTypec = "";
}
if($problem) {
  $problemc = "P1.userProblem = '$problem' AND";
} else {
  $problemc = "";
}

echo "<h2>The Errors and Warnings are as given below:</h2>";
echo "<table border=1>\n";
echo "<tr><th>Starting Time</th><th>Input</th><th>Error Type</th><th>Message</th><th>Tag</th><th>View</th></tr>\n";

$sql = $db->prepare("SELECT startTime,userName,userProblem,userSection,tID,client,server,P1.clientID from $problem_attempt AS P1,STEP_TRANSACTION AS P2 WHERE $startDatec $endDatec $problemc $errorTypec P2.server like '%\"log\":\"server\"%' AND P2.clientID=P1.clientID AND (P1.extra IS NULL OR P1.extra=0) order by P2.tID");
// echo "<pre>$sql</pre><br>\n";
$sql->execute();
$ecount=0;

  
while ($myrow = $sql->fetch()) {
  $ecount++;
  $tID=$myrow["tID"];  
  $clientID=$myrow["clientID"];
  $userName=$myrow["userName"];
  $userProblem=$myrow["userProblem"];
  $userSection=$myrow["userSection"];
  $startTime=$myrow["startTime"];
  $usertID=$tID-1;
  $ttID=$tID;
  $cc=$myrow["server"];
  $userCommand=$myrow["client"];
  $command=json_decode($cc);
  $a=json_decode($userCommand);
  $method=$a->method;  // Changed if reply parse fails.

  $yy=array();
  if($command && isset($command->result)) {
    // Don't know why I can't just use $command->result in the foreach
    $zz=$command->result; 
    foreach($zz as $bb) {
      if($bb->action == "log" && 
	 // New or old style logging
	 ((isset($bb->log) && strcmp($bb->log,"server")==0) || 
	  isset($bb->error))) {
        $key1="error-type";  // work-around for the dash
        $errorType=$bb->$key1;
	// New or old style logging
        $errorMsg=isset($bb->text)?$bb->text:$bb->error;
	// Error text is supposed to be plain text, so escape any html.
	$errorMsg=str_replace("&","&amp;",$errorMsg);
	$errorMsg=str_replace(">","&gt;",$errorMsg);
	$errorMsg=str_replace("<","&lt;",$errorMsg);
	$tag=isset($bb->entry)?$bb->entry:'';
        array_push($yy,"<td>$errorType</td><td>$errorMsg</td><td>$tag</td>");
      }
    }
  }
  // If there is no match, then something has gone wrong 
  // with the json decoding.  These are usually associated with very
  // long backtraces that have somehow gotten truncated.
  if(count($yy)==0) {
    $method='syntax';
    $bb=$cc;
    // add space after commas, for better line wrapping
    $bb=str_replace("\",\"","\", \"",$bb);
    // forward slashes are escaped in json, which looks funny
    $bb=str_replace("\\/","/",$bb);
    $bb=substr($bb,0,200);
    array_push($yy,"<td colspan=\"3\">$bb &#8230;</td>");
  }
  $nr=count($yy); // should always be nonzero

  //  $lastID=$tID-1;
  //  $userSql="select command from PROBLEM_ATTEMPT_TRANSACTION where tID=$lastID";
  $aa=json_encode($a->params);
  // Escape html codes so actual text is seen.
  $aa=str_replace("&","&amp;",$aa);
  $aa=str_replace(">","&gt;",$aa);
  $aa=str_replace("<","&lt;",$aa);
  // add space after commas, for better line wrapping
  $aa=str_replace("\",\"","\", \"",$aa);
  // forward slashes are escaped in json, which looks funny
  $aa=str_replace("\\/","/",$aa);

  echo "<tr class=\"$method\"><td rowspan=\"$nr\">$startTime</td>";
  echo "<td rowspan=\"$nr\">$aa</td>";
  echo array_shift($yy);

  echo "<td rowspan=\"$nr\"><a href=\"javascript:;\" onclick=\"openTrace('OpenTrace.php?x=$dbuser&amp;sv=$dbserver&amp;pwd=$dbpass&amp;d=$dbname&amp;cid=$clientID&amp;u=$userName&amp;p=$userProblem&amp;s=$userSection&amp;t=$ttID');\">Session&nbsp;log</a><br><a href=\"javascript:;\" onclick=\"copyRecord('Save.php?x=$dbuser&amp;sv=$dbserver&amp;pwd=$dbpass&amp;d=$dbname&amp;au=$adminName&amp;as=$adminSection&amp;&cid=$clientID&u=$userName&amp;p=$userProblem&amp;s=$userSection&amp;t=$usertID');\">Copy&nbsp;Session</a></td></tr>\n";

  foreach ($yy as $bb) {
    echo "<tr class=\"$method\">$bb</tr>\n";
  }

}

echo "</table>\n";
echo "<p>total of $ecount errors.</p>\n";

?>
</body>
</html>
