<?php

require "db-login.php";

$adminSection = $_POST['adminSection'];
$adminName = $_POST['adminName'];
$orderBy = $_POST['item'];
$order = $_POST['order'];
$extra=$_POST['extra'];
$startDate = $_POST['startDate'];
$endDate = $_POST['endDate'];
$solved=$_POST['solved'];

?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
  <LINK REL=StyleSheet HREF="log.css" TYPE="text/css">
  <script type="text/javascript" src='xml-scripts.js'></script>
</head>
<body>

<?php
echo "<h2>Comments given by the Andes Users, sorted in $order order of $orderBy, are as follows:</h2><BR>";
if($order=='Descending')
  $order = "DESC";
 else
   $order = "";
if($extra == 'Reviewed') {
  // Changed extra from number to string or null
  // commit a0719d09f0017cb, Nov 9, 2011
  $extrac = "(P1.extra IS NOT NULL OR P1.extra !=0) AND";
  $extrae = "reviewed";
 } else if($extra == 'Original') {
  $extrac = "(P1.extra IS NULL OR P1.extra = 0) AND";
  $extrae = "solved";
 } else {
  $extrac = "";
  $extrae = "solved or reviewed";
 }
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

$sql = $db->prepare("SELECT * FROM PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE $extrac $startDatec $endDatec P1.clientID = P2.clientID AND P2.client LIKE '%\"action\":\"get-help\",\"text\":%' ORDER BY $orderBy $order");

$sql->execute();
$removed=0;
$kept=0;

echo "<table border=1>";
echo "<tr><th>Solved</th><th>My Comment</th><th>User Name</th><th>Problem</th><th>Section</th><th>Starting Time</th><th>Comment</th><th>Additional</th></tr>\n";
while ($myrow = $sql->fetch()) {
  $tID=$myrow["tID"];
  // Include cases where reply is null.
  $tempSql = $db->prepare("SELECT * FROM STEP_TRANSACTION WHERE tID = $tID and (server LIKE '%\"HANDLE-TEXT\":\"COMMENT\"%' or server like '%\"HANDLE-TEXT\":\"POSSIBLE-QUESTION\"%' or server not like '%\"result\":%')");
  $tempSql->execute();
  if($tempSql->fetch()) {
    $tempResult = NULL;
    $tempSql = NULL;
    $clientID=$myrow["clientID"];
    $tID=$myrow["tID"];
    $userName=$myrow["userName"];
    $userProblem=$myrow["userProblem"];
    $userSection=$myrow["userSection"];
    $startTime=$myrow["startTime"];
    $tempCommand1=$myrow["client"];
    $tempCommand2 =explode("get-help\",\"text\":\"",$tempCommand1);
    $command=explode("\"}",$tempCommand2[1]);
    
    $rButton="UNCHECKED";
    $myCom="";
    // Get checkbox status and any comments from REVIEWED_PROBLEMS.
    $extraQuery = $db->prepare("SELECT radioID,myComment FROM REVIEWED_PROBLEMS WHERE userName=? AND tID=?");
    $extraQuery->execute(array($adminName, $tID));
    if ($myExtrarow = $extraQuery->fetch()) {
      $extraField = $myExtrarow["radioID"];
      if($extraField == 1)
        $rButton="CHECKED";         
      $myCom=$myExtrarow["myComment"];
      if($myCom == null)
        $myCom="NA";
    }
    
    // Only print out out rows that have right solved status.
    // It may be more efficient to do this filtering at the database
    // query level.
    if(($rButton=='CHECKED' && $solved != "Unsolved") ||
       ($rButton=='UNCHECKED' && $solved != "Solved")) {
      $rid = "row".$tID;
      echo "<tr id=\"$rid\"><td><INPUT TYPE=checkbox $rButton onclick=\"UpdateRecord('RecordUpdate.php?x=$dbuser&sv=$dbserver&pwd=$dbpass&d=$dbname&au=$adminName&t=$tID&u=$userName&chk='+(this.checked?1:0),'$rid')\"></td><td class=\"comment\">$myCom</td><td>$userName</td><td>$userProblem</td><td>$userSection</td><td>$startTime</td><td>$command[0]</td><td><a href=\"javascript:;\" onclick=\"copyRecord('Save.php?x=$dbuser&sv=$dbserver&pwd=$dbpass&d=$dbname&au=$adminName&as=$adminSection&u=$userName&p=$userProblem&s=$userSection&t=$tID');\">View&nbsp;Solution</a> <a href=\"OpenTrace.php?x=$dbuser&sv=$dbserver&pwd=$dbpass&d=$dbname&cid=$clientID&u=$userName&p=$userProblem&s=$userSection&t=$tID\">Session&nbsp;log</a></td></tr>\n";
      $kept++;
    } else {
      $removed++;
    }
  }
}
echo "</table>";

echo "<p>$kept comments shown, after filtering out $removed.\n";

?>
</body>
</html>
