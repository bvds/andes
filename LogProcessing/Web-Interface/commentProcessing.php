<?php
$dbname= $_POST['dbname'];
$dbuser= $_POST['dbuser'];
$dbserver= "localhost";
$dbpass= $_POST['passwd'];

$adminName = $_POST['adminName'];
$orderBy = $_POST['item'];
$order = $_POST['order'];
$extra=$_POST['extra'];
$startDate = $_POST['startDate'];
$endDate = $_POST['endDate'];
$solved=$_POST['solved'];

//******** BEGIN LISTING THE CONTENTS OF  testTable*********                                                                                                            
//CONNECTION STRING                                                                                                                                                     
function_exists('mysql_connect') or die ("Missing mysql extension");
mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO THE DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE");                                                                                                                                  
setcookie("userName",$adminName,time()+(8*60*60));
setcookie("passWord",$dbpass,time()+(8*60*60));
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
if($extra == 'Reviewed'){
  // Changed extra from number to string or null
  // commit a0719d09f0017cb, Nov 9, 2011
  $extrac = "(P1.extra IS NOT NULL OR P1.extra !=0) AND";
  $extrae = "reviewed";
 }else if($extra == 'Original'){
  $extrac = "(P1.extra IS NULL or P1.extra = 0) AND";
  $extrae = "solved";
 }else{
  $extrac = "";
  $extrae = "solved or reviewed";
 }
if($startDate){
  $startDatec = "P1.startTime >= '$startDate' AND";
 } else {
  $startDatec = "";
 }
if($endDate){
  $endDatec = "P1.startTime <= '$endDate' AND";
 } else {
  $endDatec = "";
 }

$sqlOld = "SELECT * FROM PROBLEM_ATTEMPT AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 WHERE $extrac $startDatec $endDatec P1.clientID = P2.clientID AND P2.initiatingParty = 'client' AND P2.command LIKE '%\"action\":\"get-help\",\"text\":%' ORDER BY $orderBy $order";
$sql = "SELECT * FROM PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE $extrac $startDatec $endDatec P1.clientID = P2.clientID AND P2.client LIKE '%\"action\":\"get-help\",\"text\":%' ORDER BY $orderBy $order";

$resultOld = mysql_query($sqlOld);
$result = mysql_query($sql);
$removed=0;
$kept=0;

echo "<table border=1>";
echo "<tr><th>Solved</th><th>My Comment</th><th>User Name</th><th>Problem</th><th>Section</th><th>Starting Time</th><th>Comment</th><th>Additional</th></tr>\n";
while (($myrow = mysql_fetch_array($result)) ||
       ($myrow = mysql_fetch_array($resultOld))) {
  $tID=$myrow["tID"];
  // Choose new style with STEP_TRANSACTION or old style 
  // with PROBLEM_ATTEMPT_TRANSACTION
  if(isset($myrow["client"])){
    // Include cases where reply is null.
    $tempSql = "SELECT * FROM STEP_TRANSACTION WHERE tID = $tID and (server LIKE '%\"HANDLE-TEXT\":\"COMMENT\"%' or server like '%\"HANDLE-TEXT\":\"POSSIBLE-QUESTION\"%' or server not like '%\"result\":%')";
  } else {
    // Include cases where reply is null.
    // It is not quite Kosher to assume the next tID 
     $tempSql = "SELECT * FROM PROBLEM_ATTEMPT_TRANSACTION WHERE tID = ($tID+1) and (command LIKE '%\"HANDLE-TEXT\":\"COMMENT\"%' or command like '%\"HANDLE-TEXT\":\"POSSIBLE-QUESTION\"%' or command not like '%\"result\":%')";
  }
  $tempResult = mysql_query($tempSql);    
  if(mysql_fetch_array($tempResult))
    {
      $tempResult = NULL;
      $tempSql = NULL;
      $clientID=$myrow["clientID"];
      $tID=$myrow["tID"];
      $userName=$myrow["userName"];
      $userProblem=$myrow["userProblem"];
      $userSection=$myrow["userSection"];
      $startTime=$myrow["startTime"];
      // new style with STEP_TRANSACTION or old style with 
      // PROBLEM_ATTEMPT_TRANSACTION
      $tempCommand1=isset($myrow["client"])?$myrow["client"]:$myrow["command"];
      $tempCommand2 =explode("get-help\",\"text\":\"",$tempCommand1);
      $command=explode("\"}",$tempCommand2[1]);
      
      $rButton="UNCHECKED";
      $extraQuery="SELECT MAX(radioID),myComment FROM REVIEWED_PROBLEMS WHERE adminName = '$adminName' AND tID = $tID";
      $extraRes=mysql_query($extraQuery);
      if ($myExtrarow = mysql_fetch_array($extraRes)) {
	$extraField = $myExtrarow["MAX(radioID)"];
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
	echo "<tr><td><INPUT TYPE=checkbox NAME=$tID $rButton onclick=\"UpdateRecord('RecordUpdate.php?x=$dbuser&sv=$dbserver&pwd=$dbpass&d=$dbname&a=$adminName&t=$tID&u=$userName')\"></td><td>$myCom</td><td>$userName</td><td>$userProblem</td><td>$userSection</td><td>$startTime</td><td>$command[0]</td><td><a href=\"javascript:;\" onclick=\"copyRecord('\Save.php?x=$dbuser&sv=$dbserver&pwd=$dbpass&d=$dbname&a=$adminName&u=$userName&p=$userProblem&s=$userSection&t=$tID');\">View&nbsp;Solution</a> <a href=\"OpenTrace.php?x=$dbuser&sv=$dbserver&pwd=$dbpass&d=$dbname&cid=$clientID&u=$userName&p=$userProblem&s=$userSection&t=$tID\">Session&nbsp;log</a></td></tr>\n";
	$kept++;
      }else{
	$removed++;
      }
    }
 }
echo "</table>";

echo "<p>$kept comments shown, after filtering out $removed.\n";

mysql_close();
?>
</body>
</html>