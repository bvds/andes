<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
</head>
<body>
<?
$CMD_LINE_MODE = 1;
if ($CMD_LINE_MODE) {
  include "cmdLineDebug.php";
}

include "mysql_stuff.php";
//$adminNamec $extrac  $startDatec $endDatec 
// ORDER BY $orderBy $order";
// WHERE clientID = clientID";
$sql = "SELECT * FROM PROBLEM_ATTEMPT";

$result = mysql_query($sql);
if ($myrow = mysql_fetch_array($result)) {
  echo "<table border=1>\n";
  echo "<tr><th>User Name</th><th>Problem</th><th>Section</th><th>Starting Time</th><th>Log</th></tr>\n";
  do
    {
      $clientID=$myrow["clientID"];
      $userName=$myrow["userName"];
      $userProblem=$myrow["userProblem"];
      $userSection=$myrow["userSection"];
      $startTime=$myrow["startTime"];
      
      echo "<tr><td>$userName</td><td>$userProblem</td><td>$userSection</td><td>$startTime</td><td><a href=\"OpenTrace.php?x=$dbuser&sv=$dbserver&pwd=$dbpass&d=$dbname&cid=$clientID\">Session log</a></td></tr>\n";
       
    } while ($myrow = mysql_fetch_array($result));
  echo "</table>\n";
 }



mysql_close();

?>
</body>
</html>
