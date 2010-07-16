<?
$dbuser=$_GET["x"];
$dbserver=$_GET["sv"];
$dbpass=$_GET["pwd"];
$dbname=$_GET["d"];

mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE");

echo "<h2>The Sequence of Interactions between the user and the server leading to the Error are as follows</h2>";

$userName = $_GET["u"];
$userProblem = $_GET["p"];
$userSection = $_GET["s"];
$tID = $_GET["t"];

$sql = "SELECT tID,command FROM PROBLEM_ATTEMPT AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 WHERE P1.clientID = P2.clientID AND P1.userName = '$userName' AND P1.userProblem = '$userProblem' AND P1.userSection = '$userSection' AND P2.tID <= $tID";

$result = mysql_query($sql);
echo "<table border=1>";
echo "<tr><th>TID</th><th>Command</th></tr>";

while ($myrow = mysql_fetch_array($result)) {
  $tID=$myrow["tID"];
  $command=$myrow["command"];

  echo "<tr><td>$tID</td><td>$command</td></tr>";
 }

mysql_close();
?>