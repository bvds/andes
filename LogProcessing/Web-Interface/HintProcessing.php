<?

$dbuser= $_POST['dbuser'];
$dbserver= "localhost";
$dbpass= $_POST['passwd'];
$dbname= "andes3";

function_exists('mysql_connect') or die ("Missing mysql extension");
mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE at $dbserver");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE $dbname");

$userSection = $_POST['userSection'];
$hintType=$_POST['HintType'];
$user=$_POST['userName'];
$prob=$_POST['userProblem'];

if($user){
  $userName="P1.userName='$user' AND";
 }
 else{
if($userSection=='study-e')
  $userName="P1.userName LIKE 'ThesisE%' AND";
 else if($userSection=='study')
   $userName="P1.userName LIKE 'astudy%' AND";
 else 
   $userName="P1.userName LIKE 'ThesisC%' AND";
 }
if($prob){
  $userProblem="P1.userProblem='$prob' AND";
 }

$tQuery="SELECT COUNT(*) FROM OPEN_PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE $userName $userProblem P1.userSection='$userSection' AND P1.clientID = P2.clientID";
$tResult=mysql_query($tQuery);
$myT = mysql_fetch_array($tResult);
$tIn=$myT["COUNT(*)"];
echo "<p align='center' />Total Number of Entries : $tIn<br>";
echo "<p align='center' />'$prob'<br>";

if($hintType=='Solicited'){
  //echo "<p align='center' />Instances of Solicited Hints are as follows:<br><br>";
  $sql = "SELECT * FROM OPEN_PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE $userName $userProblem P1.userSection='$userSection' AND P1.clientID = P2.clientID AND (P2.client LIKE '%\"action\":\"get-help\"%' OR P2.client LIKE '%\"action\":\"help-button\"%')";
 }
 else if($hintType=='All'){
   //echo "<p align='center' />Instances of Both Solicited and Unsolicited Hints are as follows:<br><br>";
   $sql = "SELECT * FROM OPEN_PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE $userName $userProblem P1.userSection='$userSection' AND P1.clientID = P2.clientID AND P2.server LIKE '%\"action\":\"show-hint\"%'";
 }
 else if($hintType=='Unsolicited'){
   //echo "<p align='center' />Instances of Unsolicited Hints are as follows:<br><br>";
   $sql = "SELECT * FROM OPEN_PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE $userName $userProblem P1.userSection='$userSection' AND P1.clientID = P2.clientID AND P2.server LIKE '%\"action\":\"show-hint\"%' AND P2.client Not LIKE '%\"action\":\"get-help\"%' AND P2.client Not LIKE '%\"action\":\"help-button\"%'";
}
 else{
   //echo "<p align='center' />Instances of Hint Abuse are as follows:";
 }


$Count=0;
$result = mysql_query($sql);
if ($myrow = mysql_fetch_array($result)) {
  echo "<table border=1>";
  echo "<tr><th>TID</th><th>Client</th><th>Server</th></tr>";
  do{
    $Count=$Count+1;
    $tID=$myrow["tID"];
    $client=$myrow["client"];
    $server=$myrow["server"];
    echo "<tr><td>$tID</td><td>$client</td><td>$server</td></tr>";
  }
  while ($myrow = mysql_fetch_array($result));
  echo "</table>";
  echo "<p align='center' />No of Instances : $Count";
 }
mysql_close();

?>