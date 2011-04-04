<?
$dbuser=$_GET["x"];
$dbserver=$_GET["sv"];
$dbpass=$_GET["pwd"];
$dbname=$_GET["d"];

$adminName = $_GET["a"];
$userName = $_GET["u"];
$userProblem = $_GET["p"];
$userSection = $_GET["s"];
$tID = $_GET["t"];
$clientID = $adminName.date("mdy").time();

function_exists('mysql_connect') or die ("Missing mysql extension");
mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE");          

$sqlOld = "SELECT * FROM PROBLEM_ATTEMPT AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 WHERE P1.clientID = P2.clientID AND P1.userName = '$userName' AND P1.userProblem = '$userProblem' AND P1.userSection = '$userSection' AND P2.tID < ($tID+2)";  
$sql = "SELECT * FROM PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE P1.clientID = P2.clientID AND P1.userName = '$userName' AND P1.userProblem = '$userProblem' AND P1.userSection = '$userSection' AND P2.tID <= $tID";  

$resultOld = mysql_query($sqlOld);
$result = mysql_query($sql);

if (($myrow = mysql_fetch_array($result)) ||
    ($myrow = mysql_fetch_array($resultOld))) {
  $maxQuery = "SELECT MAX(extra) FROM PROBLEM_ATTEMPT";
  $maxResult = mysql_query($maxQuery);
  $maxResultArray = mysql_fetch_array($maxResult);
  $maxValue = $maxResultArray["MAX(extra)"];
  $IncrVal = $maxValue+1;
  $reviewQuery="INSERT INTO REVIEWED_PROBLEMS(extra,userName,tID,adminName) VALUES($IncrVal,'$userName',$tID,'$adminName')";
  mysql_query($reviewQuery);
  
  $query="INSERT INTO PROBLEM_ATTEMPT(userName,clientID,userProblem,userSection,extra) VALUES('$adminName','$clientID','$userProblem','$userSection',$maxValue+1)";
  mysql_query($query);
  do
  {
    // Choose between old style with PROBLEM_ATTEMPT_TRANSACTION 
    // and new style with STEP_TRANSACTION
    if(isset($myrow["command"])){
      // Only record client (it would be nice to have replies
      // but we don't need them).
      if(strcmp($myrow["initiatingParty"],"client")==0){
	$client="'".addslashes($myrow["command"])."'";  
	$server="null";
      } else {
	continue;
      }
    } else {
      $client=$myrow["client"]?"'".addslashes($myrow["client"])."'":"null";
      $server=$myrow["server"]?"'".addslashes($myrow["server"])."'":"null";
    }
      $insertQuery = "INSERT INTO STEP_TRANSACTION(clientID,client,server) VALUES('$clientID',$client,$server)";
    mysql_query($insertQuery); 
  }
  while (($myrow = mysql_fetch_array($result)) ||
	 ($myrow = mysql_fetch_array($resultOld)));
 }
mysql_close();
echo "/web-UI/index.html?s=".$userSection."&u=".$adminName."&p=".$userProblem."&e=".$IncrVal;
?>
