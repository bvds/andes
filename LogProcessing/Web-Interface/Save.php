<?
$dbuser= "root";
$dbserver= "localhost";
$dbpass= "sin(0)=0";
$dbname= "andes";

mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE");          

$adminName = $_POST['adminName'];
$userName = $_POST['userName'];
$userProblem = $_POST['userProblem'];
$userSection = $_POST['userSection'];
$tID = $_POST['tID'];
$clientID = $adminName.date("mdy").time();

$sql = "SELECT * FROM PROBLEM_ATTEMPT AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 WHERE P1.clientID = P2.clientID AND P1.userName = '$userName' AND P1.userProblem = '$userProblem' AND P1.userSection = '$userSection' AND P2.tID < ($tID+2)";  

$result = mysql_query($sql);

if ($myrow = mysql_fetch_array($result)) {
  $maxQuery = "SELECT MAX(extra) FROM PROBLEM_ATTEMPT";
  $maxResult = mysql_query($maxQuery);
  $maxResultArray = mysql_fetch_array($maxResult);
  $maxValue = $maxResultArray["MAX(extra)"];
  $IncrVal = $maxValue+1;
  $reviewQuery="INSERT INTO REVIEWED_PROBLEMS(extra,userName) VALUES($IncrVal,'$userName')";
  mysql_query($reviewQuery);
  
  $query="INSERT INTO PROBLEM_ATTEMPT(userName,clientID,classinformationID,userProblem,userSection,extra) VALUES('$adminName','$clientID',2,'$userProblem','$userSection',$maxValue+1)";
mysql_query($query);
  do
  {
    $init = $myrow["initiatingParty"];
    $comm = $myrow["command"];       
      
    $insertQuery = "INSERT INTO PROBLEM_ATTEMPT_TRANSACTION(clientID,command,initiatingParty) VALUES('$clientID','$comm','$init')";
    mysql_query($insertQuery); 
  }
 while ($myrow = mysql_fetch_array($result));
 }
mysql_close();

?>

<script type="text/javascript" language="javascript">
 var u = '<?php echo $adminName; ?>';
 var p = '<?php echo $userProblem; ?>';
 var s = '<?php echo $userSection; ?>';
 var e = '<?php echo $IncrVal; ?>';
window.open("/web-UI/index.html?s="+s+"&u="+u+"&p="+p+"&e="+e);
</script>
