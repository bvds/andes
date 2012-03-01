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

if($userSection=='study-e')
  $userName="P1.userName LIKE 'ThesisE%' AND";
 else if($userSection=='study-c')
   $userName="P1.userName LIKE 'ThesisC%' AND";
 else
   $userName="";

$userProblemQ="SELECT DISTINCT (userProblem) FROM OPEN_PROBLEM_ATTEMPT AS P1 WHERE $userName P1.userSection='$userSection'";
$userProbResult=mysql_query($userProblemQ);

if ($myProbs = mysql_fetch_array($userProbResult)){
  do{ 
    $curProb=$myProbs["userProblem"];
    echo "<BR>Problem Name : '$curProb'<BR>";
    $userProblem="P1.userProblem = '$curProb' AND";
    $sql = "SELECT P1.userName,COUNT(*) FROM OPEN_PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE $userName $userProblem P1.userSection='$userSection' AND P1.clientID = P2.clientID AND P2.server LIKE '%\"mode\":\"incorrect\"%' GROUP BY P1.userName ORDER BY P1.startTime";
    $sqlTot = "SELECT P1.userName,COUNT(*) FROM OPEN_PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE $userName $userProblem P1.userSection='$userSection' AND P1.clientID = P2.clientID GROUP BY(P1.userName) ORDER BY(P1.startTime)";

    $result = mysql_query($sql);
    $cResult = mysql_query($sqlTot);
    $sum=0;

    if ($myrow = mysql_fetch_array($cResult)) {
      echo "<table border=1>";
      echo "<tr><th>User Name</th><th>Total Number of Entries</th></tr>";
      do{
	$user=$myrow["userName"];
	$count=$myrow["COUNT(*)"];
	$sum=$sum+$count;
	echo "<tr><td align='center'>$user</td><td align='center'>$count</td></tr>";	
      }
      while ( ($myrow = mysql_fetch_array($cResult)) );
      echo "</table>";  
      echo "<BR>Total is $sum<BR>";
    }

      if ($myrow = mysql_fetch_array($result)) {
	echo "<table border=1>";
	echo "<tr><th>User Name</th><th>Number of Red Entries</th></tr>";
	$sum=0;
	do{
	  $user=$myrow["userName"];
	  $count=$myrow["COUNT(*)"];
	  $sum=$sum+$count;
	  echo "<tr><td align='center'>$user</td><td align='center'>$count</td></tr>";
	}
	while ( ($myrow = mysql_fetch_array($result)) );
	echo "</table>";
	echo "<BR>Total is $sum<BR>";
      }
  }while($myProbs = mysql_fetch_array($userProbResult));
}

mysql_close();

?>

