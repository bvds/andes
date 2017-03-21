<?php

require "db-login.php";

$userSection = $_POST['userSection'];

if($userSection=='study-e')
  $userName="P1.userName LIKE 'ThesisE%' AND";
 else if($userSection=='study-c')
   $userName="P1.userName LIKE 'ThesisC%' AND";
 else
   $userName="";

$userProblemQ = $db->prepare("SELECT DISTINCT (userProblem) FROM OPEN_PROBLEM_ATTEMPT AS P1 WHERE $userName P1.userSection='$userSection'");
$userProblemQ->execute();

if ($myProbs = $userProblemQ->fetch()) {
  do {
    $curProb=$myProbs["userProblem"];
    echo "<BR>Problem Name : '$curProb'<BR>";
    $userProblem="P1.userProblem = '$curProb' AND";

    $sqlTot = $db->prepare("SELECT P1.userName,COUNT(*) FROM OPEN_PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE $userName $userProblem P1.userSection='$userSection' AND P1.clientID = P2.clientID GROUP BY(P1.userName) ORDER BY(P1.startTime)");

    $sqlTot->execute();

    if ($myrow = $sqlTot->fetch()) {
      echo "<table border=1>";
      echo "<tr><th>User Name</th><th>Total Number of Steps</th></tr>";

      do {
	$user=$myrow["userName"];
	$count=$myrow["COUNT(*)"];
	echo "<tr><td align='center'>$user</td><td align='center'>$count</td></tr>";	
      } while ($myrow = $sqlTot->fetch());
      echo "</table>";   
    }
  } while($myProbs = $userProblemQ->fetch());
}

?>

