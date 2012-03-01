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

$sectionName = $_POST['userSection'];
$hintType=$_POST['HintType'];
$user=$_POST['userName'];
$prob=$_POST['userProblem'];

if($user==''){
  $userName = "";
 } else {
  $userName="P.userName REGEXP '$user' AND";
 } 
if($sectionName==''){
  $userSection='';
 }else {
  $userSection = "P.userSection REGEXP  '$sectionName' AND";
 }
if($prob){
  $userProblem="P.userProblem='$prob' AND";
 }

$sql="select Count(DISTINCT userName) from OPEN_PROBLEM_ATTEMPT as P,STEP_TRANSACTION as S where $userName $userProblem $userSection and P.clientID=S.clientID and S.server like '%\"action\":\"problem-closed\"%' and S.client like '%\"time\"%' order by startTime";
$result = mysql_query($sql);
if ($myrow = mysql_fetch_array($result)) {
  $TotUsers=$myrow["Count(DISTINCT userName)"];
  echo "<p align='center'><font color=\"red\">Number of Students who attempted $prob are $TotUsers</p>";
 }

$sql="select Count(DISTINCT userName) from OPEN_PROBLEM_ATTEMPT as P,STEP_TRANSACTION as S where $userName $userProblem P.userSection='$userSection' and P.clientID=S.clientID and S.server like '%\"action\":\"problem-closed\"%' and S.server like '%\"correct_answer_entries_v_answer_entries\"%' and S.client like '%\"time\"%' and S.server like '%\"1\/%' order by startTime";
$result = mysql_query($sql);
if ($myrow = mysql_fetch_array($result)) {
  $TotUsers=$myrow["Count(DISTINCT userName)"];
  //  echo "<p align='center'><font color=\"red\">Number of Students who completed $prob are $TotUsers</p>";
 }

$sql="select * from OPEN_PROBLEM_ATTEMPT as P,STEP_TRANSACTION as S where $userName $userProblem P.userSection='$userSection' and P.clientID=S.clientID and S.server like '%\"action\":\"problem-closed\"%' and S.server like '%\"correct_answer_entries_v_answer_entries\"%' and S.client like '%\"time\"%' order by startTime";

$result = mysql_query($sql);
$Count=0;
if ($myrow = mysql_fetch_array($result)) {
  echo "<table border=1>";                                                                                                                                            
  echo "<tr><th>UserName</th><th>Client</th><th>Server</th></tr>";  
  do{
    $userN=$myrow["userName"];
    $client=$myrow["client"];
    $server=$myrow["server"];
    $done=strstr($server,"1\/");
    if($done){
      $Count=$Count+1;
      echo "<tr><td>$userN</td><td>$client</td><td>$server</td></tr>";
    }
    else
      echo "<tr><td>$userN</td><td>$client</td><td bgcolor=\"red\">$server</td></tr>";
  }
  while ($myrow = mysql_fetch_array($result));
  echo "</table>"; 
  echo "<br><p align='center'> Number of Students who completed this problem are : $Count</p>";
 }

mysql_close();
?>