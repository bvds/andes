<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
 <LINK REL=StyleSheet HREF="log.css" TYPE="text/css">
<?

$dbuser=$_GET["x"];
$dbserver=$_GET["sv"];
$dbpass=$_GET["pwd"];
$dbname=$_GET["d"];

function_exists('mysql_connect') or die ("Missing mysql extension");
mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE");


$userName = $_GET["u"];
$userProblem = $_GET["p"];
$userSection = $_GET["s"];
$tID = $_GET["t"];
$clientID = $_GET["cid"];
$methods = $_GET["m"];
if($methods){
  $methods = explode(",",$_GET["m"]);
 }

if($clientID){
  $sess=$clientID;
 } else {
  $sess=" for $userName, $userProblem, $userSection";
 }

echo "  <title>$userName, $userProblem</title>\n";
if($tID!=''){
  echo "<script type=\"text/javascript\">\n";
  echo "  window.onload = function() {\n";
  echo "    var row=document.getElementById('t$tID');\n";
  echo "    window.scrollTo(0,row.offsetTop);\n";
  echo "  };\n";
  echo "</script>\n";
 }
echo "</head>\n";
echo "<body>\n";
echo "<h2>Session $sess</h2>\n";

if($clientID==''){
  // Old style with PROBLEM_ATTEMPT_TRANSACTION
  $sqlOld = "SELECT initiatingParty,command,tID FROM PROBLEM_ATTEMPT AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 WHERE P1.clientID = P2.clientID AND P1.userName = '$userName' AND P1.userProblem = '$userProblem' AND P1.userSection = '$userSection'";
  $sql = "SELECT client,server,tID FROM PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE P1.clientID = P2.clientID AND P1.userName = '$userName' AND P1.userProblem = '$userProblem' AND P1.userSection = '$userSection' ORDER BY tID";
 } else {
  // Old style with PROBLEM_ATTEMPT_TRANSACTION
  $sqlOld = "SELECT initiatingParty,command,tID FROM PROBLEM_ATTEMPT_TRANSACTION WHERE clientID = '$clientID'";
  $sql = "SELECT client,server,tID FROM STEP_TRANSACTION WHERE clientID = '$clientID' ORDER BY tID";
 }

$resultOld = mysql_query($sqlOld);   // Old style with PROBLEM_ATTEMPT_TRANSACTION
$result = mysql_query($sql);
echo "<table border=1 width=\"100%\">";
echo "<tr><th>Time</th><th>Action</th><th>Response</th></tr>\n";

// Newer versions of php have a json decoder built-in.  Should 
// eventually have test for php version and use built-in, when possible.
include 'JSON.php';
$json = new Services_JSON();

// get student input and server reply
while (
       // Old style with PROBLEM_ATTEMPT_TRANSACTION
       (($myrow1 = mysql_fetch_array($resultOld)) &&
	($myrow2 = mysql_fetch_array($resultOld))) ||
       $myrow = mysql_fetch_array($result)) {
  
  // Old style with PROBLEM_ATTEMPT_TRANSACTION
  if($myrow1["initiatingParty"]=='client'){
    $action=$myrow1["command"];
    $ttID=$myrow1["tID"];
    $response=$myrow2["command"];
  } else if ($myrow1["initiatingParty"]=='server'){
    $action=$myrow2["command"];
    $ttID=$myrow2["tID"];
    $response=$myrow1["command"];
  } else {
    $action=$myrow["client"];
    $ttID=$myrow["tID"];
    $response=$myrow["server"];
  }  
  
  
  $a=$json->decode($action);
  $b=$json->decode($response);
  $ttime=$a->params->time;
  unset($a->params->time);  // so time doesn't show up twice.
  $method=$a->method;
  if(!$methods || in_array($method,$methods)){
    $aa=$json->encode($a->params);
    // Escape html codes so actual text is seen.
    $aa=str_replace("&","&amp;",$aa);
    $aa=str_replace(">","&gt;",$aa);
    $aa=str_replace("<","&lt;",$aa);
    // add space after commas, for better line wrapping
    $aa=str_replace("\",\"","\", \"",$aa);
    // forward slashes are escaped in json, which looks funny
    $aa=str_replace("\\/","/",$aa);
    
    echo "  <tr class='$method' id='t$ttID'><td>$ttime</td><td>$aa</td><td>";
    if(isset($b->result)){
      echo "<ul>";
      foreach($b->result as $bb){
	// add space after commas, for better line wrapping
	$bbb=str_replace("\",\"","\", \"",$json->encode($bb));
	// forward slashes are escaped in json, which looks funny
	$bbb=str_replace("\\/","/",$bbb);
	echo "<li>$bbb</li>";
      }
      echo "</ul>";
    } else {
      // json parse of result failed.
      echo "$response";
    }
    echo "</td></tr>\n";
  }
 }

mysql_close();
?>
  </table>
</body>
</html>
