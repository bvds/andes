<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
 <LINK REL=StyleSheet HREF="log.css" TYPE="text/css">
<?php

require "db-login.php";

$clientID = isset($_GET["cid"])?$_GET["cid"]:''; // Optional
$userName = $_GET["u"];
$userProblem = $_GET["p"];
$userSection = $_GET["s"];
$tID = isset($_GET["t"])?$_GET["t"]:''; // Optional

$methods = isset($_GET["m"])?$_GET["m"]:''; // Optional
if($methods){
  $methods = explode(",",$_GET["m"]);
 }

if($clientID){
  $sess=$clientID;
 } else {
  $sess=" for $userName, $userProblem, $userSection";
 }

echo "  <title>$userName, $userProblem</title>\n";
if($tID!=''){ ?>
  <script type=\"text/javascript\">
    window.onload = function() {
      var row=document.getElementById('t$tID');
      window.scrollTo(0,row.offsetTop);
    };
  </script>
<?php } ?>
</head>
<body>
<?php
echo "<h2>Session $sess</h2>\n";

if($clientID==''){
  $sql = $db->prepare("SELECT client,server,tID FROM OPEN_PROBLEM_ATTEMPT AS P1,STEP_TRANSACTION AS P2 WHERE P1.clientID=P2.clientID AND P1.userName=? AND P1.userProblem=? AND P1.userSection=? ORDER BY tID");
  $queryParams = array($userName, $userProblem, $userSection);
 } else {
  $sql = $db->prepare("SELECT client,server,tID FROM STEP_TRANSACTION WHERE clientID=? ORDER BY tID");
  $queryParams = array($clientID);
 }

$sql->execute($queryParams);
echo "<table border=1 width=\"100%\">";
echo "<tr><th>Time</th><th>Action</th><th>Response</th></tr>\n";


// get student input and server reply
while ($myrow1 = $sql->fetch()) {
  
  $action=$myrow1["client"];
  $ttID=$myrow1["tID"];
  $response=$myrow1["server"];

  $a=json_decode($action);
  $b=json_decode($response);
  $ttime=$a->params->time;
  $id=$a->id;  // Maybe want to add this to row...
  unset($a->params->time);  // so time doesn't show up twice.
  $method=$a->method;
  if(!$methods || in_array($method,$methods)){
    $aa=json_encode($a->params);
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
	$bbb=str_replace("\",\"","\", \"",json_encode($bb));
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

?>
  </table>
</body>
</html>
