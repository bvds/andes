#!/usr/bin/php
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
   <LINK REL=StyleSheet HREF="../Web-Interface/log.css" TYPE="text/css" >
   <title>Rerun Sessions</title>

<style type="text/css">
   span.red {text-color: #ffe0e0;}
</style>
	    
<script type="text/javascript">
     function openTrace($url){  
        window.open($url);
     }
</script>
	    
</head>
<body>

<?php
	    
$dbname= 'andes3'; //$_POST['dbname'];
$dbuser= 'root'; // $_POST['dbuser'];
$dbserver= "localhost";
$dbpass= file_get_contents("db_password");  /// $_POST['passwd'];

//CONNECTION STRING  
    
function_exists('mysql_connect') or die ("Missing mysql extension");
mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE"); 

$adminName = ''; // $_POST['adminName'];
$sectionName = 'study' ; //$_POST['sectionName'];
$startDate = ''; // $_POST['startDate'];
$endDate = ''; // $_POST['endDate'];
$methods = array('open-problem','solution-step','seek-help','close-problem');  //implode(",",$_POST['methods']);

if($adminName==''){
  $adminNamec = "";
  $adminNamee = "";
 } else {
  $adminNamec = "P1.userName = '$adminName' AND";
  $adminNamee = " by $adminName,";
 }  
if($sectionName==''){
  $sectionNamec = "";
  $sectionNamee = "";
 } else {
  $sectionNamec = "P1.userSection = '$sectionName' AND";
  $sectionNamee = " by $sectionName,";
 }  

$extrac = "P1.extra = 0 AND";
$extrae = "solved";

if($startDate){
  $startDatec = "P1.startTime >= '$startDate' AND";
 } else {
  $startDatec = "";
 }
if($endDate){
  $endDatec = "P1.startTime <= '$endDate' AND";
 } else {
  $endDatec = "";
 }

echo "<h2>Problems $extrae,$adminNamee$sectionNamee</h2>\n";

// Newer versions of php have a json decoder built-in.  Should 
// eventually have test for php version and use built-in, when possible.
include '../Web-Interface/JSON.php';
$json = new Services_JSON();

function escapeHtml($bb){
	    // add space after commas, for better line wrapping
	    $bbb=str_replace("\",\"","\", \"",$bb);
	    // forward slashes are escaped in json, which looks funny
	    return str_replace("\\/","/",$bbb);
}

require_once('jsonRPCClient.php');
$server  = new jsonRPCClient('http://localhost/help-test');
$sessionIdBase = "_" . date('h:i:s') . "_";
$sessionId = 0;
  
$sql = "SELECT * FROM PROBLEM_ATTEMPT AS P1 WHERE $adminNamec $sectionNamec $extrac  $startDatec $endDatec P1.clientID = P1.clientID ORDER BY startTime";

$result = mysql_query($sql);

while ($myrow = mysql_fetch_array($result)) {
  $userName=$myrow["userName"];
  $userProblem=$myrow["userProblem"];
  $userSection=$myrow["userSection"];
  $startTime=$myrow["startTime"];
  $clientID=$myrow["clientID"];
  echo "User:&nbsp; $userName, Problem: &nbsp; $userProblem, Section:&nbsp; $userSection Start:&nbsp; $startTime\n";
  echo "<table border=1 width=\"100%\">";
  echo "<tr><th>Turn</th><th>Action</th><th>Old Response</th><th>New Response</th></tr>\n";
  
  $tempSql = "SELECT initiatingParty,command,tID FROM PROBLEM_ATTEMPT_TRANSACTION WHERE clientID = '$clientID'";
  $tempResult = mysql_query($tempSql);
  $sessionId++;  
  
  // get student input and server reply
  while (($myrow1 = mysql_fetch_array($tempResult)) &&
	 ($myrow2 = mysql_fetch_array($tempResult))) {
    if($myrow1["initiatingParty"]=='client'){
      $action=$myrow1["command"];
      $ttID=$myrow1["tID"];
      $response=$myrow2["command"];
    } else {
      $action=$myrow2["command"];
      $ttID=$myrow2["tID"];
      $response=$myrow1["command"];
    }
    
    $newResponse = $server->message($action,$sessionIdBase . $sessionId);

    $a=$json->decode($action);
    $method=$a->method;
    if(isset($a->id)){
      $tid=$a->id;
    } else {
      $tid="none";
    }
    if(!$methods || in_array($method,$methods)){
      $aa=$json->encode($a->params);
      // Escape html codes so actual text is seen.
      $aa=str_replace("&","&amp;",$aa);
      $aa=str_replace(">","&gt;",$aa);
      $aa=str_replace("<","&lt;",$aa);
      $aa=escapeHtml($aa);
      
      if (strcmp($response,$newResponse) != 0) {
	$jr=$json->decode($response);
	$njr=$json->decode($newResponse);
	if(isset($jr->result) && isset($njr->result)){
	  $imax=sizeof($jr->result);
	  $nimax=sizeof($njr->result);
	  $rows=array();
	  $i=0; $ni=0;
	  while($i<$imax && $ni<$nimax){
	    $bc=$jr->result[$i];  // copy used for compare
	    $bb=$json->encode($bc); // print this out
	    $nbc=$njr->result[$ni];  // copy used for compare
	    $nbb=$json->encode($nbc);  // printed this out
	    // Remove any backtraces from compare.
	    unset($bc->backtrace);
	    $bbc=$json->encode($bc);
	    unset($nbc->backtrace);
	    $nbbc=$json->encode($nbc);
	    // Remove double precision notation from constants.
	    $bbc=preg_replace('/([0-9])d0/','$1',$bbc);
	    // Canonicalize new reply.
	    // commit 14db0660b489c3c2, Nov 19, 2010
            // Add logging for window clicks.
	    $nbbc=preg_replace('/andes.help.link.*?;/','',$nbbc);
	    
	    if(strcmp($bbc,$nbbc)==0){
	      $i++; $ni++;
	    }elseif($imax-$i == $nimax-$ni){
	      $bbb=escapeHtml($bb);
	      $nbbb=escapeHtml($nbb);
	      array_push($rows,"<td>$bbb</td><td>$nbbb</td>");
	      $i++; $ni++;
	    }elseif($imax-$i < $nimax-$ni){
	      $nbbb=escapeHtml($nbb);
	      array_push($rows,"<td></td><td>$nbbb</td>");
	      $ni++;
	    }else{
	      $bbb=escapeHtml($bb);
	      array_push($rows,"<td>$bbb</td><td></td>");
	      $i++;
	    }
	  }
	  $nrows=sizeof($rows);
	  if($nrows>0){
	    $row=array_shift($rows);
	    echo "  <tr class='$method'><td rowspan='$nrows'>$tid</td><td rowspan='$nrows'>$aa</td>$row</tr>\n";
	    foreach($rows as $row){
	      echo "  <tr class='$method'>$row</tr>\n";
	    }
	  }
	} else {
	  // json parse of result failed.
	  echo "<tr class='$method'><td>$tid</td><td>$aa</td><td>$response</td><td>$newResponse</td></tr>\n";	   
	}
      }
    }
  }
  echo "</table>\n";
 }


?>

</body>
</html>
