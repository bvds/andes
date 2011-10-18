#!/usr/bin/php
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
   <LINK REL=StyleSheet HREF="/log/log.css" TYPE="text/css" >
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
<table>
<table border=1 width="100%">
	     <tr><th>Turn</th><th>Action</th><th>Old Response</th><th>New Response</th><th>difference</th></tr>

<?php
	     // Send filtered set of transactions to server in
	     // chronological order.
	     // 
	     // Include handlers for Help system changes after 
	     // April 1, 2011.  This encompasses Raj's second study.
	     //
	     //  Clear test database:
	     //    use andes_test;
             //    DELETE FROM PROBLEM_ATTEMPT WHERE clientID LIKE '\_%';
	     //    REPLACE INTO CLASS_INFORMATION (classSection) values ('study');

	     // Still need to clean up logging:  consistent format
	     // Have interp for each red turn?

             // Start help server using 
             //         (start-help :db "andes_test")
	     // To get help server timing, need to set:
	     // (setf webserver:*debug* nil)
	     // (setf *simulate-loaded-server* nil)

$ignoreNewLogs = true;  // ignore any new non-error, log messages
$ignoreScores = true;  // ignore any changes to scoring.
$printDiffs = true;  // Whether to print out results for server diffs
$jsonFile = 'replies.json';  // File name for dumping reply json
	    
$dbserver = "localhost";
$dbLogin = split("\n",file_get_contents("../../db_user_password")); 	     
$dbuser = $dbLogin[0];   // $_POST['dbuser'];
$dbpass = $dbLogin[1];   // $_POST['passwd'];
$dbname = $dbLogin[2] ? $dbLogin[2] : 'andes3'; //$_POST['dbname'];

//CONNECTION STRING  
    
function_exists('mysql_connect') or die ("Missing mysql extension");
mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE"); 

/* filters for user name, section, etc.  
   Use regexp matching for user name and section. */
$sectionName = '' ; //$_POST['sectionName'];
$startDate = '2011-04-01'; // $_POST['startDate'];
$endDate = ''; // $_POST['endDate'];
$methods = array('open-problem','solution-step','seek-help','record-action','close-problem');  //implode(",",$_POST['methods']);

if($adminName==''){
  $adminNamec = "";
  $adminNamee = "";
 } else {
  $adminNamec = "P1.userName REGEXP '$adminName' AND";
  $adminNamee = " by $adminName,";
 }  
if($sectionName==''){
  $sectionNamec = "";
  $sectionNamee = "";
 } else {
  $sectionNamec = "P1.userSection REGEXP '$sectionName' AND";
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
$handle = fopen($jsonFile,'w');
fwrite($handle,"[\n");
$firstRow = true;

$studentTime = 0; // Total user time for all sessions.
$serverTime = 0; // Total server time for all sessions.

$sessionLink1 = "<td><a href=\"/log/OpenTrace.php?x=" . $dbuser .
	     "&amp;sv=" . $dbserver . "&amp;pwd=" . $dbpass . "&amp;d=" . $dbname;
$sessionLink2 ="\">Session&nbsp;log</a></td>";

$loadFailed = array();
$theProblem = array();
// Time for last turn in session, negative indicates idle.
$lastTime = array();
// Time used by student in each session
$sessionTime = array();
  
$sql = "SELECT S.tID,S.clientID,S.client,S.server FROM STEP_TRANSACTION AS S, PROBLEM_ATTEMPT as P1 WHERE $adminNamec $sectionNamec $extrac  $startDatec $endDatec S.clientID = P1.clientID ORDER BY S.tID";

// get student input and server reply
$result = mysql_query($sql);

// Wall clock time for sending to server.
$startTime=time();

while ($myrow = mysql_fetch_array($result)) {
  $clientID=$myrow["clientID"];
  $response=$myrow["server"];	 
  
  // If problem is closed by either student or server,
  // don't want to record any times after.
  if(preg_match('/"action":"problem-closed"/',$response)!=0){
    $lastTime[$clientID] = -2;  // Turn off timing for anything after
  }  
  
  if(isset($myrow["client"])) {
    $action=$myrow["client"];
    $ttID=$myrow["tID"];
  } else {
    $lastTime[$clientID] = -1;  // Turn off timing
    // Drop turns without client
    continue;
  }
  $a=$json->decode($action);
  // Drop turns without method.
  if(!isset($a->method)){
    $lastTime[$clientID] = -1;  // Turn off timing
    continue;
  }
  $method=$a->method;

  // Record problem name for this session
  if(strcmp($method,"open-problem")==0){
    $theProblem[$clientID] = $a->params->problem;
  }
  
  // If load has failed don't attempt any solution steps.
  if(array_key_exists($clientID,$loadFailed) &&
     $loadFailed[$clientID] &&
     in_array($method,$methods) && 
     strcmp($method,"close-problem")!=0){
    continue;
  } 
  
  // Send query to help server.
  $queryStart=microtime(true);       
  $newResponse = $server->message($action,$sessionIdBase . $clientID);
  $dt = microtime(true) - $queryStart;
  $serverTime += $dt;
  
  // Determine if problem load failed, set variable
  if(strcmp($method,"open-problem")==0 && 
     preg_match('/PROBLEM-LOAD-FAILED/',$newResponse)!=0){
    $loadFailed[$clientID] = true;
  }
  
  if(array_key_exists($clientID,$lastTime) &&
     $lastTime[$clientID]>= 0 &&
     isset($a->params->time) &&
     // Set cutoff for idle turn that is still in focus.
     $a->params->time - $lastTime[$clientID] < 5*60){
    $stepTime = $a->params->time - $lastTime[$clientID];
    $studentTime += $stepTime;
    if(array_key_exists($clientID,$sessionTime)){
      $sessionTime[$clientID] += $stepTime;
    } else {
      $sessionTime[$clientID]=0;
    }
  }    
  // Do not record time after closing problem, if missing timestamp,
  // or after "blur" event.
  if(array_key_exists($clientID,$lastTime) &&
     $lastTime[$clientID] == -2){
    // just leave it
  } elseif(!isset($a->params->time) || 
	   (strcmp($method,"record-action")==0 &&
	    isset($a->params->value) && 
	    is_string($a->params->value) && 
	    strcmp($a->params->value,"blur")==0)){  
    $lastTime[$clientID] = -1;
  } else {
    $lastTime[$clientID]=$a->params->time;
  }

  if(isset($methodTime[$method])){
    $methodTime[$method] += $dt;
  } else {
    $methodTime[$method] = $dt;
  }
  $timeLink[]=array("dt"=>$dt, "method"=>$method,
		    "link"=>$sessionLink1 . "&amp;t=" . $ttID . 
		    "&amp;cid=" . $clientID . 
		    $sessionLink2
		    );
  
  if(isset($a->id)){
    $tid=$a->id;
  } else {
    $tid="none";
  }
  
  if($printDiffs && 
     isset($a->params) && // Ignore server shutdown of idle sessions.
     (!$methods || in_array($method,$methods))){
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
	while($i<$imax || $ni<$nimax){
	  
	  if($i<$imax){
	    $bc=$jr->result[$i];  // copy used for compare
	    $bb=$json->encode($bc); // print this out
	    
	    // Remove any backtraces from compare.
	    unset($bc->backtrace);
	    $bbc=$json->encode($bc);
	  } else {
	    $bc=(object) null;
	    $bb='';
	    $bbc='';
	  }
	  
	  if($ni<$nimax){
	    $nbc=$njr->result[$ni];  // copy used for compare
	    $nbb=$json->encode($nbc);  // print this out
	    
	    // Remove any backtraces from compare.	
	    unset($nbc->backtrace);
	    $nbbc=$json->encode($nbc);
	  } else {
	    $nbc=(object) null;
	    $nbb='';
	    $nbbc='';
	  }
	  
	  /*
	   * Canonicalize strings that are compared, accounting
	   * for some of the known differences.
	   */
	  // Superfluous newlines added to log entries.
	  // See Bug #1915
	  $bbc=str_replace("\\n",'',$bbc);
	  $nbbc=str_replace("\\n",'',$nbbc);
	  // Remove double precision notation from constants.
	  // This difference occurs from using slime vs. stand-alone. 
	  $bbc=preg_replace('/(\d)d0/','$1',$bbc);
	  // Canonicalize random positive feedback.
	  $randomPositive = '/(Good!|Right\.|Correct\.|Yes\.|Yep\.|That.s right\.|Very good\.|Right indeed.) /';
	  $randomPostiveNew='**random-positive** ';
	  $bbc=preg_replace($randomPositive,$randomPostiveNew,$bbc);
	  $nbbc=preg_replace($randomPositive,$randomPostiveNew,$nbbc);
	  // Canonicalize random goal prefix
	  $randomPrefix = '/(Try|You should be|A good step would be|Your goal should be) /';
	  $randomPrefixNew='**random-prefix** ';
	  $bbc=preg_replace($randomPrefix,$randomPrefixNew,$bbc);
	  $nbbc=preg_replace($randomPrefix,$randomPrefixNew,$nbbc);
	  // This shouldn't happen, but some assoc operators hav
	  // unbound variables which show the internal binding.
	  $varUnique='/#:\?[\-\w]+/';
	  $bbc=preg_replace($varUnique,'?VAR',$bbc);
	  $nbbc=preg_replace($varUnique,'?VAR',$nbbc);	    
	  // Canonicalize end of sentence.
	  // commit a3d4eee566d6b98d, June 2, 2011 (and others)
	  $bbc=preg_replace('/\.\s+/','. ',$bbc);
	  $nbbc=preg_replace('/\.\s+/','. ',$nbbc);
	  $bbc=preg_replace('/\.&nbsp;\s+/','. ',$bbc);
	  $nbbc=preg_replace('/\.&nbsp;\s+/','. ',$nbbc);
	  // new-user-dialog
          // commit 212960a3c713fe, Tue Sep 20 17:04:41 2011
	  $bbc=preg_replace('/After watching the video, just follow the instructions/','Just follow the instructions',$bbc);
	  $nbbc=preg_replace('/After watching the video, just follow the instructions/','Just follow the instructions',$nbbc);
	  // kgraph8b problem change
	  // problems, commit fb5ec251c3a974a14, Tue Sep 27 2011
	  // Labeling of objects shifted.
	  if(strcmp($theProblem[$clientID],"kgraph8b")==0){
	    $bbc=preg_replace('/"statement\d+"/','"statementnnn"',$bbc);
	    $nbbc=preg_replace('/"statement\d+"/','"statementnnn"',$nbbc);
	    $bbc=preg_replace('/"y":\d+/','"y":nnn',$bbc);
	    $nbbc=preg_replace('/"y":\d+/','"y":nnn',$nbbc);
	  }
	  
	  if(strcmp($bbc,$nbbc)==0){ // match, go on to next pair
	    $i++; $ni++;
	  }elseif($ignoreScores && isset($bc->action) && 
		  strcmp($bc->action,"set-score")==0){
	    $i++;  // Old turn has score.
	  }elseif($ignoreScores && isset($nbc->action) &&
		  strcmp($nbc->action,"set-score")==0){
	    $ni++;  // New turn has score.
	  }elseif($ignoreScores && isset($bc->action) &&
		  strcmp($bc->action,"log")==0 &&
		  (isset($bc->subscores) || // old log format
		   (isset($bc->log) && strcmp($bc->log,"subscores")==0))){
	    $i++;  // Old turn has subscores.
	  }elseif($ignoreScores && 
		  isset($nbc->action) && isset($nbc->log) &&
		  strcmp($nbc->action,"log")==0 &&
		  strcmp($nbc->log,"subscores")==0){
	    $ni++;  // New turn has subscores.
	  }elseif(
		  // kgraph8b problem change
		  // problems, commit fb5ec251c3a974a14, Tue Sep 27 2011
		  strcmp($theProblem[$clientID],"kgraph8b")==0 &&
		  isset($bc->text) &&
		  strcmp($bc->text,"Assume the positive x-axis is pointing towards the right.")==0){
	    $i++;
	  }elseif(
		  // kgraph8b problem change
		  // problems, commit fb5ec251c3a974a14, Tue Sep 27 2011
		  strcmp($theProblem[$clientID],"kgraph8b")==0 &&
		  isset($nbc->text) &&
		  strcmp($nbc->text,"Assume the positive x-axis is pointing towards the right.")==0){
	    $ni++;
	  }elseif(
		  // User agent will change when rerunning.
		  // Drop any log of user-agent.
		  isset($bc->action) && isset($bc->log) &&
		  strcmp($bc->action,"log")==0 &&
		  strcmp($bc->log,"user-agent")==0){
	    $i++;
	  }elseif(
		  // User agent will change when rerunning.
		  // Drop any log of user-agent.
		  isset($nbc->action) && isset($nbc->log) &&
		  strcmp($nbc->action,"log")==0 &&
		  strcmp($nbc->log,"user-agent")==0){
	    $ni++;
	  }elseif($imax-$i == $nimax-$ni){ // mismatch, but same remaining
	    // Position of first discrepency
	    $pos=strspn($bbc ^ $nbbc, "\0");
	    $bbb=escapeHtml($bb);
	    $nbbb=escapeHtml($nbb);
	    array_push($rows,"<td>$bbb</td><td>$nbbb</td><td>pos. $pos<br>" .
		       escapeHtml(substr($bbc,$pos,8)) . "<br>" . 
		       escapeHtml(substr($nbbc,$pos,8)) . "</td>");
	    $i++; $ni++;
	  }elseif($imax-$i < $nimax-$ni){ // mismatch, extra row in new
	    $nbbb=escapeHtml($nbb);
	    array_push($rows,"<td></td><td>$nbbb</td><td></td>");
	    $ni++;
	  }else{  //mismatch, extra row in old
	    $bbb=escapeHtml($bb);
	    array_push($rows,"<td>$bbb</td><td></td><td></td>");
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
	
	// Dump reply result to file for testing json against smd
	// See check-json.html
	if($firstRow){
	  $firstRow=false;
	} else {
	  fwrite($handle,",\n");
	}
	fwrite($handle,"{\"method\":\"$method\",\"reply\":" .
	       $json->encode($njr->result) . "}");
	
      } else {
	// json parse of result failed.
	echo "<tr class='$method'><td>$tid</td><td>$aa</td><td>$response</td><td>$newResponse</td><td></td></tr>\n";	   
      }
    }
  }
 }

fwrite($handle,"\n]\n");
fclose($handle);

echo "</table>\n";
echo "<p>Student time " . number_format($studentTime,2) . ", \n";
echo "server time " . number_format($serverTime,2) . ", \n";
echo "and wall time " . (time()-$startTime) . " seconds.<br>\n";

echo "Server time usage for each method:<br>\n";
echo "<table border=1>\n";
echo "<tr><th>Method</th><th>time (s)</th></tr>\n";
foreach ($methodTime as $method => $time){
  echo "<tr class='$method'><td>$method</td><td>" . 
    number_format($time,2) . "</td></tr>\n";
}
echo "</table><br>\n";

echo "Turns with largest latency:<br>\n";
function cmp($a,$b){
  if($a["dt"]==$b["dt"]){
    return 0;
  }
  return ($a["dt"] > $b["dt"]) ? -1 : 1; // descending
}
usort($timeLink,"cmp");
echo "<table border=1>\n";
echo "<tr><th>latency (s)</th><th>Session</th></tr>\n";
foreach(array_splice($timeLink,0,50) as $val){
  echo "<tr class='" . $val["method"] . "'><td>" . 
    number_format($val["dt"],2) . "</td>" .
    $val["link"] . "</tr>\n";
}
echo "</table>\n";

echo "<p>Longest sessions:<br>\n";
arsort($sessionTime);
echo "<table border=1>\n";
echo "<tr><th>time (s)</th><th>Session</th></tr>\n";
foreach(array_splice($sessionTime,0,20) as $key => $value){
  echo "<tr><td>" . $value . "</td>" . 
    $sessionLink1 . "&amp;cid=" . $key . $sessionLink2 . "</tr>\n";
}
echo "</table>\n";

?>

</body>
</html>
