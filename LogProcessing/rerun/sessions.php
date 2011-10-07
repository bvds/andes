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

<?php
	     // Send filtered set of sessions to server in
	     // order of session start time.

	     //  Clear test database:
	     //    use andes_test;
             //    DELETE FROM PROBLEM_ATTEMPT WHERE clientID LIKE '\_%';
	     //    REPLACE INTO CLASS_INFORMATION (classSection) values ('study');

	     // Still need to clean up logging:  consistent format
	     // Have interp for each red turn?

	     // To get server timing, need to set:
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

/* filters for user name, section, etc.  */
$adminName = ''; // $_POST['adminName'];   student name.
$sectionName = 'study' ; //$_POST['sectionName'];
$startDate = ''; // $_POST['startDate'];
$endDate = ''; // $_POST['endDate'];
$methods = array('open-problem','solution-step','seek-help','record-action','close-problem');  //implode(",",$_POST['methods']);

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

// Use tagged warnings and errors
// commit 7c0e033a62efbf24a4, March 3, 2011
function match_error_messages($old,$new){
  if(preg_match('/interpret-equation: no interpretation for/',$old) &&
     preg_match('/INTERPRET-EQUATION-MISSING/',$new)){
    return true;
  }
  if(preg_match('/does not exist, creating new object/',$old) &&
     preg_match('/MODIFY-NON-EXISTANT-OBJECT/',$new)){
    return true;
  }
  if(preg_match('/iea-alternate-axes-prompt response/',$old) &&
     preg_match('/IEA-ALTERNATE-AXES-PROMPT/',$new)){
    return true;
  }
  if(preg_match('/inaccurate in parse-handler/',$old) &&
     preg_match('/PARSE-HANDLER-INACCURATE/',$new)){
    return true;
  }
  if(preg_match('/Bad symbol definition/',$old) &&
     preg_match('/SYMBOL-DEFINITION-MISMATCH/',$new)){
    return true;
  }
  if(preg_match('/Unable to load problem/',$old) &&
     preg_match('/PROBLEM-LOAD-FAILED/',$new)){
    return true;
  }
  return false;
}


// loop through result array and consolidate student interps.
// Before March 8, 2011, student interps were
// placed in separate rows.
function consolidate_interps($result){
  $imax=sizeof($result);
  $studentLog=array(); $tmp=array();
  $firstLog=-1;
  $j=0;
  for($i=0; $i<$imax; $i++){
    $bc=$result[$i];
    if(strcmp($bc->action,"log")==0 && 
       !isset($bc->backtrace)){
      if($firstLog<0){
	$firstLog=$i;
	$j++;
      }
      foreach($bc as $key => $value){
	$studentLog[$key]=$value;
	// Want to place log right after action so
	// string matching works.
	if(strcmp($key,'action')==0){
	  $studentLog['log']='student';
	}
      }
    }else{
      $tmp[$j++]=$bc;
    }
  }
  if($firstLog>=0){
    $tmp[$firstLog]=(object) $studentLog;
  }
  return $tmp;
}
	     
require_once('jsonRPCClient.php');
$server  = new jsonRPCClient('http://localhost/help');
$sessionIdBase = "_" . date('h:i:s') . "_";
$sessionId = 0;
$handle = fopen($jsonFile,'w');
fwrite($handle,"[\n");
$firstRow = true;

$studentTime = 0; // Total user time for all sessions
$serverTime = 0; // Total server time for all sessions.
  
$sql = "SELECT * FROM PROBLEM_ATTEMPT AS P1 WHERE $adminNamec $sectionNamec $extrac  $startDatec $endDatec P1.clientID = P1.clientID ORDER BY startTime";

$result = mysql_query($sql);

while ($myrow = mysql_fetch_array($result)) {
  $userName=$myrow["userName"];
  $userProblem=$myrow["userProblem"];
  $userSection=$myrow["userSection"];
  $startTime=$myrow["startTime"];
  $clientID=$myrow["clientID"];
  $sessionLink1 = "<td><a href=\"/log/OpenTrace.php?x=" . $dbuser .
    "&amp;sv=" . $dbserver . "&amp;pwd=" . $dbpass . "&amp;d=" . $dbname .
    "&amp;cid=" . $clientID . "&amp;u=" . $userName . "&amp;p=" . 
    $userProblem . "&amp;s=" . $userSection .  "&amp;m=";
  $sessionLink2 ="\">Session&nbsp;log</a></td>";

  echo "<p>User:&nbsp; $userName, Problem: &nbsp; $userProblem, Section:&nbsp; $userSection Start:&nbsp; $startTime\n";
  if($printDiffs){
    echo "<table border=1 width=\"100%\">";
    echo "<tr><th>Turn</th><th>Action</th><th>Old Response</th><th>New Response</th></tr>\n";
  }
  
  $tempSqlOld = "SELECT initiatingParty,command,tID FROM PROBLEM_ATTEMPT_TRANSACTION WHERE clientID = '$clientID'";
  $tempSql = "SELECT client,server,tID FROM STEP_TRANSACTION WHERE clientID = '$clientID'";
  $tempResultOld = mysql_query($tempSqlOld);
  $tempResult = mysql_query($tempSql);
  $sessionId++; 
  $ttime = 0;  // User time for this session
  $loadFailed = false;
  
  // get student input and server reply
  while (($myrow1 = mysql_fetch_array($tempResult)) ||
	 // Old style with PROBLEM_ATTEMPT_TRANSACTION
	 (($myrow1 = mysql_fetch_array($tempResultOld)) &&
	  ($myrow2 = mysql_fetch_array($tempResultOld)))) {
    if(isset($myrow1["command"])){
      if($myrow1["initiatingParty"]=='client'){
	$action=$myrow1["command"];
	$ttID=$myrow1["tID"];
	$response=$myrow2["command"];
      } else {
	$action=$myrow2["command"];
	$ttID=$myrow2["tID"];
	$response=$myrow1["command"];
      }
    } else if(isset($myrow1["client"])) {
      $action=$myrow1["client"];
      $ttID=$myrow1["tID"];
      $response=$myrow1["server"];	 
    } else {
      // For new style, drop turns without client
      continue;
    }
    $a=$json->decode($action);
    // Drop turns without method.
    if(!isset($a->method)){
      continue;
    }
    $method=$a->method;

    // If load has failed don't attempt any solution steps.
    if($loadFailed && in_array($method,$methods) && 
       strcmp($method,"close-problem")!=0){
      continue;
    } 

    // Send query to help server.
    $queryStart=microtime(true);       
    $newResponse = $server->message($action,$sessionIdBase . $sessionId);
    $dt = microtime(true) - $queryStart;
    $serverTime += $dt;

    // Determine if problem load failed, set variable
    if(strcmp($method,"open-problem")==0 && 
       preg_match('/PROBLEM-LOAD-FAILED/',$newResponse)!=0){
      $loadFailed = true;
    }

    // Problem might be closed later by server.
    // No attempt here to remove "out of focus" since we
    // don't have that working yet.
    if(isset($a->params->time) && strcmp($method,"close-problem")!=0){  
      $ttime=$a->params->time;
    }
    if(isset($methodTime[$method])){
      $methodTime[$method] += $dt;
    } else {
      $methodTime[$method] = $dt;
    }
    $timeLink[]=array("dt"=>$dt, "time"=>$ttime, "method"=>$method,
		      "link"=>$sessionLink1 . "&amp;t=" . $ttID . 
		      $sessionLink2);

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

	  // Old logs spread student interpretations over
	  // several lines.
	  $jr->result=consolidate_interps($jr->result);
	  $imax=sizeof($jr->result);

	  $rows=array();
	  $i=0; $ni=0;
	  while($i<$imax || $ni<$nimax){
	    if($i<$imax){
	      $bc=$jr->result[$i];  // copy used for compare
	      $bb=$json->encode($bc); // print this out
	      
	      // Remove dummy url from close-problem
	      // commit 512d1492822, Jan 5, 2011
	      if(strcmp($bc->action,"problem-closed")==0){
		unset($bc->url);
	      }
	      
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
	    // commit c828125f4bc0e6, Jan. 5, 2011 (and others)
	    $bbc=preg_replace('/\.\s+/','. ',$bbc);
	    $nbbc=preg_replace('/\.\s+/','. ',$nbbc);
	    $bbc=preg_replace('/\.&nbsp;\s+/','. ',$bbc);
	    $nbbc=preg_replace('/\.&nbsp;\s+/','. ',$nbbc);
	    
	    // Add period to end of sentence.
	    // commit 7227bfa4b73c091d130b79bfcd6c, Nov. 25, 2010
	    // Escaping for php regexps seems to be buggy ...
	    $bbc=preg_replace('/(Undefined variable.*?<..var>)\\"/','$1."',$bbc);
	    // commit 14db0660b489c3c2, Nov 19, 2010
            // Add logging for window clicks.
	    $bbc=preg_replace('/andes\.help\.link.*?;/','',$bbc);
	    $nbbc=preg_replace('/andes\.help\.link.*?;/','',$nbbc);
	    
	    // Add marker for syntax error
	    // commit c0f8084f1c41886a17c968e, Nov. 25, 2010
            // Couldn't figure out match for two characters \/, possible bug
	    // in php?
	    $unparsed='/<span class=\\\"unparsed\\\">(.*?)<..span>/';
	    $bbc=preg_replace($unparsed,'$1',$bbc);
	    $nbbc=preg_replace($unparsed,'$1',$nbbc);
	    
	    // andes.principles.review modification
	    // commit 026a072d1471bd5b17, Feb 4, 2011
	    $manualLink='/andes\.principles\.review.*?;/';
	    $manualHolder='**manual**';
	    $bbc=preg_replace($manualLink,$manualHolder,$bbc);
	    $nbbc=preg_replace($manualLink,$manualHolder,$nbbc);
	    
	    // Wrapper <var>...</var> has been added to more cases
	    // commit c828125f4bc0e6de5b, Jan. 5, 2011
            // commit 3425f36e509234504b38, Dec. 8, 2010
	    $bbc=preg_replace('/<var>(.*?)<..var>/','$1',$bbc);
	    $nbbc=preg_replace('/<var>(.*?)<..var>/','$1',$nbbc);
	    
	    // Change hint wording for unmatched sought.
	    // January 7, 2011
	    $bbc=preg_replace('/Your entry does not match any quantities needed to solve this problem/','**unmatched-sought**',$bbc);
	    $nbbc=preg_replace('/Sorry, I was not able to understand your entry/','**unmatched-sought**',$nbbc);
	    
	    // Last hint for NSH sought
	    // eb5239f490dbb5, Jan 10, 2011
	    $nbbc=preg_replace('/Please try one more time/','Please try again',$nbbc);
	    // Update format for untagged error messages.
	    // commit 7c0e033a62efbf24a4, March 3, 2011
	    if(isset($bc->action) && 
	       strcmp($bc->action,"log")==0 && isset($bc->error) &&
	       !isset($nbc->entry)){  // change only untagged errors
	      $bbc=preg_replace('/"action":"log",/',
				'"action":"log","log":"server",',$bbc);
	      $bbc=preg_replace('/"error":/','"text":',$bbc);
	    }
	    
	    // fix typo in vec1ay
	    // problems commit 477931a39be6, March 7, 2011
	    $bbc=preg_replace('/usethe/','use the',$bbc);
	    
	    // Old logs don't distinguish between log:student and log:tutor
	    // Turn everything to log:student in new logs
	    // Logs after March 9, 2011 do.
	    if(isset($bc->action) && isset($nbc->action) &&
	       strcmp($bc->action,"log")==0 &&
	       strcmp($nbc->action,"log")==0 && 
	       preg_match('/"log":"student"/',$bbc)){
	      $nbbc=preg_replace('/"log":"tutor"/','"log":"student"',$nbbc);
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
		    // Use tagged warnings and errors
		    // commit 7c0e033a62efbf24a4, March 3, 2011
		    isset($bc->action) && isset($nbc->action) &&
		    strcmp($bc->action,"log")==0 && 
		    strcmp($nbc->action,"log")==0 && 
		    isset($bc->error) && isset($nbc->entry) &&
		    match_error_messages($bc->error,$nbc->entry)){
	      $i++; $ni++; 
	    }elseif(
		    // log user agent
		    // commit e08a0b2487d7b6a, Feb 26, 2011
		    isset($bc->action) && isset($bc->log) && 
		    strcmp($bc->action,"log")==0 && 
		    strcmp($bc->log,"user-agent")==0){
	      $i++;
	    }elseif(
		    isset($nbc->action) && isset($nbc->log) &&
		    strcmp($nbc->action,"log")==0 && 
		    strcmp($nbc->log,"user-agent")==0){
	      $ni++;
	    }elseif(
		    // New hints for empty definition
		    // commit 53a456b8c04e358, Dec. 1, 2010
		    isset($bc->action) && isset($nbc->action) && 
		    strcmp($bc->action,"modify-object")==0 &&
		    strcmp($nbc->action,"show-hint")==0 &&
		    isset($bc->mode) && strcmp($bc->mode,"incorrect")==0 &&
		    (preg_match('/Generally, objects should be labeled./',
				$nbc->text) ||
		     preg_match('/You should.*define a variable/',
				$nbc->text))){ 
	      // Also, skip over immediately following link.
	      $i++; $ni+=2; 
	    }elseif($imax-$i == $nimax-$ni){ // mismatch, but same remaining
	      $bbb=escapeHtml($bb);
	      $nbbb=escapeHtml($nbb);
	      array_push($rows,"<td>$bbb</td><td>$nbbb</td>");
	      $i++; $ni++;
	    }elseif($imax-$i < $nimax-$ni){ // mismatch, extra row in new
	      // drop new log messages 
	      // commit ea69ab06944, Jan 4, 2011, "EQUATION-SYNTAX-ERROR":"WRONG-TOOL"
	      // commit 53a456b8c04, Dec 1, 2010, "NO-VARIABLE-DEFINED":"NIL"
	      //                                   NO-LABEL
	      // commit 220f3bcff2f, Nov 19, 2010, EQUATION-SYNTAX-ERROR
	      // See bug #1870

	      if(!($ignoreNewLogs && 
		   isset($nbc->action) && isset($nbc->log) &&
		   strcmp($nbc->action,"log")==0 &&
		   strcmp($nbc->log,"tutor")==0)){
		$nbbb=escapeHtml($nbb);
		array_push($rows,"<td></td><td>$nbbb</td>");
	      }
	      $ni++;
	    }else{  //mismatch, extra row in old
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
	  echo "<tr class='$method'><td>$tid</td><td>$aa</td><td>$response</td><td>$newResponse</td></tr>\n";	   
	}
      }
    }
  }
  if($printDiffs){
    echo "</table>\n";
  }
  $studentTime += $ttime;
 }

fwrite($handle,"\n]\n");
fclose($handle);

echo "<p>Student time " . number_format($studentTime,2) . 
     " and server time " . number_format($serverTime,2) . " seconds.<br>\n";

echo "Server time usage for each method:<br>\n";
echo "<table border=1>\n";
echo "<tr><th>Method</th><th>time (s)</th></tr>\n";
foreach ($methodTime as $method => $time){
  echo "<tr class='$method'><td>$method</td><td>" . 
    number_format($time,2) . "</td></tr>\n";
}
echo "</table><br>";

echo "Turns with largest latency:<br>\n";
function cmp($a,$b){
  if($a["dt"]==$b["dt"]){
    return 0;
  }
  return ($a["dt"] > $b["dt"]) ? -1 : 1; // descending
}
usort($timeLink,"cmp");
echo "<table border=1>\n";
echo "<tr><th>latency (s)</th><th>Session<br>time</th><th>Session</th></tr>\n";
foreach(array_splice($timeLink,0,50) as $val){
  echo "<tr class='" . $val["method"] . "'><td>" . 
    number_format($val["dt"],2) . "</td><td>" . 
    number_format($val["time"],2) . "</td>" .
    $val["link"] . "</tr>\n";
}
echo "</table>";

?>

</body>
</html>
