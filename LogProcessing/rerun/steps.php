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

             // Lisp with space:  sbcl --dynamic-space-size 1000
             // Start help server using 
             //         (start-help :db "andes_test")
	     // To get help server timing, need to set:
	     // (setf webserver:*debug* nil)
	     // (setf *simulate-loaded-server* nil)

$ignoreNewLogs = true;  // ignore any new non-error, log messages
$ignoreScores = true;  // ignore any changes to scoring.
$ignoreMetaHints = true;  // Ignore meta hints
$ignorePreferences = true; // Ignore any client preferences
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
//         rking_asu
$adminName = '' ;   // user name
	     // MIT_.*
	     // asu_3u16472755e704e5fasul1_.*
	     // asu_3u16472755e704e5fasul1_15865
$sectionName = 'asu_9Q1920841f2ca4d1fasul1_.' ; //$_POST['sectionName'];
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

  // Changed extra from number to string or null
  // commit a0719d09f0017cb, Nov 9, 2011
$extrac = "(P1.extra IS NULL or P1.extra = 0) AND";
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
  // Would be nice to leave in valid html and
  // escape everything else.
  // Escape html codes so actual text is seen.
  $bb=str_replace("&","&amp;",$bb);
  $bb=str_replace(">","&gt;",$bb);
  $bb=str_replace("<","&lt;",$bb);
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
$stepTally = 0; // Number of steps analyzed
$diffStepTally = 0;  // Number of steps where unexplained diff was found.

$sessionLink1 = "<td><a href=\"/log/OpenTrace.php?x=" . $dbuser .
	     "&amp;sv=" . $dbserver . "&amp;pwd=" . $dbpass . "&amp;d=" . $dbname;
$sessionLink2 ="\">Session&nbsp;log</a></td>";

$loadFailed = array();
$theProblem = array();
// Time for last turn in session, negative indicates idle.
$lastTime = array();
// Time used by student in each session
$sessionTime = array();

// Test if text corresponds to a meta-hint
function containsMetaHint($t){
  return strpos($t,'introductory video') !== false ||
    strcmp($t,"You can click on the above link to get more help.") ==0 ||
    (strpos($t,"Your entry has turned red") !== false &&
     strpos($t,"the hint button") !== false);
}

// Test for known student errors.
function containsErrorType($p,$ans){
  // Errors with unsolicited hints were logged in old
  // log files.  There are commented out here:
  $normalErrs = array(
		    "(DEFAULT-WRONG-DIR ",
		    "(already-defined)",
		    "(answer-is-malformed)",
		    "(answer-is-not-sought)",
		    "(answer-sought-is-undefined)",
		    "(definition-has-no-matches)",
		    "(definition-has-too-many-matches)",
		    "(empty-answer)",
		    //  "(equation-syntax-error ",
		    "(extra-answer)",
		    // "(forgot-units)",
		    "(goal-incomplete ",
		    "(internal-error ",
		    // "(maybe-forgot-units)",
		    "(more-than-given)",
		    "(no-label ",
		    "(no-variable-defined)",
		    "(nothing-to-match-definition)",
		    "(should-be-compo-form)",
		    "(should-be-given)",
		    "(should-be-known)",
		    "(should-be-magdir-form)",
		    "(should-be-unknown)",
		    "(solve-for-var ",
		    // "(Undefined-variables ",
		    // "(Unused-variables ",
		    "(using-variables-in-answer)",
		    "(variable-already-in-use)",
		    "(variable-not-defined)",
		    "(wrong-given-value)",
		    "(wrong-tool-error)"
		    // "(wrong-units "
		    );
  $ansErrs=array("(ANSWER-SOUGHT-IS-UNDEFINED)",
		 // "(FORGOT-UNITS)",
		 "(DEFAULT-WRONG-ANSWER ",
		 "(MISSING-NEGATION-ON-VECTOR-MAGNITUDE ",
		 "(Undefined-variables ",
		 "(WRONG-UNITS)"
		 );
  foreach (($ans?$ansErrs:$normalErrs) as $errName){
    if(strncasecmp($p,$errName,strlen($errName))==0){
      return true;
    }
  }
  return false;
}


//  Initial query to determine when sessions end
//  If the server crashes, then any sessions running will be
//  left open according to the log files.

$sqlFilter = "FROM STEP_TRANSACTION AS S, PROBLEM_ATTEMPT as P1 WHERE $adminNamec $sectionNamec $extrac  $startDatec $endDatec S.clientID = P1.clientID ORDER BY S.tID";
$sql = "SELECT S.tID,S.clientID,P1.startTime " . $sqlFilter;
// get student input and server reply
$result = mysql_query($sql);
$lastID=array();
while ($myrow = mysql_fetch_array($result)) {
  $tID=$myrow["tID"];
  $clientID=$myrow["clientID"];
  $lastID[$clientID]=$tID;
  $sessionStartTime[$clientID]=$myrow["startTime"];
 }

//  Now query to get posts and replies
$sql = "SELECT S.tID,S.clientID,S.client,S.server " . $sqlFilter;
// get student input and server reply
$result = mysql_query($sql);

// Wall clock time for sending to server.
$startTime=time();

$closeSession=array();
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

  // See if this is the last turn in a session 
  // Or if the server has closed the session and
  // the problem has not already been closed.
  // Then close the session.
  if(($lastID[$clientID] == $ttID || 
      strpos($response,"Your session is no longer active.") !== false) &&
     !(array_key_exists($clientID,$lastTime) &&
       $lastTime[$clientID] == -2)){
    // Close the session "by hand" and record
    $closeID=$a->id + 1;
    $closeAction="{\"id\":$closeID,\"method\":\"close-problem\",\"params\":{},\"jsonrpc\":\"2.0\"}";
    $closeResponse = $server->message($closeAction,$sessionIdBase . $clientID);
    $closeTime=$sessionStartTime[$clientID];
    $closeSession[$clientID]="<td>$closeTime</td>" . $sessionLink1 . "&amp;cid=" . 
      $clientID . $sessionLink2 . "<td>$closeResponse</td>";
  }
  
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
    $stepTally++;
    $aa=$json->encode($a->params);
    $aa=escapeHtml($aa);
    
    if (strcmp($response,$newResponse) != 0) {
      $jr=$json->decode($response);
      $njr=$json->decode($newResponse);
      $aaa = "<a href=\"/log/OpenTrace.php?x=$dbuser&amp;sv=$dbserver&amp;pwd=$dbpass&amp;d=$dbname&amp;cid=$clientID&amp;t=$ttID\">$tid</a>";
      if($jr == null || $njr == null){
	// One of the json decodes fails, can't compare old
	// and new response.
	    // Position of first discrepency
	    $pos=strspn($response ^ $newResponse, "\0");
	    echo "<tr class='syntax'><td>$aaa</td><td>$aa</td>" . 
	      "<td>" . ($jr== null?"json decode failed":"OK") . 
	      "<br>len. " . strlen($response) . "</td>" .
	      "<td>" . ($njr== null?"json decode failed":"OK") . 
	      "<br>len. " . strlen($newResponse) . "</td>" .
	      "<td>pos. $pos</td></tr>\n";
	    $diffStepTally++;
      } elseif(isset($jr->error) || isset($njr->error)){
	echo "<tr class='$method'><td>$aaa</td><td>$aa</td><td>$response</td><td>$newResponse</td><td></td></tr>\n"; 
	$diffStepTally++;
      } else {
	// Server drops result key-value pair if array is empty.
	if(!isset($jr->result)){$jr->result=array();}
	if(!isset($njr->result)){$njr->result=array();}
	
	// Loop through old result and remove unwanted rows
	$bcr=array();
	foreach($jr->result as $bc){
	  if(!(
	       // Remove hints from open-problem:  these are
	       // generally from rerunning old sessions through 
	       // help, and have already been evaluated.
	       (strcmp($method,"open-problem")==0 && isset($bc->action) && 
		(strcmp($bc->action,"show-hint") == 0 || 
		 strcmp($bc->action,"show-hint-link") == 0)) ||

	       
	       // Old turn has score.
	       ($ignoreScores && isset($bc->action) && 
		strcmp($bc->action,"set-score")==0) ||
	       
	       // Old turn has subscores.
	       ($ignoreScores && isset($bc->action) &&
		strcmp($bc->action,"log")==0 &&
		(isset($bc->subscores) || // old log format
		 (isset($bc->log) && strcmp($bc->log,"subscores")==0))) ||
	     
	       // Old turn has meta-hint
	       ($ignoreMetaHints && isset($bc->action) &&
		strcmp($bc->action,"show-hint")==0 &&
		containsMetaHint($bc->text)) ||

	       // Ignore client preferneces
	       ($ignorePreferences && isset($bc->action) &&
		strcmp($bc->action,"set-preference")==0) ||

	       // Add log message to all incorrect entries
	       // commit 4889f5fb386998, Dec 17 20:17:21 2011
	       // Remove redundant log message.
	       (isset($bc->action) && isset($bc->log) && 
		isset($bc->{'error-type'}) &&
		strcmp($bc->action,"log")==0 &&
		strcmp($bc->log,"student")==0 &&
		strcmp($bc->{'error-type'},"(SYNTAX-ERROR-IN-EQN)")==0) ||
	       
	       // User agent will change when rerunning.
	       // Drop any log of user-agent.
	       (isset($bc->action) && isset($bc->log) &&
		strcmp($bc->action,"log")==0 &&
		strcmp($bc->log,"user-agent")==0)

	       )){
	    array_push($bcr,$bc);
	  }
	}
	$jr->result=$bcr;
	
	// Loop through new result and remove unwanted rows
	$nbcr=array();
	foreach($njr->result as $bc){
	  if(!(	       
	       // Remove hints from open-problem:  these are
	       // generally from rerunning old sessions through 
	       // help, and have already been evaluated.
	       (strcmp($method,"open-problem")==0 && isset($bc->action) && 
		(strcmp($bc->action,"show-hint") == 0 || 
		 strcmp($bc->action,"show-hint-link") == 0)) ||

	       // New turn has score.
	       ($ignoreScores && isset($bc->action) &&
		strcmp($bc->action,"set-score")==0) ||
	       
	       // New turn has subscores.
	       ($ignoreScores && isset($bc->action) &&
		strcmp($bc->action,"log")==0 &&
		(isset($bc->subscores) || 
		 (isset($bc->log) && strcmp($bc->log,"subscores")==0))) ||
	     
	       // New turn has meta-hint
	       ($ignoreMetaHints && 
		isset($bc->action) &&
		strcmp($bc->action,"show-hint")==0 &&
		containsMetaHint($bc->text)) ||

	       // Ignore client preferneces
	       ($ignorePreferences && isset($bc->action) &&
		strcmp($bc->action,"set-preference")==0) ||
	       
	       // Add log message to all incorrect entries
	       // commit 4889f5fb386998, Dec 17 20:17:21 2011
	       (strcmp($method,"solution-step")==0 && 
		isset($bc->action) && isset($bc->log) && 
		isset($bc->{'error-type'}) &&
		strcmp($bc->action,"log")==0 &&
		strcmp($bc->log,"student")==0 &&
		containsErrorType($bc->{'error-type'},
				  isset($a->params->symbol) &&
				  strcmp($a->params->symbol,"Answer")==0)) ||
	       
	       // User agent will change when rerunning.
	       // Drop any log of user-agent.
	       (isset($bc->action) && isset($bc->log) &&
		strcmp($bc->action,"log")==0 &&
		strcmp($bc->log,"user-agent")==0))
	     ){
	    array_push($nbcr,$bc);
	  }
	}
	$njr->result=$nbcr;
	
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
	  $nbbc=preg_replace('/(\d)d0/','$1',$nbbc);
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
	  // Remove spaces at end of make-explain-more hints 
	  // in Help/NextStepHelp.cl
	  // Dec. 1 2011
	  if(isset($bc->action) && strcmp($bc->action,"show-hint")==0){
	    $bbc=preg_replace('/\.[ ]+"/','."',$bbc);
	    $bbc=preg_replace('/\.&nbsp;[ ]*"/','."',$bbc);
	  }
	  // Canonicalize indy equation numbers.
	  // Not sure what is causing different numbering.
	  if(isset($bc->action) && strcmp($bc->action,"log")==0){
	    $bbc=preg_replace('/indyStudentAddEquationOkay..\d+ /','/indyStudentAddEquationOkay((0 ',$bbc);
	  }
	  if(isset($nbc->action) && strcmp($nbc->action,"log")==0){
	    $nbbc=preg_replace('/indyStudentAddEquationOkay..\d+ /','/indyStudentAddEquationOkay((0 ',$nbbc);
	  }
	  
	  
	  if(strcmp($bbc,$nbbc)==0){ // match, go on to next pair
	    $i++; $ni++;
	  }elseif(isset($bc->action) && strcmp($bc->action,"log") == 0 &&
		  isset($nbc->action) && strcmp($nbc->action,"log") == 0 &&
		  // Add log message to all incorrect entries
		  // commit 4889f5fb386998, Dec 17 20:17:21 2011
		  // Compare syntax error logs
		  strpos($bbc,'EQUATION-SYNTAX-ERROR') !== false &&
		  strpos($nbbc,'EQUATION-SYNTAX-ERROR') !== false){
	    $i++; $ni++;
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
	  $diffStepTally++;
	  $row=array_shift($rows);
	  echo "  <tr class='$method'><td rowspan='$nrows'>$aaa</td><td rowspan='$nrows'>$aa</td>$row</tr>\n";
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
      }
    }
  }
 }

fwrite($handle,"\n]\n");
fclose($handle);

echo "</table>\n";
if($printDiffs){
  echo "<p>Analysed $stepTally steps with $diffStepTally discrepencies.\n";
 }
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

// List any sessions that had to be closed automatically.
if(count($closeSession)>0){
  echo "<p>Hanging Sessions:<br>\n";
  echo "<table border=1>\n";
  echo "<tr><th>Start Time</th><th>Session</th><th>Response</th></tr>\n";
  foreach($closeSession as $value){
    echo " <tr>$value</tr>\n";
  }
  echo "</table>\n";
 }

?>

</body>
</html>
