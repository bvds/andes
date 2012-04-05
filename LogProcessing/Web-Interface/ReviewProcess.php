<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
  <LINK REL=StyleSheet HREF="log.css" TYPE="text/css" >
  <title>Review Sessions</title>
  <script type="text/javascript" src='xml-scripts.js'></script>
  <script type="text/javascript">
    function openTrace(url){  
        window.open(url);
   }
  </script>
</head>
<body>
<?php
$dbname= $_POST['dbname'];
$dbuser= $_POST['dbuser'];
$dbserver= "localhost";
$dbpass= $_POST['passwd'];
if(strcmp($dbuser,'open')==0){
  $problem_attempt='OPEN_PROBLEM_ATTEMPT';
 } else {
  $problem_attempt='PROBLEM_ATTEMPT';
 } 

//CONNECTION STRING  
    
function_exists('mysql_connect') or die ("Missing mysql extension");
mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE"); 

$adminName = $_POST['adminName'];
$sectionName = $_POST['sectionName'];
$orderBy = $_POST['item'];
$order = $_POST['order'];
$extra = $_POST['extra'];
$slice = $_POST['slice'];
$startDate = $_POST['startDate'];
$endDate = $_POST['endDate'];
$methods = implode(",",$_POST['methods']);

if($order=='Descending')
  $order = "DESC";
 else
   $order = "";
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
if($extra == 'Reviewed'){
  // Changed extra from number to string or null
  // commit a0719d09f0017cb, Nov 9, 2011
  $extrac = "(P1.extra IS NOT NULL OR P1.extra !=0) AND";
  $extrae = "reviewed";
 }else if($extra == 'Original'){
  $extrac = "(P1.extra IS NULL or P1.extra = 0) AND";
  $extrae = "solved";
 }else{
  $extrac = "";
  $extrae = "solved or reviewed";
 }
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

if($slice == 'comments'){
  
  echo "<h2>Comments in problems $extrae,$adminNamee$sectionNamee sorted in $order order of $orderBy</h2>\n";
  
  $sqlOld = "SELECT * FROM $problem_attempt AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 WHERE $adminNamec $sectionNamec $extrac $startDatec $endDatec P1.clientID = P2.clientID AND P2.initiatingParty = 'client' AND P2.command LIKE '%\"action\":\"get-help\",\"text\":%' ORDER BY $orderBy $order";
  $sql = "SELECT * FROM $problem_attempt AS P1,STEP_TRANSACTION AS P2 WHERE $adminNamec $sectionNamec $extrac $startDatec $endDatec P1.clientID = P2.clientID AND P2.client LIKE '%\"action\":\"get-help\",\"text\":%' ORDER BY $orderBy $order";
  
  $resultOld = mysql_query($sqlOld);
  $result = mysql_query($sql);
  if (($myrow = mysql_fetch_array($result)) ||
      ($myrow = mysql_fetch_array($resultOld))) {
    echo "<table border=1>";
    echo "<tr><th>User Name</th><th>Problem</th><th>Section</th><th>Starting Time</th><th>Comment</th><th>Additional</th></tr>\n";
    do
      {
	$tID=$myrow["tID"];
	// Choose new style with STEP_TRANSACTION or old style 
	// with PROBLEM_ATTEMPT_TRANSACTION
	if(isset($myrow["client"])){
	  // Include cases where reply is null.
	  $tempSql = "SELECT * FROM STEP_TRANSACTION WHERE tID = $tID and (server LIKE '%\"HANDLE-TEXT\":\"COMMENT\"%' or server like '%\"HANDLE-TEXT\":\"POSSIBLE-QUESTION\"%' or server not like '%\"result\":%')";
	} else {
	  // Include cases where reply is null.
	  // It is not quite Kosher to assume the next tID 
	  $tempSql = "SELECT * FROM PROBLEM_ATTEMPT_TRANSACTION WHERE tID = ($tID+1) and (command LIKE '%\"HANDLE-TEXT\":\"COMMENT\"%' or command like '%\"HANDLE-TEXT\":\"POSSIBLE-QUESTION\"%' or command not like '%\"result\":%')";
	}
	$tempResult = mysql_query($tempSql);
	if(mysql_fetch_array($tempResult))
	  {
	    $tempResult = NULL;
	    $tempSql = NULL;
	    $userName=$myrow["userName"];
	    $userProblem=$myrow["userProblem"];
	    $userSection=$myrow["userSection"];
            $clientID=$myrow["clientID"];
            $tID=$myrow["tID"];
	    $startTime=$myrow["startTime"];
	    // new style with STEP_TRANSACTION or old style with 
	    // PROBLEM_ATTEMPT_TRANSACTION
	    $tempCommand1=isset($myrow["client"])?$myrow["client"]:$myrow["command"];
	    $tempCommand2 =explode("get-help\",\"text\":\"",$tempCommand1);
	    $command=explode("\"}",$tempCommand2[1]);
	    
	    echo "<tr><td>$userName</td><td>$userProblem</td><td>$userSection</td><td>$startTime</td><td>$command[0]</td><td><a href=\"OpenTrace.php?x=$dbuser&amp;sv=$dbserver&amp;pwd=$dbpass&amp;d=$dbname&amp;cid=$clientID&amp;u=$userName&amp;p=$userProblem&amp;s=$userSection&amp;t=$tID&amp;m=$methods\">Session&nbsp;log</a></td></tr>\n";
	  }
      }
    while ($myrow = mysql_fetch_array($result));
    echo "</table>";
  }
  
} elseif($slice == 'sessions') {
  
  echo "<h2>Problems $extrae,$adminNamee$sectionNamee sorted in $order order of $orderBy</h2>\n";
  
  $sql = "SELECT * FROM $problem_attempt AS P1 WHERE $adminNamec $sectionNamec $extrac  $startDatec $endDatec P1.clientID = P1.clientID ORDER BY $orderBy $order";
  
  // echo "mysql query \"$sql\"\n";
  
  $result = mysql_query($sql);
  $count = 0;
  if ($myrow = mysql_fetch_array($result)) {
    echo "<table border=1>\n";
    echo "<tr><th>User Name</th><th>Problem</th><th>Section</th><th>Starting Time</th><th>Log</th></tr>\n";
    do
      {
	$count++;
	$clientID=$myrow["clientID"];
	$userName=$myrow["userName"];
	$userProblem=$myrow["userProblem"];
	$userSection=$myrow["userSection"];
	$startTime=$myrow["startTime"];
	
	echo "<tr><td>$userName</td><td>$userProblem</td><td>$userSection</td><td>$startTime</td><td><a href=\"OpenTrace.php?x=$dbuser&amp;sv=$dbserver&amp;pwd=$dbpass&amp;d=$dbname&amp;cid=$clientID&amp;u=$userName&amp;p=$userProblem&amp;s=$userSection&amp;m=$methods\">Session log</a></td></tr>\n";
	
      }
    while ($myrow = mysql_fetch_array($result));
    echo "</table>\n";
  }
  echo "<p>Found $count sessions.\n";
  
} elseif($slice == 'errors') {
  // doesn't use $order or $orderBy

  require("time.php");
  $badTimes=array();
    
  $initialTime = time();
  $queryTime = 0.0;  // Time needed to query database.
  $jsonTime1 = 0.0;
  $jsonTime2 = 0.0;
  // Newer versions of php have a json decoder built-in.  Should 
  // eventually have test for php version and use built-in, when possible.
  include 'JSON.php';
  // However, this is really slow.  For now, just increase time limit:  
  set_time_limit(3000);
  
  $json = new Services_JSON();
  
  echo "<h2>Student errors in problems $extrae,$adminNamee$sectionNamee sorted by time of confusion</h2>\n";
  
  $sql = "SELECT * FROM $problem_attempt AS P1 WHERE $adminNamec $sectionNamec $extrac  $startDatec $endDatec P1.clientID = P1.clientID";
  $queryStart=microtime(true);   
  $result = mysql_query($sql);
  $queryTime += microtime(true)-$queryStart;
  if ($myrow = mysql_fetch_array($result)) {
    $rowOutput=array();
    $tt=0;  // Total time for all sessions, with out-of-focus removed.
    $totalFloundering=0;  // Total time for all sessions for floundering
    do
      {
	$clientID=$myrow["clientID"];
	$sessionColumns = "<td>" . $myrow["userName"] . "</td><td>" . 
	  $myrow["userProblem"] . "</td><td>" . $myrow["userSection"] . 
	  "</td><td>" . $myrow["startTime"] . "</td>";
	$sessionLink1 = "<td><a href=\"OpenTrace.php?x=" . $dbuser .
	  "&amp;sv=" . $dbserver . "&amp;pwd=" . $dbpass . "&amp;d=" . $dbname .
	  "&amp;cid=" . $clientID . "&amp;u=" . $myrow["userName"] . "&amp;p=" . 
	  $myrow["userProblem"] . "&amp;s=" . $myrow["userSection"] .  
	  "&amp;m=" . $methods;
	$sessionLink2 ="\">Session&nbsp;log</a></td>";
	$tempSqlOld = "SELECT initiatingParty,command,tID FROM PROBLEM_ATTEMPT_TRANSACTION WHERE clientID = '$clientID'";
	$tempSql = "SELECT client,server,tID FROM STEP_TRANSACTION WHERE clientID = '$clientID'";
	$queryStart=microtime(true);   
	$tempResultOld = mysql_query($tempSqlOld);
	$tempResult = mysql_query($tempSql);
	$queryTime += microtime(true)-$queryStart;
	
	// loop through session
	$cutoff=600; // Minimum number of seconds between turns where 
	             // we assume user is not "on task."
	$sessionTime = new session_time();
	
	// get student input and server reply
	while (($myrow = mysql_fetch_array($tempResult)) ||
	       ($myrow = mysql_fetch_array($tempResultOld))) {
	  if(isset($myrow["command"])){
	    $myrow2 = mysql_fetch_array($tempResultOld);
	    if($myrow["initiatingParty"]=='client'){
	      $action=$myrow["command"];
	      $ttID=$myrow["tID"];
	      $response=$myrow2["command"];
	    } else {
	      $action=$myrow2["command"];
	      $ttID=$myrow2["tID"];
	      $response=$myrow["command"];
	    }
	  } else {
	    $action=$myrow["client"];
	    $ttID=$myrow["tID"];
	    $response=$myrow["server"];
	  }
	  
	  // decode json and count number of steps (and time)
	  // between incorrect turns and next correct turn.
	  $jsonStart=microtime(true);   
	  $a=$json->decode($action);
	  $jsonTime1 += microtime(true)-$jsonStart;
	  // If session is closed, don't continue
	  if((isset($a->method) && $a->method == 'close-problem') ||
	     // Help server has closed an idle session.
	     strpos($response,"Your session is no longer active.")!==false){
	    break;
	  }
	  // Drop turns where timestamp is corrupted
	  if(isset($a->params->time) && 
	   $a->params->time<$sessionTime->timeStamp){
	    $badTimes[$a->params->time]=$action;
	    continue;
	  } elseif(isset($a->params) && isset($a->params->time)){
	    $sessionTime->update_timeStamp($a->params->time);
	    $timeStampAction=$action;
	  } else {
	    // drop turns without timestamps;
	    // this is generally from the server dropping idle sessions
	    continue;  
	  }

	  $sessionTime->update_focus($cutoff,$a);

	  // echo "  step $timeStamp  $sessionTime $ttID<br>\n";

	  if(isset($a->method) && ($a->method == 'solution-step' || 
				   $a->method == 'seek-help')){
	    $jsonStart=microtime(true);   
	    $b=$json->decode($response);
	    $jsonTime2 += microtime(true)-$jsonStart;
	    $thisTurn=NULL;
	    if(isset($b->result)){
	      foreach ($b->result as $row){
		//print_r($row); echo "<br>\n";
		if($row->action=="modify-object" && isset($row->mode)){
		  $thisTurn=$row->mode;
		}
	      }
	    }

	    // true if at end of floundering episode
	    if($sessionTime->update_flounder($thisTurn,$ttID)){
	      $rowOutput[$sessionTime->fTime]= "<tr>" . $sessionColumns . 
		'<td>' . $sessionTime->fSteps . '</td><td>' . 
		round($sessionTime->fTime) . "</td>" . $sessionLink1 . "&amp;t=" . 
		$sessionTime->confusedtID . $sessionLink2 . "</tr>";
	    }
	  }
	} // loop through session rows       
	
	// true if at end of floundering episode
	if($sessionTime->endSession()){
	  $rowOutput[$sessionTime->fTime]= "<tr class=\"quit\">" . 
	    $sessionColumns . '<td>' . $sessionTime->fSteps . '</td><td>' . 
	    round($sessionTime->fTime) . "</td>" . $sessionLink1 . "&amp;t=" . 
	    $sessionTime->confusedtID . $sessionLink2 . "</tr>";
	}

      }
    while ($myrow = mysql_fetch_array($result));
    krsort($rowOutput);

    echo "<table border=1>\n";
    echo "<colgroup><col span=\"4\"><col align=\"right\"><col align=\"char\"></colgroup>\n";
    echo "<thead>\n";
    echo "<tr><th>User Name</th><th>Problem</th><th>Section</th><th>Starting Time</th><th>Count</th><th>time</th><th>Additional</th></tr>\n";
    echo "</thead>\n";
    echo "<tbody>\n";    
    foreach($rowOutput as $row){
      echo "$row\n";
    }
    echo "</tbody>\n";
    echo "</table>\n";
  } else {// if for any session existing
    echo "No matching sessions found\n";
  }
  echo "<p>Total student time:&nbsp; " . number_format($tt,0) . 
    " seconds; total time <em>floundering</em>:&nbsp; " . 
    number_format($totalFloundering,0) . " seconds.\n";

  $elapsedTime = time() - $initialTime;
  echo "<p>Time to process:&nbsp; $elapsedTime s with " 
  . number_format($queryTime,2) . " s for mysql.&nbsp;\n";
  echo "Json decoding times:&nbsp; " . number_format($jsonTime1,2) .
  " s, " . number_format($jsonTime2,2) . " s.\n";

  if(count($badTimes)>0){
    echo "<p>Bad Timestamps:</p>\n";
    echo "<table border=1>\n";
    echo "<thead>\n";
    echo "<tr><th>Time</th><th>Action</th></tr>\n";
    echo "</thead>\n";
    echo "<tbody>\n";
    foreach ($badTimes as $time => $action){  
      echo "<tr><td>$time</td><td>$action</td></tr>\n";
    }
    echo "</tbody>\n";
    echo "</table>\n";
  }

} elseif($slice == 'user-agent') {
  // doesn't use $order or $orderBy
  $initialTime = time();
  $queryTime = 0.0;
  $jsonTime1 = 0.0;
  $jsonTime2 = 0.0;
  // Newer versions of php have a json decoder built-in.  Should 
  // eventually have test for php version and use built-in, when possible.
  include 'JSON.php';
  // However, this is really slow.  For now, just increase time limit:  
  set_time_limit(300);
  
  $json = new Services_JSON();
  
  echo "<h2>User Agent Strings</h2>\n";
  echo "<p>Record only the initial user agent string for each user since\n";
  echo "students may change browser based on an initial bad experience.\n";
  
  $sql = "SELECT * FROM $problem_attempt AS P1 WHERE $adminNamec $sectionNamec $extrac  $startDatec $endDatec P1.clientID = P1.clientID ORDER BY startTime";
  $queryStart=microtime(true);   
  $result = mysql_query($sql);
  $queryTime += microtime(true)-$queryStart;
  if ($myrow = mysql_fetch_array($result)) {
    $agentString=array();
    do
      {
	$clientID=$myrow["clientID"];
	$userSection =  $myrow["userName"] . "_" .  $myrow["userSection"];
	if(isset($agentString[$userSection])) {
	  continue;
	}

	// Only get initial transaction.
	$tempSql = "SELECT client,server,tID FROM STEP_TRANSACTION WHERE clientID = '$clientID' ORDER BY tID LIMIT 1";
	$queryStart=microtime(true);   
	$tempResult = mysql_query($tempSql);
	$queryTime += microtime(true)-$queryStart;
		
	// get student input and server reply
	while ($myrow = mysql_fetch_array($tempResult)) {
	  $action=$myrow["client"];
	  $ttID=$myrow["tID"];
	  $response=$myrow["server"];
	  
	  // decode json
	  $jsonStart=microtime(true);   
	  $a=$json->decode($action);
	  $jsonTime1 += microtime(true)-$jsonStart;

	  if (isset($a->method) && $a->method == 'open-problem'){
            // Go through open-problem and pick out user agent string.
	    $jsonStart=microtime(true);   
	    $b=$json->decode($response);
	    $jsonTime2 += microtime(true)-$jsonStart;
	    if(isset($b->result)){
	      foreach ($b->result as $row){
		if($row->action=='log' && $row->log=='user-agent' &&
		   !isset($agentString[$userSection])){
		  $agentString[$userSection]=$row->text;
		}
	      }
	    }
	  }
	} // loop through session rows       
      }
    while ($myrow = mysql_fetch_array($result));
  } else {// if for any session existing
    echo "No matching sessions found\n";
  }

  // Accumulate histogram and sort by popularity
  $agentHistogram = array();
  foreach ($agentString as $us => $agent){  
    if(isset($agentHistogram[$agent])){
      $agentHistogram[$agent]++;
    } else {
      $agentHistogram[$agent]=1;
    }
  }
  arsort($agentHistogram);
  
  // Divide into major browser types
  $x=array('Chrome' => 0, 'Safari' => 0, 'Firefox' => 0, 
	   'IE 7' => 0, 'IE 8' => 0, 'IE 9' => 0, 'other' => 0); 
  foreach ($agentHistogram as $agent => $count){  
    if(preg_match('/ MSIE 7.0;/',$agent)){
      $x['IE 7']+=$count;
    } elseif(preg_match('/ MSIE 8.0;/',$agent)){
      $x['IE 8']+=$count;
    } elseif(preg_match('/ MSIE 9.0;/',$agent)){
      $x['IE 9']+=$count;
      // Chrome must come before Safari
    } elseif(preg_match('/ Chrome/',$agent)){
      $x['Chrome']+=$count;
    } elseif(preg_match('/ Safari/',$agent)){
      $x['Safari']+=$count;
    } elseif(preg_match('/ Firefox/',$agent)){
      $x['Firefox']+=$count;
    } else {
      $x['other']+=$count;
    }
  }

  $nn=count($agentString);
  echo "<p>Total of $nn users.\n";

  echo "<table border=1>\n";
  echo "<thead>\n";
  echo "<tr><th>Percentage</th><th>Browser</th></tr>\n";
  echo "</thead>\n";
  echo "<tbody>\n";
  foreach ($x as $agent => $count){  
    echo "<tr><td>" . number_format($count/$nn*100,1) . 
      "</td><td>$agent</td></tr>\n";
  }
  echo "</tbody>\n";
  echo "</table>\n";

  echo "<p>Full list of user agents:\n";
  echo "<table border=1>\n";
  echo "<thead>\n";
  echo "<tr><th>Number</th><th>User Agent String</th></tr>\n";
  echo "</thead>\n";
  echo "<tbody>\n";
  foreach ($agentHistogram as $agent => $count){  
    echo "<tr><td>$count</td><td>$agent</td></tr>\n";
  }
  echo "</tbody>\n";
  echo "</table>\n";

  $elapsedTime = time() - $initialTime;
  echo "<p>Time to process:&nbsp; $elapsedTime s with " 
  . number_format($queryTime,2) . " s for mysql.&nbsp;\n";
  echo "Json decoding times:&nbsp; " . number_format($jsonTime1,2) .
  " s, " . number_format($jsonTime2,2) . " s.\n";
} else {
  echo "Unknown choice for slice";
}
	    
mysql_close();
?>
</body>
</html>
