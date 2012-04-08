<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
  <LINK REL=StyleSheet HREF="log.css" TYPE="text/css" >
  <title>Review Sessions</title>
</head>
<body>
<?php

// These are all optional
$userName = '';  // regexp to match
	     // MIT_.*
             // asu experiment
	     // asu_3u16472755e704e5fasul1_.*
	     // asu_3u16472755e704e5fasul1_15865
	     // Help server dies on this section when running all osu:
	     // asu_3u16472755e704e5fasul1_15854
	     // ^uwplatt_
	     // ^uwplatt_(8p1304|90476) Scaife sections 
	     //       user names got mangled in these sections.
	     // ^uwplatt_(2Y130|514219|6l1305|3n130) Pawl sections
	     // 
$sectionName = '';  // regexp to match
$startDate = '2011-04-01'; 
$endDate = '';

  // File with user name and password.
  // chmod 600 db_onelog_password
$myFile = "db_onelog_password";
$fh = fopen($myFile, 'r');
$dbuser = chop(fgets($fh));
$dbpass = chop(fgets($fh));
$dbname = chop(fgets($fh));
if(strlen($dbname)==0){
  $dbname='andes3';
 }
fclose($fh);

$dbserver= "localhost";
if(strcmp($dbuser,'open')==0){
  $problem_attempt='OPEN_PROBLEM_ATTEMPT';
 } else {
  $problem_attempt='PROBLEM_ATTEMPT';
 } 

//Establish connection  
    
function_exists('mysql_connect') or die ("Missing mysql extension");
mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE"); 

$extra = 'Original'; // can be 'Reviewed', 'Original', or '' for both.

if($userName==''){
  $userNamec = "";
  $userNamee = "";
 } else {
  $userNamec = "P1.userName REGEXP '$userName' AND";
  $userNamee = " by $userName,";
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

function prob_link($turns){
  global $dbuser,$dbserver,$dbpass,$dbname;
  $turn=reset($turns);
  $clientID=$turn['clientID'];
  $tID=$turn['tID'];
  return "<a href=\"/log/OpenTrace.php?x=$dbuser&amp;sv=$dbserver&amp;pwd=$dbpass&amp;d=$dbname&amp;cid=$clientID&amp;t=$tID\">Session&nbsp;log</a>";
}

require("time.php");
require("blame.php");
require("binomials.php");
$initialTime = time();
$queryTime = 0.0;  // Time needed to query database.
$jsonTime1 = 0.0;
$jsonTime2 = 0.0;
// Newer versions of php have a json decoder built-in.  Should 
// eventually have test for php version and use built-in, when possible.
include 'JSON.php';
// However, this is really slow.  For now, just increase time limit:  
set_time_limit(300000);
ini_set("memory_limit","1024M");  // if I do everything, it gets big...

$json = new Services_JSON();

echo "Analyze problems $extrae,$userNamee$sectionNamee\n";

// Always do sessions in chronological order
$sql = "SELECT * FROM $problem_attempt AS P1 WHERE $userNamec $sectionNamec $extrac $startDatec $endDatec P1.clientID = P1.clientID ORDER BY startTime ASC";
$queryStart=microtime(true);   
$result = mysql_query($sql);
$queryTime += microtime(true)-$queryStart;
if ($myrow = mysql_fetch_array($result)) {
  $rowOutput=array();
  $tt=0;  // Total time for all sessions, with out-of-focus removed.
  $totalFloundering=0;  // Total time for all sessions for floundering
  $badTimes=array();
  do
    {
      $clientID=$myrow['clientID'];
      $thisName=$myrow['userName'];
      $thisSection=$myrow['userSection'];
      $thisProblem=$myrow['userProblem'];
      $tempSqlOld = "SELECT initiatingParty,command,tID FROM PROBLEM_ATTEMPT_TRANSACTION WHERE clientID = '$clientID'";
      $tempSql = "SELECT client,server,tID FROM STEP_TRANSACTION WHERE clientID = '$clientID'";
      $queryStart=microtime(true);   
      $tempResultOld = mysql_query($tempSqlOld);
      $tempResult = mysql_query($tempSql);
      $queryTime += microtime(true)-$queryStart;
      
      // loop through session

      // Minimum number of seconds between turns where 
      // we assume user is not "on task."
      $cutoff=600; 
      $sessionTime = new session_time();
      $sessionCorrects=array();  // Objects that have turned green. 
      $blame = new turn_blame();
      $sessionScore=0;
      // 
      // echo "session " . $myrow["startTime"] . "<br>\n";
      
      // get student input and server reply
      while (($myrow = mysql_fetch_array($tempResult)) ||
	     ($tempResultOld &&
	      ($myrow = mysql_fetch_array($tempResultOld)))) {
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
	// Ignore fade problems
	if(false && isset($a->method) && $a->method == 'open-problem' &&
	   $a->params->problem == "vec1ay"){
	  break;
	}
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
	  $thisTurn=false;
	  $thisObject=isset($a->params->id)?$a->params->id:NULL;
	  if(isset($b->result)){
	    foreach ($b->result as $row){
	      //print_r($row); echo "<br>\n";
	      if($row->action=="modify-object" && isset($row->mode)){
		$thisTurn=$row->mode;
		$thisObject=$row->id;
	      }
	    }
	  }
	  if($a->method == 'seek-help'){
	    $thisTurn="help";
	  }
	  $sessionTime->update_flounder($thisTurn,$ttID);
	  
	  // Collect Knowledge components
	  // $a->id or $b->id is number of turn.
	  // $thisObject is name of object on user interface.
	  
	  if($thisTurn && // 'correct' 'incorrect' or 'help'
	     // If turn doesn't have object, then need to record.
	     // WWH hints should log associated object, but they don't...
	     // In any case, NSH doesn't have an object.
	     !($thisObject &&
	       // ignore object once it turns green.
	       array_key_exists($thisObject,$sessionCorrects))){

	    // ignore object once it turns green.
	    if($thisTurn=='correct' && $thisObject){
	      $sessionCorrects[$thisObject]=1;
	    }
	    
	    $turnTable=array('grade' => $thisTurn, 
			     'timeStamp' => $sessionTime->timeStamp,
			     'id' => $a->id,
			     'clientID' => $clientID,
			     'tID' => $ttID,
			     'dt' => $sessionTime->dt(),
			     'random-help' => array());
	    // Determine if there is an associated error.
	    $car1 = '/^.([^ ]*).+$/'; $car2='$1';
	    $hints=false;
	    $reversibleOp=false; // Hint name matches $reversibleErrors
	    $reversibleHint=false; // Hint text matches $reversibleHints
	    $hasHintLog=false;  // Hint is explained in assoc field.
	    if(isset($b->result)){
	      foreach ($b->result as $row){
		if(isset($row->action) && $row->action == 'log' &&
		   $row->log == 'student' && isset($row->{'error-type'})){
		  // pull out first word in error.
		  $thisError=preg_replace($car1,$car2,$row->{'error-type'},1);
		  $turnTable['error']= $thisError;
		  $allErrors[$thisError]=1;
		  $hasHintLog=true;
		} elseif(isset($row->action) && $row->action == 'log' &&
			  $row->log == 'tutor' && isset($row->assoc)){
		  foreach($row->assoc as $var => $val){
		    if(strcasecmp($var,'random-help')==0){
		      $turnTable['random-help'][$val]=1;
		      // Any operator that is candidate for backwards hints
		    } elseif (isReversibleError($var)){
		      $reversibleOp=true;
		      $hasHintLog=true;
		    } elseif (strcasecmp($var,'NO-LABEL')==0){
		      // Andes gives spontaneous hint for no-label
		      // based on student model.  Thus, the help
		      // strategy changes with time.
		      $turnTable['random-help']['spontaneous-hint']=1;
		      $hasHintLog=true;
		    } else {
		      $hasHintLog=true;
		    }
		  }
		} elseif(isset($row->action) && $row->action == 'show-hint' &&
			 isClarificationHint($row->text)){
		  // clarification hints are ignored as feedback.
		} elseif(isset($row->action) && $row->action == 'show-hint' &&
			 isMetaHint($row->text)){
		  // meta-hints are ignored as feedback.
		  // If we had meta skill "asking for help when floundering"
		  // then we would do something with these.
		} elseif(isset($row->action) && $row->action == 'show-hint'){
		  $hints=true;
		  // This test is probably rather time-consuming.
		  // It doesn't really need to be run, except
		  // as a sanity test.
		  if(!$reversibleOp // if this is true, no testing done
		     && isReversibleHint($row->text)){
		    $reversibleHint=true;
		  }
		} elseif(isset($row->action) && $row->action == 'set-score'){
		  $sessionScore=$row->score;
		}
	      }
	    }
	    // Could have given a hint, but didn't
	    if($thisTurn=='incorrect' && !$hints){
	      $turnTable['random-help']['no-hints']=1;
	    }
	    // Detect cases where backwards hint could be given.
	    if($hints && ($thisTurn=='incorrect' || $thisTurn=='hint')){
	      $mark=!isset($turnTable['random-help']['GIVE-BACKWARDS-HINTS']);
	      if($reversibleOp){
		if($mark)
		  $turnTable['random-help']['forwards-hint']=1;
	      } else if($reversibleHint && !$hasHintLog){
		if($mark)
		  $turnTable['random-help']['forwards-hint']=1;
	      } else if(!$reversibleHint && $hasHintLog){
		// ok, hint not eligible for reverse
		// This should not ever be marked as backwards.
	      } else if($hasHintLog){
		// non error-hint matches list
		$turnTable['random-help']['zz-extra-hint']=1;
	      } else {
		// Hint without logging but not in error hint list:
		// Should check.
		$turnTable['random-help']['zz-unclassified-hint']=1;
	      }
	    }	

	    $blame->update($turnTable,$thisObject,$a,$b);

	    if(!isset($turnTable['error']) || 
	       !isset($simpleErrors[$turnTable['error']])){
	      // Note that time spend modifying green objects
	      // is counted towards work on the next step.
	      // Assume that no time is spent on slips.
	      $sessionTime->endNonSlip();
	    }
	  }	      
	} // loop through session rows    
      }   
      
      $sessionTime->endSession();
      
      // Now go back and do assignment of blame for entries
      // with an object, but no later interp has been found,
      // along with entries without object.
   
      // Finally, we need to sort the turns in this session,
      // consolidate help requests, and append to global lists
      $blame->resolve($thisSection,$thisName);

      // Look at intervention
      $fProbs=array('vec1ay','kt11ay','vec3dy');
      $fCut=4*3600;  // Cutoff, in seconds
      if(!isset($fade[$thisSection][$thisName])){
	$fade[$thisSection][$thisName] =
	  array('tTime0' => 0, 'fTime0' => 0, // before intervention
		'tTime1' => 0, 'fTime1' => 0, // during intervention
		'tTime2' => 0, 'fTime2' => 0, // after intervention
		'G' => 0, 'score' => 0, 'video' => 0);
      }
      $f=&$fade[$thisSection][$thisName];
      if($f['tTime0']+$f['tTime1']+$f['tTime2']<$fCut){
	if(in_array($thisProblem,$fProbs) || $sessionTime->videoTime>0){
	  // Is fade problem or watched video
	  if(in_array($thisProblem,$fProbs)){
	    $f['G']+=$sessionTime->sessionTime;
	    $f['score']=max($f['score'],$sessionScore);
	  }
	  $f['video']+=$sessionTime->videoTime;
	  $f['tTime1']+=$sessionTime->sessionTime;
	  $f['fTime1']+=$sessionTime->sessionFlounder;
	  // Any time previously marked as "after intervention"
	  // is now moved to "during intervention"
	  $f['tTime1']+=$f['tTime2'];
	  $f['fTime1']+=$f['fTime2'];
	  $f['tTime2']=0;
	  $f['fTime2']=0;
	} else if($f['G']==0){
	  // Has not done fade problem yet.
	  $f['tTime0']+=$sessionTime->sessionTime;
	  $f['fTime0']+=$sessionTime->sessionFlounder;
	} else {
	  // Not a fade problem, but fade problems have been done.
	  $f['tTime2']+=$sessionTime->sessionTime;
	  $f['fTime2']+=$sessionTime->sessionFlounder;
	}
      }
      
    } while ($myrow = mysql_fetch_array($result));

  krsort($rowOutput);
   } else {// if for any session existing
  echo "<p>No matching sessions found\n";
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



// For each student and KC, find the Maximun Likelihood solution for a model
// with three paramenters: P(G) P(S) step where skill was learned.
//
foreach($allKCStudent as $kc => $sec) {
  foreach($sec as $thisSection => $stu) {
    foreach($stu as $thisName => $opps) {
      $model[$kc][$thisSection][$thisName] =
	maximum_likelihood_models($opps);
    }
  }
}

// For each KC, find all opportunities to learn skill that
// did or did not result in learning.

function printv($arr){
  $s='('; $i=0;
  foreach($arr as $v => $ignore){
    $s=$s . ($i++==0?'':',') . $v;
  }
  return $s . ')';
}   
function print_turn($turn){
  return $turn['timeStamp'] . $turn['grade'] . printv($turn['random-help']);
}
$debugLearn=false;
if($debugLearn) echo "<ul>\n";
foreach($model as $kc => $sec){
  foreach($sec as $thisSection => $stu){
    foreach($stu as $thisName => $maxv){
      if($debugLearn) echo "  <li>$kc for $thisName $thisSection: \n" . 
	"  <ul>\n";
      if($maxv['valid']){
	$top=count($allKCStudent[$kc][$thisSection][$thisName]);
	for($opp=0; $opp<$top; $opp++){
	  $turns=$allKCStudent[$kc][$thisSection][$thisName][$opp];
	  $learn=isset($maxv['learnProb'][$opp])?
	    number_format($maxv['learnProb'][$opp],2):'???';
	  if($debugLearn) echo "    <li>learn $learn:";
	  foreach($turns as $turn){
	    if($debugLearn) echo ' ' . print_turn($turn);
	  }	  
	  if($debugLearn) echo ' ' . prob_link($turns) . "\n";	  
	}
      } else {
	// less than 50% correct is "never learn skill"
	$status=$maxv['ps']->val>0.5?'never':'already';
	// Case where the student did not know skill and never learned it.
	$opps=$allKCStudent[$kc][$thisSection][$thisName];
	// Ignore last opportinuty:  student could have learned skill 
	// at that time, but we'll never know.
	for($opp=0; $opp<count($opps); $opp++){
	  $turns=$opps[$opp];
	  if($debugLearn)
	    echo '    <li>' . 
	      (($opp==count($opps)-1)?'???':"$status learned:");
	  foreach($turns as $turn){
	    // This is a turn that did not result in learning.
	    if($debugLearn) echo ' ' .  print_turn($turn);
	  }
	  if($debugLearn) echo ' ' . prob_link($turns) . "\n";	  
	} 
      }
      if($debugLearn) echo "  </ul>\n";
    }
  }
}
if($debugLearn) echo "</ul>\n";


// Average model over students.
// Use http://arxiv.org/abs/physics/0401042
// for handling errors
//
// Since the model is not the same over students, there
// is both the error associated with each student as
// well as the actual variation across the population.
// In the following, we estimate the error due to the
// uncertainty of each student model.
foreach ($model as $kc => $ss){
  $allModel[$kc]=array('learn' => average_model($ss,'learn',true),
		       'pg' => average_model($ss,'pg',true),
		       'ps' => average_model($ss,'ps',true),
		       'psNot' => average_model($ss,'ps',false));
}


// Average kc's over students
foreach ($allKCStudent as $kc => $ss){
  $sums=array();
  $total=array();
  foreach($ss as $thisSection => $st){
    foreach($st as $thisName => $insts){
      foreach($insts as $i => $turns){
	if(!isset($sums[$i][$turns[0]['grade']])){
	  $sums[$i][$turns[0]['grade']]=0;
	}
	$sums[$i][$turns[0]['grade']]++;
	if(!isset($total[$i])){
	  $total[$i]=0;
	}
	$total[$i]++;
      }
    }
  }
  foreach($sums as $i => $sum){
    if(!isset($sum['correct']))
      $sum['correct']=0;
    $c = $sum['correct']; $w = $total[$i]-$c;
    $frac = $c/($c+$w);
    // Calculate lower and upper numerically.
    $y = new valErr($frac);
    binomial_errors($y,$c,$w);
    $avgCorrectKC[$kc][$i] = $y;
  }
}

// Print out model parameters for each kc, averaged over student.
if(false){
  foreach($allModel as $kc => $params){
    echo "$kc:  ";
    foreach($params as $param => $value){
      $countValid=$value['countValid']; $countAll=$value['countAll'];
      echo " $param=" . $value['val']->print0() . "[$countValid/$countAll],";
    }
    echo "\n";
  }
 }

// Print out fraction of time first step is correct for each
// opportunity and kc, averaged over students.
if(false){
  foreach($avgCorrectKC as $kc => $ii){
    echo "$kc:  ";
    foreach($ii as $i => $correct){
      echo $correct->print0();
    }
    echo "\n";
  }
 }

// For each kc and student, print out first step for each opportunity.
if(false){
  foreach ($allKCStudent as $kc => $ss){
    foreach($ss as $thisSection => $st){
      foreach($st as $thisName => $xx){
	echo "$kc $thisSection $thisName\n";
	foreach($xx as $yyy) {
	  $yy=reset($yyy); // return first element.
	  $g=$yy['grade']; $dt=$yy['dt'];
	  $id=$yy['id']; $tt=$yy['timeStamp']; $clientID=$yy['clientID'];
	  $err=isset($yy['error'])?$yy['error']:'';
	  echo "    ($g," . print2($dt) . ",$err)\n";
	}
	echo "\n";
      }
    }
  }
}

// dump fade data out to mathematica
if(true){
  $i=0;
  echo "fades={";
  foreach($fade as $thisSection => $ss){
    foreach($ss as $thisName => $st){
      if($i++>0)
	echo ",\n  ";
      $tTime0=$st['tTime0']; $fTime0=$st['fTime0'];
      $tTime1=$st['tTime1']; $fTime1=$st['fTime1'];
      $tTime2=$st['tTime2']; $fTime2=$st['fTime2'];
      $G=$st['G']; $score=$st['score']; $video=$st['video'];
      echo '{' . "\"$thisSection\",\"$thisName\",$G,$score,$video,$tTime0,$fTime0,$tTime1,$fTime1,$tTime2,$fTime2" . '}';
    }
  }
  echo "};\n";
 }

// Print out all KS's used.
echo "<p>";
ksort($allKCs);
foreach($allKCs as $kc => $dummy){
  echo "$kc ";
}
echo "<p>";

// Print out all errors.
ksort($allErrors);
foreach($allErrors as $err => $dummy){
  echo "$err ";
}
echo "\n\n";
	    
mysql_close();
?>
</body>
</html>
