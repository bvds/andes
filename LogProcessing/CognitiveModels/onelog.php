<?php 
$htmlHeaders=false;  // Add html header to beginning
if($htmlHeaders):
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
  <LINK REL=StyleSheet HREF="log.css" TYPE="text/css" >
  <title>Review Sessions</title>
</head>
<body>
<?php
   endif;

// These are all optional
 //     ^md5:b50efc   // Student used for tests of different models.
 //     ^md5:e2ed3385  // student in '^uwplatt_2Y130'
$userName = '^md5:b50efc';  // regexp to match
	     // MIT_.*
             // asu experiment
	     // asu_3u16472755e704e5fasul1_.*
	     // asu_3u16472755e704e5fasul1_15865
	     // Help server dies on this section when running all osu:
	     // asu_3u16472755e704e5fasul1_15854
	     // ^uwplatt_
	     // ^uwplatt_(8p1304|90476) Thomas Scaife sections 
	     //       user names got mangled in these sections.
	     // ^uwplatt_(2Y130|514219|6l1305|3n130) Andy Pawl sections
	     //  discrepencies
$sectionName = '';  // regexp to match
$startDate = '2011-03-25';
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
require("step-model.php");
require("training.php");
require("state.php");

$training = new training();
$state = new student_state();
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

if($htmlHeaders){
  echo "Analyze problems $extrae,$userNamee$sectionNamee\n";
 }

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

      // Minimum number of seconds between transactions where 
      // we assume user is not "on task."
      $cutoff=180; 
      $sessionTime = new session_time();
      $sessionCorrects=array();  // Objects that have turned green. 
      $blame = new turn_blame();
      $sessionScore=0;
      $state->begin_session();
      // 
      // echo "session " . $myrow["startTime"] . "<br>\n";
      
      // get student input and server reply
      while (($myrow = mysql_fetch_array($tempResult)) ||
	     ($tempResultOld &&
	      ($myrow = mysql_fetch_array($tempResultOld)))) {
	if(isset($myrow['command'])){
	  $myrow2 = mysql_fetch_array($tempResultOld);
	  if($myrow['initiatingParty']=='client'){
	    $action=$myrow['command'];
	    $ttID=$myrow['tID'];
	    $response=$myrow2['command'];
	  } else {
	    $action=$myrow2['command'];
	    $ttID=$myrow2['tID'];
	    $response=$myrow['command'];
	  }
	} else {
	  $action=$myrow['client'];
	  $ttID=$myrow['tID'];
	  $response=$myrow['server'];
	}

	// decode json and count number of transactions (and time)
	// between incorrect transactions and next correct transaction.
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
	   strpos($response,'Your session is no longer active.')!==false){
	  break;
	}
	// Drop transactions where timestamp is corrupted
	if(isset($a->params->time) && 
	   $a->params->time<$sessionTime->timeStamp){
	  $badTimes[$a->params->time]=$action;
	  continue;
	} elseif(isset($a->params) && isset($a->params->time)){
	  $sessionTime->update_timeStamp($a->params->time);
	  $timeStampAction=$action;
	} else {
	  // drop transactions without timestamps;
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
	      if($row->action=='modify-object' && isset($row->mode)){
		$thisTurn=$row->mode;
		$thisObject=$row->id;
	      }
	    }
	  }
	  if($a->method == 'seek-help'){
	    $thisTurn='help';
	  }


	  // Record state before floundering for transaction 
	  // is calculated.  See Bug #1956.
	  //
	  // Could slim down the number of states that get
	  // recorded since only some are needed.
	  $state->update($ttID,$thisTurn,$sessionTime,$sessionCorrects);

	  $sessionTime->update_flounder($thisTurn,$ttID);
	  
	  // Collect Knowledge components
	  // $a->id or $b->id is number of turn.
	  // $thisObject is name of object on user interface.
	  
	  if($thisTurn && // 'correct' 'incorrect' or 'help'
	     // If transaction doesn't have object, then need to record.
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
	    if($hints && ($thisTurn=='incorrect' || $thisTurn=='help')){
	      $mark=!isset($turnTable['random-help']['GIVE-BACKWARDS-HINTS']);
	      if($reversibleOp){
		if($mark)
		  $turnTable['random-help']['forwards-hint']=1;
	      } else if($reversibleHint && !$hasHintLog){
		if($mark)
		  $turnTable['random-help']['forwards-hint']=1;
	      } else if(!$reversibleHint && $hasHintLog){
		// ok, hint not eligible for reverse
		// This should never be marked as backwards.
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
   
      // Finally, we need to sort the transactions in this session,
      // consolidate help requests, and append to global lists
      $blame->resolve($thisSection,$thisName);

      $training->update($thisSection,$thisName,$thisProblem,
			$sessionScore,$sessionTime);
      
    } while ($myrow = mysql_fetch_array($result));

  krsort($rowOutput);
   } else {// if for any session existing
  echo "<p>No matching sessions found\n";
 }
if($htmlHeaders){
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
 }


// For each student and KC, find the Maximun Likelihood solution for a model
// with three paramenters: P(G) P(S) and L=step where skill was learned.
// Also, find a set of models for each L, and the relative probability
// of each sub-model.
//
$debugML=false;
foreach($allKCStudent as $kc => $sec) {
  foreach($sec as $thisSection => $stu) {
    foreach($stu as $thisName => $opps) {
      if($debugML){
	echo "$kc, $thisSection, $thisName \n";
      }
      $model[$kc][$thisSection][$thisName] =
	maximum_likelihood_models($opps,$debugML);
    }
  }
}

// For each KC, find all steps (opportunities to learn skill) that
// did or did not result in learning.

function printv($arr){
  $s='('; $i=0;
  foreach($arr as $v => $ignore){
    $s=$s . ($i++==0?'':',') . $v;
  }
  return $s . ')';
}   
function print_turn($turn){
  return $turn['timeStamp'] . ':' . $turn['id'] . $turn['grade'] . 
    printv($turn['random-help']);
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
	  $learn=isset($maxv['learnHereProb'][$opp+1])?
	    number_format($maxv['learnHereProb'][$opp+1],2):'???';
	  if($debugLearn) echo "    <li>learn $learn:";
	  foreach($turns as $turn){
	    if($debugLearn) echo ' ' . print_turn($turn);
	  }	  
	  if($debugLearn) echo ' ' . prob_link($turns) . "\n";	  
	}
      } else {
	// less than 50% correct is "never learn skill"
	$status=$maxv['ps']>0.5?'never':'already';
	// Case where the student did not know skill and never learned it.
	$opps=$allKCStudent[$kc][$thisSection][$thisName];
	// Ignore last step:  student could have learned skill 
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
    $avgCorrectKC[$kc][$i] = $c/($c+$w);
  }
}

mysql_close();
if($htmlHeaders){
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
  echo "</body>";
  echo "</html>";
 }

// Non-html format output


// Print out fraction of time first step is correct for each
// step and kc, averaged over students.
if(false){
  foreach($avgCorrectKC as $kc => $ii){
    echo "$kc:  ";
    foreach($ii as $i => $correct){
      echo $correct->print0();
    }
    echo "\n";
  }
 }


// For each student, print out first transaction for each step, 
// with kc and grade in csv format (for debugging).
if(false){
  foreach ($allStudentKC as $thisSection => $st){
    foreach($st as $thisName => $opps){
      echo "$thisSection $thisName\n";
      foreach($opps as $yy) {
	$kc=$yy['kc']; $g=$yy['grade'];
	$id=$yy['id']; $tt=$yy['timeStamp']; $clientID=$yy['clientID'];
	echo "    $clientID, $id, $tt, $kc, $g\n";
      }
      echo "\n";
    }
  }
 }

// For each kc and student, list correctness of first transaction
// in each opportunity, csv format.  
// Suitable as input for models of learning.
if(true){
  ksort($allKCStudent);
  foreach ($allKCStudent as $kc => $ss){
    // 'none' are transactions where assignment of blame
    // has failed.
    if($kc=='none'){continue;} 
    foreach($ss as $thisSection => $st){
      foreach($st as $thisName => $opps){
	echo "\"$kc\",\"$thisSection\",\"$thisName\"";
	foreach($opps as $opp) {
	  $yy=reset($opp); // return first element.
	  echo "," . ($yy['grade']=='correct'?'1':'0');
	}
	echo "\n";
      }
    }
  }
 }

// For each kc and student, print out first transaction for each step.
if(false){
  ksort($allKCStudent);
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

function skipKC($kc){
    // none:  entries where assigment of blame failed
    // select-mc-answer  answer multiple-choice questions
  return $kc=='none' || $kc=='select-mc-answer';
}

// For each student and KC, Print out the model parameters.
if(false){
  echo "\"KC\",\"Section\",\"Name\",\"AIC\",\"gainProb\"\n";
  ksort($allKCStudent);
  foreach($allKCStudent as $kc => $sec) {
    // none:  entries where assigment of blame failed
    // select-mc-answer  answer multiple-choice questions
    if(!skipKC($kc)){
      foreach($sec as $thisSection => $stu) {
	foreach($stu as $thisName => $opps) {
	  $maxv=$model[$kc][$thisSection][$thisName];
	  $aic=$maxv['valid']?2*3-2*$maxv['logLike']:0;
	  $gainProb=$maxv['gainProb'];
	  echo "\"$kc\",\"$thisSection\",\"$thisName\",$aic,$gainProb\n";
	}
      }
    }
  }
 }


// For each kc and step, print model parameters, step id (ttID),
// and policies used, in csv format.
if(false){
  $randomHelpCategories=array();
  foreach ($allKCStudent as $ss){
    foreach($ss as $st){
      foreach($st as $opp){
	foreach($opp as $turns){
	  foreach($turns as $turn){
	    foreach($turn['random-help'] as $var => $val){
	      if(!isset($randomHelpCategories[$var])){
		// echo "adding $var\n";
		$randomHelpCategories[$var]=1;
	      }
	    }
	  }
	}
      }
    }
  }

  // Header
  echo "\"KC\",\"clientID\",\"ttID\",\"stepTrans\",\"learning\",\"noSlip\"";
  foreach($randomHelpCategories as $var => $val){
    echo ",\"$var\"";
  }
  echo "\n";
  
  // Discount factor.
  // According to min's thesis, they use a discount factor
  // for number of kc-relevant transactions (page 36 of thesis) 
  // before the reward with a value of 0.9.
  // We will do this per incorrect step, since any  model-scaffold-fade 
  // strategy would be implemented in that manner. 
  $gamma=0.5; 
  foreach ($allKCStudent as $kc => $ss){
    // none:  entries where assigment of blame failed
    // select-mc-answer  answer multiple-choice questions
    if(skipKC($kc))continue; 
    foreach($ss as $thisSection => $st){
      foreach($st as $thisName => $opps){
	$maxv=$model[$kc][$thisSection][$thisName];
        $i=0;
	foreach($opps as $turns){
	  // There are things other than the hints which may
	  // cause learning.  Thus, count all transactions for determining
	  // weighting.  We don't know which of the transactions in a given
	  // step actually contributed to learning, so we
	  // weight them all equally. 
	  $oppTurns=count($turns);
	  $learn=0;
	  $gammaFactor=1;
	  for($k=$i+1; $k<count($opps); $k++){
	    $learn+=$maxv['learnHereProb'][$k]*$maxv['learnGain'][$k]*
	      $gammaFactor;
	    // grade undefined.
	    if($opps[$k][0]['grade'] != 'correct'){
	      $gammaFactor *= $gamma;
	    }
	  }
	  // In cases where there is no learning, student
	  // may have already learned skill.  In any case, we
	  // want to minimize slips after learning so that we can
	  // reward effective after-learning strategies.
	  //
	  // For the present policy choices, the policies
	  // can only be applied when a step is not 'correct'
	  // Need to distribute reward over only those steps.
	  // Otherwise we get a situation where cases with 
	  // lots of slips get a higher total reward.
	  //
	  $noSlip=0;
	  for($k=0; $k<=$i; $k++){
	    $noSlip+=($maxv['slip'][$k]>0?
		      (1-$maxv['slip'][$k])*$maxv['learnHereProb'][$k]/
		      ((count($opps)-$k)*$maxv['slip'][$k]):0);
	  }
	  
	  foreach($turns as $turn){
	    // Only print out instances where policy 
	    // change may apply.
	    if(count($turn['random-help'])>0){
	      $ttID=$turn['tID'];
	      $clientID=$turn['clientID'];
	      echo "\"$kc\",\"$clientID\",$ttID,$oppTurns,$learn,$noSlip";
	      foreach($randomHelpCategories as $var => $val){
		if(isset($turn['random-help'][$var])){
		  echo ",1";
		} else {
		  echo ",0";
		}
	      }
	      echo "\n";
	    }
	  }
	  $i++;
	}
      }
    }
  }
 }

// dump fade data out to mathematica
if(false){
  $training->mma_dump();
 }

// Dump student state in csv format.
if(false){
  $state->csv();
 }      

?>
