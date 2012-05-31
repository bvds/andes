<?php

  // Find session for a given tID, for debugging:
  // select p.userSection,p.userName,p.userProblem,p.StartTime from PROBLEM_ATTEMPT as p, STEP_TRANSACTION as s WHERE p.clientID=s.clientID and s.tID='532823';

  //  Match funciton log0 in Help/model.cl
function log0($x){
  return $x>0?log($x):-1;
}

class student_state {

  private $sessionGrades;      // Talley grades
  public $studentState;  // State object for each ttID

  function begin_session(){
      // Talley grades
    $this->sessionGrades=array('correct' => array(), 
			       'incorrect'=>0, 'help'=>0);
  }  


  // Update student state.  Ideally, the state should be determined
  // after the student action is analyzed but before
  // any help is decided upon.  See Bug #1956.
  // In the current situation, however, there is no red/green 
  // information for the current transaction.
  
  function update($ttID,$thisTurn,$sessionTime,$sessionCorrects){
    $sC=count($sessionCorrects);
    $sI=(isset($this->sessionGrades['incorrect'])?
	 $this->sessionGrades['incorrect']:0);
    $sH=(isset($this->sessionGrades['help'])?
	 $this->sessionGrades['help']:0);
    $this->studentState[$ttID] =
      array('logSessionTime' => log0($sessionTime->sessionTime),
	    'fracSessionFlounderTime' => 
	    $sessionTime->sessionFlounder/$sessionTime->sessionTime,
	    'logNowRedTrans' => log0($sessionTime->nongreenSteps),
	    'logNowRedTime' => log0($sessionTime->nongreenTime),
	    'logNowIdleTime' => log0($sessionTime->dt()),
	    'sessionCorrect' => $sC,
	    'sessionIncorrect' => $sI,
	    'sessionHelp' => $sH,
	    'fracSessionCorrect' => ($sC+$sI+$sH>0?$sC/($sC+$sI+$sH):0)
	    );
    // Update after recording state.
    if($thisTurn){
      $this->sessionGrades[$thisTurn]++; 
    }
  }
  
// Dump student state in csv format.
  function csv(){
    echo "ttID";
    foreach(reset($this->studentState) as $key => $value){
      echo ",$key";
    }
    echo "\n";
    foreach($this->studentState as $ttID => $state){
      echo "$ttID";
      foreach($state as $key => $value){
	echo ",$value";
    }
      echo "\n";
    }
  }

}

?>
