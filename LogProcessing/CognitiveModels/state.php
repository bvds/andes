<?php

function log0($x){
  return $x>0?log($x):-1;
}

class student_state {

  private $sessionGrades;      // Talley grades
  public $studentState;  // State object for each ttID

  function begin_session(){
      // Talley grades
      $this->sessionGrades=array('correct' => 0, 'incorrect'=>0, 'help'=>0);
  }  

  function update($ttID,$thisTurn,$sessionTime){
    if($thisTurn)
      $this->sessionGrades[$thisTurn]++; 
    $this->studentState[$ttID] =
      array('logSessionTime' => log0($sessionTime->sessionTime),
	    'fracSessionFlounderTime' => 
	    $sessionTime->sessionFlounder/$sessionTime->sessionTime,
	    'logNowFlounderSteps' => log0($sessionTime->now_flounder_steps()),
	    'logNowFlounderTime' => log0($sessionTime->now_flounder_time()),
	    'logNowStepTime' => log0($sessionTime->dt()),
	    'sessionCorrect' => $this->sessionGrades['correct'],
	    'sessionIncorrect' => $this->sessionGrades['incorrect'],
	    'sessionHelp' => $this->sessionGrades['help'],
	    'fracSessionCorrect' => 
	    $this->sessionGrades['correct']/($this->sessionGrades['correct']+
					     $this->sessionGrades['incorrect']+
					     $this->sessionGrades['help'])
	    );
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
