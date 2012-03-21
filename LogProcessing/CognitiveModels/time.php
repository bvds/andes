<?php

  // Session timing information.

class session_time {

  private $lastTimeStamp=-1; // time stamp for this transaction.
  public $timeStamp=-1; // time stamp for this transaction.
  private $lastState='none'; // can be 'blur' 'focus' or 'something'
  private $sessionTime=0;  // total active time for session.
  private $lastStepTime=0;         // $sessionTime for last step.
  private $confused=false;
  public $counter=-1;  // number of steps while in confused state.
  private $lastCorrectSessionTime=0;  // $sessionTime for last green entry
  private $lastInorrectSessionTime=0;  // $sessionTime for last red entry
  public $confusedtID;

  function update_timeStamp($t){
    $this->lastTimeStamp=$this->timeStamp;
    $this->timeStamp=$t;
  }
  
  function update_focus($cutoff,$a){
    // Add up times that canvas is out of focus.
    // Sometimes blur or focus events get dropped.
    // Count consecutive blur-blur, blur-focus, focus-focus,
    //                   something-focus, blur-something  
    // as time out of focus.  That is, any time a blur or
    // focus event is missing, we assume maximum idle time.
    if(isset($a->method) && $a->method == 'record-action' &&
       $a->params->type == 'window' && $a->params->name == 'canvas'){
      if($a->params->value == 'focus'){
	// cases blur-focus focus-focus something-focus
	$this->lastState = 'focus';	      
      } elseif($a->params->value == 'blur'){
	if($this->lastState != 'blur' && 
	   $this->timeStamp-$this->lastTimeStamp<$cutoff){
	  // cases something-blur focus-blur
	  $this->sessionTime += $this->timeStamp-$this->lastTimeStamp;
	}
	$this->lastState = 'blur';
      }
    } else {
      if($this->lastState!='blur' && 
	 $this->timeStamp-$this->lastTimeStamp<$cutoff){
	// cases focus-something something-something
	$this->sessionTime += $this->timeStamp-$this->lastTimeStamp;
      }
      $this->lastState = 'something';
    }
  }

  function dt(){
    return $this->sessionTime-$this->lastStepTime;
  }

  function endNonSlip(){
    $this->lastStepTime = $this->sessionTime;
  }

  function update_flounder($thisTurn,$ttID){
    global $totalFloundering;
    
    if($thisTurn=='correct'){
      if($this->confused && $this->counter>1){
	// End of floundering episode.
	$dt=$this->lastIncorrectSessionTime-$this->lastCorrectSessionTime;
	$totalFloundering+=$dt;
	// $this->counter holds number of steps while floundering
      }
      $this->lastCorrectSessionTime = $this->sessionTime;
      $this->confused=false;
      $this->counter=0;
    } elseif ($thisTurn=='incorrect'){
      if(!$this->confused){
	$confusedtID=$ttID; // can be used as pointer to beginning of episode.
      }
      $this->lastIncorrectSessionTime = $this->sessionTime;
      $this->confused=true;
      $this->counter++;
    } else {
      $this->counter++;
    }
  }

  function endSession(){
    global $totalFloundering, $tt;
    
    //  Session has ended before confusion is resolved.
    if($this->confused && $this->counter>1){
      $dt=$this->sessionTime-$this->lastCorrectSessionTime;
      $totalFloundering+=$dt;
    }

    $tt+=$this->sessionTime;
  }
  

}

?>