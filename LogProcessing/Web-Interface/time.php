<?php

  // Session timing information.

class session_time {

  private $lastTimeStamp=-1; // time stamp for this transaction.
  public $timeStamp=-1; // time stamp for this transaction.
  private $lastState='none'; // can be 'blur' 'focus' or 'something'
  public $sessionTime=0;  // total active time for session
  public $sessionFlounder=0;  // total flounder time for session
  private $lastStepTime=0;         // $sessionTime for last step.
  private $confused=false;
  private $counter=-1;  // number of steps while in confused state.
  public $fSteps=-1;
  public $fTime=-1;
  public $nongreenSteps=0; // Number of transactions since last green entry.
  public $nongreenTime=0;  // Number of seconds since last green entry.
  private $lastCorrectSessionTime=0; // $sessionTime for last green entry
  private $lastInorrectSessionTime=0;  // $sessionTime for last red entry
  public $confusedtID;
  public $videoTime=0;  // time spent watching video this session
  private $videoTimeStamp=-1;
  private $lastVideo='none';

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
    
    // Keep tabs of intro video usage.
    if(isset($a->method) && $a->method == 'record-action' &&
       $a->params->type == 'window' && $a->params->name == 'IntroVideo'){
      if($a->params->value == 'focus'){
	if($this->lastVideo!='focus')
	  $this->videoTimeStamp=$this->timeStamp;
	$this->lastVideo='focus';
      } elseif($a->params->value == 'blur'){
	$this->videoTime += $this->timeStamp-$this->videoTimeStamp;
	$this->lastVideo = 'blur';	  
      } 
    } else {
      if($this->lastVideo=='focus'){
	// Missing video blur
	$this->videoTime += $this->timeStamp-$this->videoTimeStamp;
      }
      $this->lastVideo = 'blur';
      $this->videoTimeStamp=$this->timeStamp;
     }

  }

  function dt(){
    return $this->sessionTime-$this->lastStepTime;
  }

  function endNonSlip(){
    $this->lastStepTime = $this->sessionTime;
  }

  // This should match code in Help/model.cl function model-flounder
  // Flounder time is time between incorrect transactions (object is marked red)
  // with no intervening correct (an object is marked green) transactions.  
  // The associated number of turns starts with the first
  // red transaction counted as zero.  Thus, any flounder episode will have
  // at least 1 turn.
  function update_flounder($thisTurn,$ttID){
    global $totalFloundering;
    
    $endFlounder=false;

    // Time and turns since last green.
    if($thisTurn=='correct'){
      $this->lastCorrectSessionTime = $this->sessionTime;
      $this->nongreenSteps=0;
      $this->nongreenTime=0;
    } else {
      $this->nongreenSteps++;
      $this->nongreenTime= $this->sessionTime-$this->lastCorrectSessionTime;
    }
      
    if($thisTurn=='correct'){
      if($this->confused && $this->fSteps>0){
	// End of floundering episode.
	$endFlounder=true;
      }
      $this->confused=false;
    } elseif ($thisTurn=='incorrect'){
      if(!$this->confused){
	// Beginning of possible flounder episode.
	$this->confusedtID=$ttID; // pointer to beginning of episode
	$this->counter=0;
	$this->fTime=0;
	$this->confused=true;
      } else {
	// It is indeed a flounder episode, update times and steps.
	$dt=$this->sessionTime-$this->lastIncorrectSessionTime;
	$this->fTime+=$dt;
	$this->sessionFlounder+=$dt;
	$totalFloundering+=$dt;
	$this->counter++;
	$this->fSteps=$this->counter;  
      }
      $this->lastIncorrectSessionTime = $this->sessionTime;
    } elseif ($this->confused) {
      $this->counter++;
    }
    return $endFlounder;
  }

  function endSession(){
    global $tt;
    
    $tt+=$this->sessionTime;
    // Are we in flounder episode?
    return $this->confused && $this->fSteps>0;
  }

}

?>
