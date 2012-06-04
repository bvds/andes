<?php

  // Session timing information.

$fProbs=array('vec1ay','kt11ay','vec3dy');


class training {

  public $fade=array(); // Object holding training information.
  public $fCut=3600; // Cutoff, in seconds of student time.
  
  function update($thisSection,$thisName,$thisProblem,
		  $sessionScore,$sessionTime){
    global $fProbs;

    // Look at intervention
    if(!isset($this->fade[$thisSection][$thisName])){
      $this->fade[$thisSection][$thisName] =
	array('tTime0' => 0, 'fTime0' => 0, // before intervention
	      'tTime1' => 0, 'fTime1' => 0, // during intervention
	      'tTime2' => 0, 'fTime2' => 0, // after intervention
	      'G' => 0, 'score' => 0, 'video' => 0);
    }
    
    $f=&$this->fade[$thisSection][$thisName];
    if($f['tTime0']+$f['tTime1']+$f['tTime2']<$this->fCut){
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
  }

  // Dump results in Mathematica format.
  function mma_dump(){
    echo "(* Total time cutoff $this->fCut s.*)\n";
    echo "fades={";
    $i=0;
    foreach($this->fade as $thisSection => $ss){
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
  
}

?>