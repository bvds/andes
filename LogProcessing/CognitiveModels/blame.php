<?php

  // Assignment of blame

// Errors to treat as special KC's instead of regular ones.
$simpleErrors=array(
		    'NO-LABEL' => 'object-label', 
		    'NO-VARIABLE-DEFINED' =>  'object-label',
		    'EQUATION-SYNTAX-ERROR' => 'equation-syntax',
		    // Andes uses radians convention
		    'TRIG-ARGUMENT-UNITS' => 'equation-syntax',
		    'SOLVE-FOR-VAR' => 'write-known-value-eqn',
		    'NOTHING-TO-MATCH-DEFINITION' => 'definition-syntax',
		    'DEFINITION-HAS-NO-MATCHES' => 'definition-syntax',
		    'DEFINITION-HAS-TOO-MANY-MATCHES' => 'definition-syntax'
		    );

// Additional kcs associated with user-interface elements
$UIKCs = array(
	       'statement' => array('definition-syntax'),
	       'equation' => array('equation-syntax'),
	       'ellipse' => array('object-label'),
	       'rectangle' => array('object-label'),
	       'vector' => array('object-label'),
	       'line' => array('object-label')
	       );

$metaHints = array(
		   '/It looks like you are having difficulty/',
		   '/click on .*the hint button.* below/',
		   '/should view the .*introductory video/',
		   '/You can click on the above link to get more help/'
		   );

function isMetaHint($text){
  global $metaHints;
  foreach ($metaHints as $hint){
    if(preg_match($hint,$text))
      return true;
  }
  return false;
}

$clarificationHints = array(
			    '/^I interpreted your definition of /'
			    );

function isClarificationHint($text){
  global $clarificationHints;
  foreach ($clarificationHints as $hint){
    if(preg_match($hint,$text))
      return true;
  }
  return false;
}

class turn_blame {
  
  // Turns that are associated with object, but missing interpretation.
  private $missingInterp=array();
  // Turns with no object or interpretation
  private $missingObject=array();  
  // For a turn without an interp, the temporal next turn kc's
  private $temporalHeuristic=array(); 
  private $orphanTurn=array();   // Candidates for temporal heuristic.
  private $KC=array();      // turns for each KC (not sorted).

  function update($turnTable,$thisObject,$a,$b){
    global $simpleErrors, $UIKCs, $allKCs;
    
    // Treat some kinds of errors as "slips":  that is,
    // they have their own special KCs and don't count
    // towards regular learning.  
    if(isset($turnTable['error']) && 
       isset($simpleErrors[$turnTable['error']])){
      // Don't associate a time with these.
      $turnTable['dt']=0;
      // For book-keeping, record this turn as if it were a kc
      // However, don't let it count it as target when 
      // figuring out time or spatial heuristic.
      // Just use object ID for instantiation.
      $this->KC[$simpleErrors[$turnTable['error']]][$thisObject][$a->id] = 
	$turnTable;
      $allKCs[$simpleErrors[$turnTable['error']]]=1;
      
    } else {
    
      // KC's associated with user interface element.
      // These exist in parallel with physics KC's
      if(isset($a->params) && isset($a->params->type) && 
	 isset($UIKCs[$a->params->type])){
	$turnTableUI=$turnTable;
	// Any errors here are associated with physics stuff:
	// These turns should be counted as correct with
	// respect to UI skills.
	unset($turnTableUI['error']);
	$turnTableUI['grade']='correct';
	$turnTable['random-help']=array();
	$turnTableUI['dt']=0; // Assume time spent was on physics
	foreach ($UIKCs[$a->params->type] as $kc){
	  $this->KC[$kc][$thisObject][$a->id] = $turnTableUI;
	  $allKCs[$kc]=1;
	}
      }
      
      $hasInterp=false;
      if(isset($b->result)){
	foreach ($b->result as $row){
	  if(isset($row->action) && $row->action == 'log' &&
	     $row->log == 'student' && isset($row->assoc)){
	    foreach($row->assoc as $kc => $inst) {
	      $allKCs[$kc]=1;
	      // If there were any previous turns without
	      // interp, give pointers to next turn kc's.
	      foreach($this->orphanTurn as $id){
		$this->temporalHeuristic[$id][]=
		  array('kc' => $kc, 'inst' => $inst);
	      }
	      // See if there were any previous turns without
	      // interp and add them.
	      if($thisObject && isset($this->missingInterp[$thisObject])){
		foreach ($this->missingInterp[$thisObject] as $id => $turn){
		  // push onto array
		  $this->KC[$kc][$inst][$id] = $turn;
		}
	      }
	      
	      // push this turn onto kc attempts
	      $this->KC[$kc][$inst][$a->id] = $turnTable;
	      $hasInterp=true;
	    }
	    $this->orphanTurn=array();
	    unset($this->missingInterp[$thisObject]);
	  }
	}
	if(!$hasInterp){
	  $this->orphanTurn[]=$a->id;
	  if($thisObject){
	    // Turn without a KC to blame
	    // In this case, we use location heuristic.
	    // If that fails, then we switch to time heuristic.
	    $this->missingInterp[$thisObject][$a->id] = $turnTable;
	  } else {
	    // Turn without a KC or object, then 
	    // look for next turn with associated object.
	    $this->missingObject[$a->id] = $turnTable;
	  }
	}
      } 
    }
  }

  function resolve($thisSection,$thisName){
    global $allStudentKC, $allKCStudent;
    
    foreach ($this->missingInterp as $id => $turns){
      foreach ($turns as $turn){
	if(isset($this->temporalHeuristic[$id])){
	  foreach($this->temporalHeuristic[$id] as $kcI){
	    $this->KC[$kcI['kc']][$kcI['inst']][$id] = $turn;
	  }
	} else {
	  // Turn has no assignment of blame.
	}
      }	  
    }
    // Do assignment of blame for entries without object.
    foreach ($this->missingObject as $id => $turn){
      if(isset($this->temporalHeuristic[$id])){
	foreach($this->temporalHeuristic[$id] as $kcI){
	  $this->KC[$kcI['kc']][$kcI['inst']][$id] = $turn;
	}
      } else {
	// Turn has no assignment of blame.
      }
    }
  
    // Sort the turns in this session,
    // consolidate help requests, and append to global lists
    foreach ($this->KC as $kc => $instTurns){
      $firstid = array();
      // sort the instances by the id of the first turn.
      // Drop id and inst, since they don't have meaning
      // outside the session.
      foreach($instTurns as $turns){
	// $turns indexed by id, so this will give time-order in session.
	ksort($turns);
	reset($turns);
	$id=key($turns);
	
	// Normalize array keys:
	$turns2=array();
	foreach($turns as $turn){
	  $turns2[]=$turn;
	}
	// Use id for first turn as index.
	$firstid[$id]=$turns2;
      }
      
      // sort KC instantiations by id of first turn.
      ksort($firstid);
      foreach($firstid as $turns){
	// Results are put into global variables.
	$allStudentKC[$thisSection][$thisName][$kc][]= $turns;
	$allKCStudent[$kc][$thisSection][$thisName][]= $turns;
      }
    }
  }
}

?>