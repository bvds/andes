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
	       'ellipse' => array('object-label','definition-syntax'),
	       'rectangle' => array('object-label','definition-syntax'),
	       'vector' => array('object-label','definition-syntax'),
	       'line' => array('object-label','definition-syntax')
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

$reversibleErrors = array(
			  'ophint',
			  'variable-with-wrong-time',
			  'only-body-wrong',
			  'wrong-body-speed',
			  'velocity-not-speed',
			  'wrong-type-coef-friction',
			  'wrong-body-energy',
			  'wrong-order-potential-energy',
			  'wrong-body-potential-energy',
			  'wrong-agent-for-energy',
			  'wrong-spring-compression-distance',
			  'wrong-spring-for-spring-constant',
			  'wrong-body-for-height',
			  'wrong-body-moment-of-inertia',
			  'wrong-body-radius-revolution',
			  'wrong-body-radius-of-circle',
			  'wrong-body-length-width',
			  'wrong-body-for-work',
			  'wrong-agent-for-work',
			  'wrong-body-net-work',
			  'massless-body',
			  'wrong-object-for-body-tool',
			  'need-to-rotate-axes',
			  'vector-time',
			  'vector-time-inside-correct-time',
			  'vector-wrong-time-and-direction',
			  'default-wrong-body',
			  'default-vector-body',
			  'default-wrong-line',
			  'velocity-should-be-non-zero',
			  'velocity-should-be-zero',
			  'deceleration-bug',
			  'force-on-whole-not-pt',
			  'force-wrong-agent',
			  'net-force-only-on-body',
			  'force-type-unspecified',
			  'wrong-force-type-single',
			  'wrong-force-type-dual-undone',
			  'wrong-force-type-dual-frict-done',
			  'wrong-force-type-dual-other-done',
			  'wrong-force-agent-and-type',
			  'weight-due-to-object',
			  'normal-force-mistyped',
			  'switched-objects-for-force',
			  'normal-force-direction',
			  'friction-force-not-parallel',
			  'static-friction-force-sense',
			  'kinetic-friction-force-sense',
			  'net-force-straight',
			  'wrong-applied-pt-torque',
			  'wrong-pivot-pt-torque',
			  'wrong-pivot-net-torque',
			  'wrong-pt-relative-position',
			  'wrong-ref-pt-relative-position',
			  'opposite-relative-position',
			  'wrong-ref-pt-relative-vel',
			  'field-wrong-loc',
			  'field-loc-too-specific',
			  'field-wrong-agent',
			  'field-wrong-type',
			  'flux-wrong-type',
			  'dipole-moment-wrong-type',
			  'substitute-mag-vars',
			  'used-magnitude-instead-of-component',
			  'trig-argument-units',
			  'missing-axis-tilt',
			  'default-sign-error',
			  'missing-negation-on-vector-component',
			  'missing-negation-on-vector-magnitude',
			  'hint-drawn-quants',
			  'two-force-sums',
			  'wrong-given-value',
			  // These aren't so obvious:
			  'missing-forces-in-x-axis-sum',
			  'missing-forces-in-y-axis-sum',
			  'missing-torques-in-z-axis-sum'
			  );

function isReversibleError($text){
  global $reversibleErrors;
  foreach ($reversibleErrors as $errorOp){
    if(strcasecmp($errorOp,$text)==0)
      return true;
  }
  return false;
}



// There should have been a tutor log message for these,
// but there isn't, so we have to use the hint text instead.
$reversibleHints = array(
			 '/ is not the correct value for .* can be determined /',
			 '/^The correct value for .* is /',
			 '/^Are you sure you want the variable defined /',
			 '/^Although you need a variable for .* solution I know of needs it /',
			 '/^Define a variable for .* instead of /',
			 '/^Are you sure you want to choose /',
			 '/^Perhaps you should choose .* instead./',
			 '/^You.ll need a speed variable, but maybe not for /',
			 '/^Define the speed variable for .* instead of /',
			 '/^On this problem, you should use velocity instead of speed./',
			 '/^Delete the speed variable and draw a velocity vector for /',
			 '/^Think about what type of friction occurs in this problem/',
			 '/^Kinetic friction acts on an object as it moves with /',
			 '/^Change the coefficient of friction type from /',
			 '/^Are you sure it.s .* that you want .* of/',
			 '/^No solution I know needs that energy/',
			 '/^Define .* for .* instead of /',
			 '/^Although interactions are symmetrical, Andes uses /',
			 '/^Re-order the arguments in your definition to define the /',
			 '/^Are you sure .* is the primary body for which to consider /',
			 '/^Andes uses the convention that the body argument /',
			 '/^You could define .* using .* instead of .* in the body slot/',
			 '/^Is .* the agent of a conservative force on .* that gives rise to /',
			 '/^Instead of defining .* as due to interaction with /',
			 '/^The compression distance should be specified /',
			 '/^You should define the compression /',
			 '/^The spring constant should be specified for /',
			 '/^You should define the spring constant of /',
			 '/^You should specify the height of the body /',
			 '/^Define the height of /',
			 '/^Are you sure you need the moment of inertia of /',
			 '/^Define the moment-of-inertia for /',
			 '/^Are you sure you need the radius of /',
			 '/^Choose an object that is moving in a circle/',
			 '/^Because .* is moving in a circle, /',
			 '/^Are you sure it.s .* that you want the radius of/',
			 '/^Define a variable for the radius of /',
			 '/^Are you sure you need the .* of /',
			 '/^Define the .* of .* instead of /',
			 '/^You can define a variable for the work /',
			 '/^Try defining the work on /',
			 '/^An object does work on a moving body when /',
			 '/^Instead of defining the work done by /',
			 '/^You can define a variable for the net work /',
			 '/^Try defining the net work done /',
			 '/^It is okay to choose a massless object /',
			 '/^Although .* has mass, it is not /',
			 '/^What other bodies can you define/',
			 '/^Analyzing .* is more important for solving /',
			 '/^Choose .* as the body/',
			 '/^Try a rotated coordinate system/',
			 '/^Rotating the coordinate system can /',
			 '/^You should rotate the axes by /',
			 '/^You don.t need to draw /',
			 '/^You should draw /',
			 '/^Although .* certainly exists /',
			 '/^Define .* instead of /',
			 '/^Both the time specification and /',
			 '/^None of the solutions that I know of contain /',
			 '/^If you are unsure of what to do you can call /',
			 '/^You should draw .* with a .* direction/',
			 '/^Are you sure you want the body on that vector to be /',
			 '/^Maybe the body should be /',
			 '/^Are you sure you want to define a vector for /',
			 '/^A better choice of body .but maybe not the /',
			 '/^Are you sure you want to draw /',
			 '/^You should draw /',
			 '/^Is .* is at rest ~/',
			 '/^Is ~a at rest ~/',
			 '/^Since .* is not at rest /',
			 '/^Are you sure the velocity of /',
			 '/^Because *. is at rest .*, its velocity /',
			'/^Click on this vector and change its length to be /',
			 '/^Notice that .* is slowing down/',
			 '/^When an body is moving in a straight line /',
			 '/^Make the direction of the acceleration be /',
			'/^Although that force does act on /',
			 '/^Define the force to act on /',
			 '/^Is the force really /',
			 '/^Forces are caused by interacting objects/',
			'/^Draw a .* force on /',
			 '/^None of the solutions that I know of include a single-force vector /',
			 '/^For this problem you only need to define a net-force vector /',
			 '/^You need to define a net-force on /',
			 '/^You need to specify the .*kind.* of /',
			 '/^It is a .* force/',
			 '/^There is indeed a force on /',
			 '/^There are two forces on /',
			 '/^Change the force type to either /',
			 '/^There are indeed forces on /',
			 '/^You should draw a .*force /',
			 '/^Andes. solution does not mention any /',
			 '/^You could try drawing a /',
			 '/^Although there is a weight force acting on /',
			 '/^The weight force is due to the interaction /',
			 '/^Change the definition of the force so it is due /',
			 '/^Is .* really the type of force you /',
			 '/^When a surface pushes on an object, the /',
			 '/^Double click on the force and change the type /',
			 '/^Judging from the direction and type of force /',
			 '/^Try defining the force on /',
			 '/^Are you sure the normal force is straight /',
			 '/^The normal force is perpendicular /',
			 '/^Because the normal force on /',
			 '/^Think about the direction of the force/',
			 '/^Friction forces are always parallel /',
			 '/^The direction of the force /',
			 '/^Think about the direction of the force/',
			 '/^To figure out which way static friction points/',
			 '/^The direction of the force should be /',
			 '/^Kinetic friction opposes the direction /',
			 '/^Notice that .* while it moves in a straight line/',
			 '/^Whenever a body is moving in a straight line /',
			 '/^Because .*, it has non-zero acceleration/',
			 '/^You specified that the point where the force /',
			 '/^Instead of .* you should specify a point /',
			 '/^For instance, define the .* due to a force /',
			 '/^You.ve picked .* as the point that .* rotates /',
			 '/^Because .* rotates about .*, /',
			 '/^Do you really want the relative position of /',
			 '/^Perhaps you should define the relative position /',
			 '/^You.ve picked .* as the reference point/',
			 '/^You probably want to define the relative position /',
			 '/^The relative position you defined /',
			'/^For this problem you need to use the relative position /',
			 '/^For doppler problems, you should define /',
			 '/^Define the velocity of /',
			 '/^Are you sure you want to consider the .* field /',
			 '/^A better choice of location .but maybe not the/',
			 '/^Yes, an .* field exists at /',
			 '/^A more general choice of location /',
			 '/^Is .* the source of the .* field/',
			 '/^A better choice .but maybe not the only one. /',
			 '/^Is the field really supposed to be /',
			 '/^You probably want the .* field/',
			 '/^Is the flux really supposed to be /',
			 '/^You probably want the /',
			 '/^Is the dipole moment really supposed to be/',
			 '/^Did you really mean to use /',
			 '/^I am not sure this is what you intended, but /',
			 '/^You used .*, which is the MAGNITUDE of /',
			 '/^Instead of the magnitude, perhaps/',
			 '/^Radians are the default unit for angles/',
			 '/^Replace .* with .* deg. in your equation/',
			 '/^Remember that the axes are rotated by /',
			 '/^Because the positive x axis is /',
			 '/^Check your signs/',
			 '/^Perhaps the sign of the /',
			 '/^Think about the direction of /',
			 '/^Changing the sign on /',
			 '/^Perhaps you are confusing the MAGNITUDE /',
			 '/^Because the vector is parallel /',
			 '/^Did you leave a .* out of your sum of /',
			 '/^You left .* out of your sum /',
			 '/^Some .* may be missing from your sum/',
			 '/^You left several /',
			 '/^You have written an equation of the form /',
			 '/^You can rewrite your equation /'
			 );

// This is obviously going to be pretty slow, so we 
// should check that turn is incorrect and that there
// is no tutor logging describing hint.  
function isReversibleHint($text){
  global $reversibleHints;
  foreach ($reversibleHints as $hint){
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
  // Max number of turns associated with an object 
  // where a switch to temporal heuristic is permitted
  public $switchToTemporalCutoff=1;  

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
	$turnTableUI['random-help']=array();
	$turnTableUI['dt']=0; // Assume time spent was on physics
	foreach ($UIKCs[$a->params->type] as $kc){
	  $this->KC[$kc][$thisObject][$a->id] = $turnTableUI;
	  $allKCs[$kc]=1;
	}
      }
      
      $hasInterp=false;
      if(isset($b->result)){
	foreach ($b->result as $row){
	  // KC found in 
	  // "action":"log","log":"student","assoc":<list of KCs>
	  // 
	  if(isset($row->action) && $row->action == 'log' &&
	     $row->log == 'student' && isset($row->assoc)){
	    foreach($row->assoc as $kc => $inst) {
	      $allKCs[$kc]=1;
	      // If there were any previous turns without
	      // interp, create pointers to this turn's KCs.
	      foreach($this->orphanTurn as $id){
		$this->temporalHeuristic[$id][]=
		  array('kc' => $kc, 'inst' => $inst);
	      }
	      // See if there were any previous turns for
	      // this object without interp and add them.
	      if($thisObject && isset($this->missingInterp[$thisObject])){
		foreach ($this->missingInterp[$thisObject] as $id => $turn){
		  // push onto array
		  $this->KC[$kc][$inst][$id] = $turn;
		}
	      }
	      // See if there were any previous turns
	      // with out an associated object.  Use temporal
	      // heuristic and add them to the current KC.
	      if(isset($this->missingObject)){
		foreach($this->missingObject as $id => $turn){
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
	    $this->missingObject=array();
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
    
    // Assignment of blame for entries associated with an object
    // Can only do this after the fact, since student may 
    // do further steps on object, changing which heuristic 
    // to use.
    foreach ($this->missingInterp as $object => $turns){
      foreach ($turns as $id => $turn){
	// Only if there are a few turns in an object
	// do we switch to temporal hueristic.
	// Empirically, we find that if a student has
	// spent a long time working on an object without
	// success, they then switch to doing something else.
	if(count($turns)<=$this->switchToTemporalCutoff &&
	   isset($this->temporalHeuristic[$id])){
	  foreach($this->temporalHeuristic[$id] as $kcI){
	    $this->KC[$kcI['kc']][$kcI['inst']][$id] = $turn;
	  }
	} else {
	  // Turn has no assignment of blame.
	  $this->KC['none']['none'][$id] = $turn;
	}
      }	 
    }

    // For remaining entries without object, assignment
    // of blame has failed.
    foreach ($this->missingObject as $id => $turn){
      // Turn has no assignment of blame.
      $this->KC['none']['none'][$id] = $turn;
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

      // Save session results in global variables.
      foreach($firstid as $turns){
	$firstTurn=reset($turns);
	$firstTurn['kc']=$kc;
	$allStudentKC[$thisSection][$thisName][]=$firstTurn; 
	$allKCStudent[$kc][$thisSection][$thisName][]=$turns;
      }
    }
  }
}

?>