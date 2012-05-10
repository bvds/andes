<?php

// Functions for handling binomial distributed quantities



function print2($pg){
  return (is_numeric($pg)?number_format($pg,2):$pg);
}

// Find the probability that S+G is less than one, in 
// which case, learning has occurred. This is calculated
// by integrating the probability distributions for G & S
// over the triangular region G+S<1.

$learnCache=array();
function learnProb($cbl,$wbl,$cal,$wal){
  global $learnCache;
  $val=learnProb2($cbl,$wbl,$cal,$wal);
  if($val<0 || $val>1){
    echo "learnProb for $cbl,$wbl,$cal,$wal";
    echo "  $val\n";
    print_r($learnCache);
  }
  // The recursion traverses a two-dimensional surface
  // in a four-dimensional space.  Thus, cached values
  // will not be reused very often for different function calls.
  // Thus, we clear cache to keep the memory use from blowing up.
  $learnCache=array();
  return $val;
}
function learnProb2($cbl,$wbl,$cal,$wal){
  global $learnCache;
  $key=$cbl . ',' . $wbl . ',' . $cal . ',' . $wal;
  // Could use symmetries to reduce size of cacheArray by 
  // a factor of four.
  if($cbl==0 && $cal==0){
    return (1+$wbl)/(2+$wbl+$wal);
  } else if($cal<$cbl){
    return 1-learnProb2($cal,$wal,$cbl,$wbl);
  } else if (isset($learnCache[$key])){
    return $learnCache[$key];
  } else {
    $val=(($cal+$wal+1)*learnProb2($cbl,$wbl,$cal-1,$wal)
	  -(1+$wal)*learnProb2($cbl,$wbl,$cal-1,$wal+1))/$cal;
    $learnCache[$key]=$val;
    return $val;
  }
}


// Confidence level for accepting a model.
// In terms of standard deviation:  0.6826895, 0.9544997, 0.9973002
$confidenceLevel=0.6826895;

function maximum_likelihood_models($opps,$debugML=false){
  global $confidenceLevel; 

  if($debugML){
    echo "Grades for this student and KC:\n";
    foreach($opps as $j => $opp){
      $yy=reset($opp);
      $gg=$yy['grade'];
      echo " $gg";
    }
    echo "\n";
  }
  
  $maxll=false;
  $allll=array();
  $allGain=array();
  // Step through possible chances for learning skill.
  for($step=0; $step<count($opps); $step++){
    if($debugML){
      echo "step $step of " . count($opps) . " ";
    }
    $cbl=0; $wbl=0; $cal=0; $wal=0;
    foreach($opps as $j => $opp){
      $yy=reset($opp);
      if($j<$step){
	if($yy['grade']=='correct'){
	  $cbl++;
	} else {
	  $wbl++;
	}
      }else{
	if($yy['grade']=='correct'){
	  $cal++;
	} else {
	  $wal++;
	}
      }
    }
    
    // Values for guess and slip are from maximum likelihood.
    $pg=$cbl+$wbl>0?$cbl/($cbl+$wbl):false;
    $ps=$cal+$wal>0?$wal/($cal+$wal):false;
    $allSlip[$step]=$ps;
    if($debugML){
      echo " hi1 ";
    }

    // Calculate the log likelihood for model with step at $step.
    // Note that this number is negative (so we are trying to maximize it)
    // Also, note that the case $step=0 corresponds to not learning.
    $ll=0;
    $ll+=$cbl>0?$cbl*log($pg):0.0;
    $ll+=$wbl>0?$wbl*log(1.0-$pg):0.0;
    $ll+=$wal>0?$wal*log($ps):0.0;
    $ll+=$cal>0?$cal*log(1.0-$ps):0.0;    
    $allll[$step]=$ll;
    
    // Expectation value for the learning gain 1-G-S.
    // This is gotten by integrating over the binomial
    // distributions P(G) and P(S).
    // For some choices of step, there is no learning.
    $allGain[$step]=1-(1+$cbl)/(2+$cbl+$wbl)-(1+$wal)/(2+$cal+$wal);
    // Only count cases where step occurs inside student data
    $gainProb=$step>0?learnProb($cbl,$wbl,$cal,$wal):0;
    $allGainProb[$step]=$gainProb;

    if($debugML){
      echo " hi3 ";
    }

    // Find maximum value.
    if($maxll===false || $ll>$maxll){
      $maxll=$ll;
      $maxv=array('logLike' => $ll, 'learn' => $step, 
		  'gainProb' => $gainProb,
		  'valid' => $gainProb>$confidenceLevel,
		  'pg' => $pg, 'ps' => $ps);
      if($step==0){
	$maxv0=$maxv;
      }
    }
    
    if($debugML){
      echo ' (' . $step . ',' . print2($pg) . ',' . 
	print2($ps) . ',' .  number_format($ll,3) . ')';
    }	
  }
  
  if($debugML){
    echo "\n";
  }
  
  // If no significant gain is seen at maximum likelihood, then 
  // model has failed, "point of learning" doesn't exist.
  // use the value from learn=0 
  if(!$maxv['valid']){
    $maxv=$maxv0;
   }

  // Find relative probabilities for learning on each step.
  //
  // We are are going from "the probability of producing seen behavior
  // for a given L" to "the probablitiy of a certain L given the 
  // seen behavior."  Thus, we are assuming that the
  // prior p.d.f. for a given L is constant.  See Section 33.1.4 of
  // http://pdg.lbl.gov/2011/reviews/rpp2011-rev-statistics.pdf
  //
  // Elements where learning cannot be determined are left empty.
  // 
  // In fact, we use AIC to determine the relative probability
  // of different models with various values of L.  When there
  // is a fit with a step, there are two model parameters.  When no 
  // learning is found ($i=0), there is one model parameter.
  // See http://en.wikipedia.org/wiki/Akaike_information_criterion

  $maxv['learnHereProb']=array();
  if($maxv['valid']){
    // First, need to normalize.
    $sum=0;
    // Include no learning case.
    for($i=0; $i<count($allll); $i++){
      // Using AIC, to determing the relative probability.
      $sum += exp($allll[$i]-($i==0?1:2));
    }
    // There is no way to measure learning on last step.
    for($i=0; $i<count($allll); $i++){
      $maxv['learnHereProb'][$i]=exp($allll[$i]-($i==0?1:2))/$sum;
    }
    // Associated learning gains
    $maxv['learnGain']=$allGain;
    $maxv['slip']=$allSlip;
    $maxv['learnGainProb']=$allGainProb;
  }
  
  if($debugML){
    if($maxv['valid']){
      echo ' model with learn=' .$maxLearn->print0() . ', pg=' . 
	$pg->print0() . ', ps=' . $ps->print0() . ', logLike=' . 
	number_format($maxv['logLike'],3) . ')';
    } else {
      echo ' no learning: ps=' . $ps->print0();
    }
    echo "\n";
  }

  return $maxv;
}

?>
