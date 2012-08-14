<?php

// Functions for handling binomial distributed quantities



function print2($pg){
  return (is_numeric($pg)?number_format($pg,2):$pg);
}

// Find the probability that S+G is less than one, in 
// which case, learning has occurred. This is calculated
// by integrating the probability distributions for G & S
// over the triangular region G+S<1.

$logFactorialCache=array();
function logFactorial($x){
  global $logFactorialCache;
  if($x==0){
    return 0;
  }else if(isset($logFactorialCache[$x])){
    return $logFactorialCache[$x];
  } else {
    $val=log($x)+logFactorial($x-1);
    $logFactorialCache[$x]=$val;
    return $val;
  }
}

// Find probability that positive learning has occurred.
// This is not used anywhere since we just use normalized
// gain and include everything.

function learnProb($a,$b,$c,$d){
  if($c==0){
    return exp(logFactorial(1+$b+$d)+logFactorial(1+$a+$b)
	       -logFactorial($b)-logFactorial(2+$a+$b+$d));
  } else {
    return learnProb($a,$b,$c-1,$d+1)
      +exp(logFactorial($c+$d+1)+logFactorial($a+$c)+logFactorial($a+$b+1)
	   +logFactorial($b+$d+1)-logFactorial($a+$b+$c+$d+2)
	   -logFactorial($a)-logFactorial($b)-logFactorial($c)
	   -logFactorial($d+1));
  }
}


function maximum_likelihood_models($opps,$debugML=false){

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

    // Calculate the log likelihood for model with step at $step.
    // Note that this number is negative (so we are trying to maximize it)
    // Also, note that the case $step=0 corresponds to not learning.
    $ll=0;
    $ll+=$cbl>0?$cbl*log($pg):0.0;
    $ll+=$wbl>0?$wbl*log(1.0-$pg):0.0;
    $ll+=$wal>0?$wal*log($ps):0.0;
    $ll+=$cal>0?$cal*log(1.0-$ps):0.0;
    $allll[$step]=$ll;
    
    // Maximum likelihood estimator for the learning gain 1-g-s
    $allGain[$step]=($ps && $pg)?1-$ps-$pg:0;

    // Find maximum value.
    if($maxll===false || $ll>$maxll){
      $maxll=$ll;
      // Only count cases where step occurs inside student data
      $maxv=array('logLike' => $ll, 'learn' => $step, 
		  'pg' => $pg, 'ps' => $ps);
      if($step==0){
	$maxv0=$maxv;
      }
    }
    
    if($debugML){
      echo ' (' . $step . ',' . print2($pg) . ',' . 
	print2($ps) . ',' .  number_format($ll,3) . ',' . ')';
      // echo " <$cbl,$wbl,$cal,$wal>";
    }	
  }
  
  if($debugML){
    echo "\n";
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

  $maxv['opps']=count($opps);
  $maxv['learnHereProb']=array();
  $sum=0;
  // Include no learning case.
  // There is no way to measure learning on last step.
  for($i=0; $i<count($allll); $i++){
    // Using AIC, to determing the relative probability.
    $val= exp($allll[$i]-($i==0?1:2));
    $maxv['learnHereProb'][$i]=$val;
    $sum += $val;
  }
  // Next, normalize probabilities;
  for($i=0; $i<count($allll); $i++){
    $maxv['learnHereProb'][$i]/=$sum;
  }

  // If no significant model-weighted gain is seen, then 
  // model has failed, "point of learning" doesn't exist.
  // There are two possible strategies to determine learning:  
  // 1.  The best fit parameters predict positive learning or
  // 2.  The model predicts positive learning with probability
  //     greater than some confidence level.
  //
  // The model weighted gain is about 1/2 for any data
  // where the initial rate cbl/(cbl+wbl) is equal to the 
  // final rate cal/(cal+wal).
  // This includes all correct, all wrong, or some
  // random rate of correctness.

  //  get all fits where there is enough data to fit.
  $maxv['valid']=(count($opps)>=3) || 
    (($maxv['learn']>0) && ($maxv['ps']+$maxv['pg']<1));
  // use the value from learn=0 
  if(!$maxv['valid']){
    $maxv['learn']=0;
    $maxv['pg']=false;
    $maxv['ps']=$maxv0['ps'];
    $maxv['logLike']=$maxv0['logLike'];
  }

  // Associated learning gains
  $maxv['learnGain']=$allGain;
  $maxv['slip']=$allSlip;
  
  if($debugML){
    if($maxv['valid']){
      echo ' model with pg=' . number_format($pg,3) . 
	', ps=' . number_format($ps,3) . ', logLike=' . 
	number_format($maxv['logLike'],3);
    } else {
      echo ' no learning: ps=' . number_format($ps,2);
    }
    echo "\n";
  }

  return $maxv;
}

?>
