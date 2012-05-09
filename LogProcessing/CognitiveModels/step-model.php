<?php

// Functions for handling binomial distributed quantities


function print2($pg){
  return (is_numeric($pg)?number_format($pg,2):$pg);
}


class valErr {
  public $val;       // value
  public $e;         // standard error
  public $u = false; // upper limit standard error
  public $l = false; // lower limit standard error
  function __construct($x = false,$err = false){
    $this->val = $x;
    $this->e = $err;
  }
  function print0(){  // php doesn't allow 'print'
    return '(' . print2($this->val) .
      (is_numeric($this->e)?',' . print2($this->e):'') .
      ((is_numeric($this->l) || is_numeric($this->u))?
	',' . print2($this->l) . ',' . print2($this->u):'') . ')';
  }
}


function log_binomial($p,$params){
  if(($p<1e-15 && $params[0]>0) || (1.0-$p<1e-15 && $params[1]>0)){
    return -1;
  }
  // add constant so max is at zero.
  $phat=$params[0]/($params[0]+$params[1]);
    return ($params[0]>0?$params[0]*log($p/$phat):0)+
    ($params[1]>0?$params[1]*log((1.0-$p)/(1.0-$phat)):0)-$params[2];
}


function root_finder($function,$params,$lower,$upper){
  $yl=call_user_func($function,$lower,$params);
  $yu=call_user_func($function,$lower,$params);
  while($upper>$lower+1.0e-12){
    $x=($lower+$upper)*0.5;
    // echo "root_finder $lower $upper $yl $yu\n";
    $y=call_user_func($function,$x,$params);
    if($yl*$y>0){
      $lower=$x;
      $yl=$y;
    } else {
      $upper=$x;
      $yu=$y;
    }
  }
  return 0.5*($lower+$upper);
}

$learnCache=array();
function learnProb($cbl,$wbl,$cal,$wal){
  if($cbl==0 && $cal==0){
    return (1+$wbl)/(2+$wbl+$wal);
  } else if($cal==0){
    return 1-learnProb(0,$wal,$cbl,$wbl);
    // need to figure out how to make flat hash table for this.
  } else if (isset($learnCache[array($cbl,$wbl,$cal,$wal)])){
    return $learnCache[array($cbl,$wbl,$cal,$wal)];
  } else {
    return // formula
  }
}

function binomial_errors($p,$c,$w){
  // Analytic formula (Should be good away from endpoints).
  $p->e=sqrt($p->val*(1.0-$p->val)/($c+$w));
  // Exact upper and lower limits for binomial distribution.
  $p->l=$p->val-root_finder('log_binomial',array($c,$w,-0.5),0,$p->val);
  $p->u=root_finder('log_binomial',array($c,$w,-0.5),$p->val,1)-$p->val;
}


// Calculate weighted average for asymmetric errors.
// See http://arxiv.org/abs/physics/0401042
function average_model($ss,$val,$valid){
  $sumVal=0;
  $sumWeight=0;
  $countValid=0;
  $countAll=0;
  foreach($ss as $thisSection => $st){
    foreach($st as $thisName => $maxv){
      $x=$maxv[$val];
      if(!is_object($x) || get_class($x) != 'valErr'){
	echo "Bad class for $val:\n";
	print_r($maxv);
      }
      $countAll++;
      if($valid?$maxv['valid']:!$maxv['valid']){
	$countValid++;
	$sigma=0.5*($x->u+$x->l);
	$alpha=0.5*($x->u-$x->l);	
	$weight=1.0/($sigma*$sigma+2.0*$alpha*$alpha);
	$sumVal += $x->val*$weight;
	$sumWeight += $weight;
      }	
    }
  }
  return array('val' =>
	       new valErr($sumWeight>0?$sumVal/$sumWeight:false,
			  // From http://en.wikipedia.org/wiki/Weighted_mean
			  // "Dealing with variance" 
			  // Don't know how to extend to asymmetric errors:
			  // just a guess.
			  $sumWeight>0?1.0/sqrt($sumWeight):false),
	       'countValid' => $countValid,
	       'countAll' => $countAll);
}


function maximum_likelihood_models($opps,$debugML=false){
  
  if($debugML){
    echo "$thisSection $thisName $kc\n";
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
    
    // Values are from maximum liklihood.
    // This is the mean and variance of the binonial distribution
    $pg=new valErr($cbl+$wbl>0?$cbl/($cbl+$wbl):false);
    $ps=new valErr($cal+$wal>0?$wal/($cal+$wal):false);
    // Note that this number is negative (so we are trying to maximize it)
    // Also, note that the case $step=0 corresponds to not learning.
    $ll=0;
    $ll+=$cbl>0?$cbl*log($pg->val):0.0;
    $ll+=$wbl>0?$wbl*log(1.0-$pg->val):0.0;
    $ll+=$wal>0?$wal*log($ps->val):0.0;
    $ll+=$cal>0?$cal*log(1.0-$ps->val):0.0;
    
    $allll[$step]=$ll;
    // Expectation value for the learning gain 1-G-S.
    // This is gotten by integrating over the binomial
    // distributions P(G) and P(S).
    $thisGain=1-(1+$cbl)/(2+$cbl+$wbl)-(1+$wal)/(2+$cal+$wal);
    // this is gotten by finding the variance of G+S and taking square root:
    $thisGainErr=sqrt((1+$cbl)*(1+$wbl)/(pow(2+$cbl+$wbl,2)*(3+$cbl+$wbl))
		     +(1+$cal)*(1+$wal)/(pow(2+$cal+$wal,2)*(3+$cal+$wal)));
    // For some choices of step, there is no learning.
    $allGain[$step]=($thisGain>0?$thisGain:0);
    $allSlip[$step]=$ps->val;
    
    // Find maximum value.
    if($maxll===false || $ll>$maxll){
      $maxll=$ll;
      $learn=new valErr($step);
      $maxv=array('logLike' => $ll, 'learn' => $learn, 
		  'pg' => $pg, 'ps' => $ps,
		  'cbl' => $cbl, 'wbl' => $wbl, 'cal' => $cal, 'wal' => $wal);
      if($step==0){
	$maxv0=$maxv;
      }
    }
    
    if($debugML){
      echo ' (' . $learn->val . ',' . print2($pg->val) . ',' . 
	print2($ps->val) . ',' .  number_format($ll,3) . ')';
    }	
  }
  
  if($debugML){
    echo "\n";
  }
  
  // To get standard error for learning step: calculate numerically
  // range of $ll that is above maxll-s^2/2 (for s standard deviations).
  // Probably want an interval since it doesn't look very parabolic
  // in most examples.
  // Also, may just want to use value of $ll for P(G) and P(S) minimized
  // (as we have calculated above):
  // this would take into account any correlations.
  $llDev=$maxv['logLike']-1/2;
  if($allll[0]>$llDev){
    $maxv['learn']=new valErr();  // can't determine point of learning.
  } else {
    $maxLearn=$maxv['learn'];
    foreach($allll as $learn => $ll){
      if($ll>$llDev){
	$maxLearn->u = $learn-$maxLearn->val+0.5; // half a step
	if($maxLearn->l===false){
	  $maxLearn->l = $maxLearn->val-$learn+0.5; // half a step
	} 
      }
    }
  }
  
  // Add error estimates to P(G) and P(S).
  $pg=$maxv['pg']; $ps=$maxv['ps'];
  if($maxv['cbl']+$maxv['wbl']>0)
    binomial_errors($pg,$maxv['cbl'],$maxv['wbl']);
  binomial_errors($ps,$maxv['wal'],$maxv['cal']);
  
  // If no significant improvement is actually seen, then 
  // model has failed, "point of learning" doesn't exist.
  //
  // The correct way to add errors is here:
  // http://statistics.stanford.edu/~ckirby/techreports/ONR/SOL%20ONR%20467.pdf 
  // However, we simply add upper limit errors in 
  // quadrature, which is only really correct in large N limit.      
  $maxv['valid']=is_numeric($maxv['learn']->val) && 
    is_numeric($pg->val) && is_numeric($ps->val) && 
    $pg->val+$ps->val+sqrt($pg->u*$pg->u+$ps->u*$ps->u)<1.0;
  
  if(!$maxv['valid'] && is_numeric($maxv['learn']->val)){
    // If no significant learning was found with best fit,
    // use the value from learn=0 for P(S).
    $maxv=$maxv0;
    $maxv['valid']=false;  // Note that no learning found.
    // Recalculate associated errors.
    binomial_errors($maxv['ps'],$maxv['wal'],$maxv['cal']);
  }

  // Find relative probabilities for learning on each opportunity
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

  $maxv['learnProb']=array();
  if($maxv['valid']){
    // First, need to normalize.
    $sum=0;
    // Include no learning case.
    for($i=0; $i<count($allll); $i++){
      // Using AIC, to determing the relative probability.
      $sum += exp($allll[$i]-($i==0?1:2));
    }
    // There is no way to measure learning on last opportunity.
    for($i=0; $i<count($allll); $i++){
      $maxv['learnProb'][$i]=exp($allll[$i]-($i==0?1:2))/$sum;
    }
    // Associated learning gains
    $maxv['learnGain']=$allGain;
    $maxv['slip']=$allSlip;
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
