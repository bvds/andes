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

?>
