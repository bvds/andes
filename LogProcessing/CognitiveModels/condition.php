<?php

  // Get experiment condition information
  // for machined learned help experiment.

class experiment_condition {

  public $problems=array();
  public $tID=array();

  function get_conditions($section_r){
    $query = "SELECT userSection,userName,tID,value FROM OPEN_STUDENT_STATE where property='experiment-problems' and userSection REGEXP '$section_r' and model='server'";
    $result = mysql_query($query);
    while ($myrow = mysql_fetch_array($result)) {
      $this->problems[$myrow['userSection']][$myrow['userName']]=
	explode(' ',trim($myrow['value'],'() '));
      $this->tID[$myrow['userSection']][$myrow['userName']]=$myrow['tID'];
      // echo "got this:  "  . $this->tID[$myrow['userSection']][$myrow['userName']] . "\n";
      // print_r($this->problems[$myrow['userSection']][$myrow['userName']]);
    }
  }

}

?>
