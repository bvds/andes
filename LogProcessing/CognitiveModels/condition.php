<?php

  // Get experiment condition information
  // for machined learned help experiment.

class experiment_condition {
  
  private $problems=array();
  
  function get_conditions($section_r){
    $query = "SELECT userSection,userName,value FROM OPEN_STUDENT_STATE where property='experiment-problems' and userSection REGEXP '$section_r' and model='server'";
    $result = mysql_query($query);
    while ($myrow = mysql_fetch_array($result)) {
      foreach(explode(' ',trim($myrow['value'],'()')) as $prob){
	$this->problems[$myrow['userSection']][$myrow['userName']][strtolower(trim($prob))]=1;
      }
    }   
  }

  function inExperiment($section, $user, $problem){
    return isset($this->problems[$section][$user][$problem]);
  }

}

?>
