<?php
/*
  Record a comment an update associated checkbox.
 */

require 'db-login.php';

$adminName = $_GET["au"];
$tID = $_GET["t"];
$url=$_GET["c"];
$checked=$_GET["chk"];

$sqlQuery=$db->prepare("REPLACE INTO REVIEWED_PROBLEMS(tID,userName,radioID,myComment) VALUES(?,?,?,?)");
$sqlQuery->execute(array($tID, $adminName, $checked, $url));

$result = $sqlQuery->execute();
if($result){
  echo "Updated the Record Successflly";
} else {
  echo "Error Updating Record";
}
?>
