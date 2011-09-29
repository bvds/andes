<?php
$file = 'client_log';
// The new person to add to the file
$person = 
  // $_SERVER['REMOTE_ADDR'] . ' ' .
  // '[' . date('c',$_SERVER['REQUEST_TIME']) . '] "' . 
  'andes-client "' . 
  // Should escape quotes and backslashes in these
  $_POST['tag'] . '" "' .  $_POST['text'] . '" ' . 
  $_POST['Client-Id'] . ' "' .
  $_SERVER['HTTP_USER_AGENT'] . '"'; 
// Write the contents to the Apache log file
// Would be nice to break out into own file, Bug #1908
error_log($person,4);
?>
