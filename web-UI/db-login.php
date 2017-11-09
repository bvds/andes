<?php
/*
     Dragoon Project
     Arizona State University
     (c) 2014, Arizona Board of Regents for and on behalf of Arizona State University
     
     This file is a part of Dragoon
     Dragoon is free software: you can redistribute it and/or modify
     it under the terms of the GNU Lesser General Public License as published by
     the Free Software Foundation, either version 3 of the License, or
     (at your option) any later version.
     
     Dragoon is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU Lesser General Public License for more details.
     
     You should have received a copy of the GNU Lesser General Public License
     along with Dragoon.  If not, see <http://www.gnu.org/licenses/>.
*/

  // Get database login parameters.
  // This is meant to be required by scripts that 
  // access the database.
  
  // To log into database, create file db_user_password in 
  // project root directory containing username, password
  // and database name.  Then "chmod 600 db_user_password"
$myFile = "../db_user_password";
$fh = fopen($myFile, 'r') or die("Could not find password file");
$dbuser = chop(fgets($fh));
$dbpass = chop(fgets($fh));
$dbname = chop(fgets($fh));
if(strlen($dbname)==0){
  $dbname='andes3';
}
$ltidbname = chop(fgets($fh));
if(strlen($ltidbname)==0){
  $ltidbname='lti';
}
fclose($fh);

try {
    $db = new PDO("mysql:host=localhost;dbname=$ltidbname;charset=utf8",
              $dbuser, $dbpass);
    $db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch (PDOException $e) {
    trigger_error('Could not connect to database.'. $e->getMessage());
}
