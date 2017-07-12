<?php
/*  Brett van de Sande, 2011
 *
 *  Copyright 2011 by Kurt Vanlehn and Brett van de Sande
 *   This file is part of the Andes Intelligent Tutor Stystem.
 * 
 *   The Andes Intelligent Tutor System is free software: you can redistribute
 *   it and/or modify it under the terms of the GNU Lesser General Public 
 *   License as published by the Free Software Foundation, either version 3 
 *   of the License, or (at your option) any later version.
 * 
 *   The Andes Solver is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU Lesser General Public License for more details.
 * 
 *   You should have received a copy of the GNU Lesser General Public License
 *   along with the Andes Intelligent Tutor System.  If not, see 
 *   <http://www.gnu.org/licenses/>.
 */
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
