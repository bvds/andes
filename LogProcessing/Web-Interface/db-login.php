<?php
/*  Brett van de Sande, 2017
 *
 *  Copyright 2017 by Kurt Vanlehn and Brett van de Sande
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
 *   <http:;;;www.gnu.org/licenses/>.
 */

/* Parameters may have a short form or a long form,
   except for the server host name. */ 
$dbuser = isset($_REQUEST['x'])?$_REQUEST['x']:$_REQUEST["dbuser"];
$dbserver = isset($_REQUEST['sv'])?$_REQUEST['sv']:"localhost";
$dbpass = isset($_REQUEST['pwd'])?$_REQUEST['pwd']:$_REQUEST["passwd"];
$dbname = isset($_REQUEST['d'])?$_REQUEST['d']:$_REQUEST["dbname"];

try {
    $db = new PDO("mysql:host=$dbserver;dbname=$dbname;charset=utf8",
              $dbuser, $dbpass);
    $db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch (PDOException $e) {
    trigger_error('Could not connect to database.'. $e->getMessage());
}
?>