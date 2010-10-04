<?
$dbuser   = array_key_exists("dbuser",$_POST) ? $_POST["dbuser"] : "root";
$dbpass   = $_POST["passwd"];
$dbserver = array_key_exists("host", $_POST) ? $_POST["host"] : "localhost";
$dbname   = "andes3";

print "dbuser:   |${dbuser}|\n";
print "dbpass:   |${dbpass}|\n";
print "dbserver: |${dbserver}|\n";
print "dbname:   |${dbname}|\n";

mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE"); 
?>