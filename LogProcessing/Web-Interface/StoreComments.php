<?
$dbuser= "rrangan";
$dbserver= "gideon.eas.asu.edu";
$dbpass= "sin(0)=0";
$dbname= "andes";

mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE");

$adminName = $_GET["a"];
$tID = $_GET["t"];
$userName=$_GET["u"];




if($result == 1){
  echo "Updated the Record Successfully";
 } else {
  echo "Error Updating Record";
 }
mysql_close();
?>
