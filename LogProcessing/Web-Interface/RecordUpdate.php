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

$maxQuery = "SELECT MAX(radioID) FROM REVIEWED_PROBLEMS WHERE adminName='$adminName' AND tID=$tID";
$maxResult = mysql_query($maxQuery);
$maxResultArray = mysql_fetch_array($maxResult);
$maxValue = $maxResultArray["MAX(radioID)"];
if($maxValue == null)
  {
    $reviewQuery="INSERT INTO REVIEWED_PROBLEMS(userName,tID,adminName) VALUES('$userName',$tID,'$adminName')";
    mysql_query($reviewQuery);   
  }
$maxResult = mysql_query($maxQuery);
$maxResultArray = mysql_fetch_array($maxResult);
$maxValue = $maxResultArray["MAX(radioID)"];
if($maxValue == 0)
$sqlQuery="UPDATE REVIEWED_PROBLEMS SET radioID=1 WHERE adminName='$adminName' AND tID=$tID";
 else
   $sqlQuery="UPDATE REVIEWED_PROBLEMS SET radioID=0 WHERE adminName='$adminName' AND tID=$tID";
 
 $result=mysql_query($sqlQuery);
if($result == 1){
  echo "Updated the Record Successfully";
 } else {
  echo "Error Updating Record";
 }
mysql_close();
?>
