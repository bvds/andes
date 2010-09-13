<?
$dbuser=$_GET["x"];
$dbserver=$_GET["sv"];
$dbpass=$_GET["pwd"];
$dbname=$_GET["d"];

$adminName = $_GET["a"];
$tID = $_GET["t"];
$userName=$_GET["u"];
$url=$_GET["c"];
$comm=explode("&c=",$url);

function_exists('mysql_connect') or die ("Missing mysql extension");
mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE $dbserver");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE $dbname");

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
$sqlQuery="UPDATE REVIEWED_PROBLEMS SET radioID=1,myComment='$url' WHERE adminName='$adminName' AND tID=$tID";
 else
   $sqlQuery="UPDATE REVIEWED_PROBLEMS SET radioID=0,myComment='$url' WHERE adminName='$adminName' AND tID=$tID";
 
 $result=mysql_query($sqlQuery);
if($result == 1){
  echo "Updated the Record Successflly";
 } else {
  echo "Error Updating Record";
 }
mysql_close();
?>
