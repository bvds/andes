<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
  <LINK REL=StyleSheet HREF="log.css" TYPE="text/css">

  <script type="text/javascript">

function createXMLHttp(){
  if(typeof XMLHttpRequest != "undefined"){
    return new XMLHttpRequest();
  } else {
    var aVersions = ["MSXML2.XMLHttp.5.0","MSXML2.XMLHttp.4.0","MSXML2.XMLHttp.3.0","MSXML2.XMLHttp","Microsoft.XMLHttp"];
    for(var i=0;i<aVersions.length;i++){
      try {
        var oXmlHttp = new ActiveXObject(aVersions[i]);
        return oXmlHttp;
      } catch(oError){

      }
    }
  }
  throw new Error("XMLHttp could not be created");
 }

function copyRecord(url){
  var oXmlHttp = createXMLHttp();
  oXmlHttp.open("GET",url,true);
  oXmlHttp.setRequestHeader("Content-Type","application/x-www-form-urlencoded");
  oXmlHttp.onreadystatechange = function(){
    if(oXmlHttp.readyState==4) {
      if(oXmlHttp.responseText.indexOf('Success')==-1){
        window.open(oXmlHttp.responseText);
        return false;
      } else {
        alert(oXmlHttp.responseText);
	location.href = location.href;
      }
    }
  }
  oXmlHttp.send(null);
}

function openTrace(url){  
  window.open(url);
}

  </script>

</head>
<body>
<?
$dbuser= $_POST['dbuser'];
$dbserver= "localhost";
$dbpass= $_POST['passwd'];
$dbname= "andes3";

mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO DATABASE at $dbserver");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE $dbname");         

$adminName=$_POST['adminName'];
$startDate = $_POST['startDate'];
$endDate = $_POST['endDate'];
$errorType=$_POST['errorType'];

  if($startDate){
    $startDatec = "P1.startTime >= '$startDate' AND";
  } else {
    $startDatec = "";
  }
  if($endDate){
    $endDatec = "P1.startTime <= '$endDate' AND";
  } else {
    $endDatec = "";
  }
  if($errorType){
    $errorTypec = "\"$errorType\"";
  } else {
    $errorTypec = "";
  }

echo "<h2>The Errors and Warnings are as given below:</h2>";
echo "<table border=1>\n";
echo "<tr><th>Starting Time</th><th>Input</th><th>Error Type</th><th>Error Message</th><th>View</th></tr>\n";


// Newer versions of php have json decoder built-in.  Should
// eventually have test for php version and use built-in, when possible.
include 'JSON.php';
$json = new Services_JSON();

$sql="SELECT startTime,userName,userProblem,userSection,tID,command,P1.clientID from PROBLEM_ATTEMPT AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 WHERE $startDatec $endDatec P2.initiatingParty='server' AND P2.command like '%\"error-type\":$errorTypec%' AND P2.command like '%\"error\":%' AND P2.clientID=P1.clientID AND P1.extra=0 order by P2.tID";
$result=mysql_query($sql);

  
while ($myrow = mysql_fetch_array($result)) {
  $tID=$myrow["tID"];  
  $usertID=$tID-3;
  $userName=$myrow["userName"];
  $userProblem=$myrow["userProblem"];
  $userSection=$myrow["userSection"];
  $startTime=$myrow["startTime"];
  $command=$json->decode($myrow["command"]);
  // Don't know why I can't just use $command->result in the foreach
  $zz=$command->result;
  $yy=array();
  foreach($zz as $bb) {
    $key1="error-type";  // work-around for the dash
    $errorType=$bb->$key1;
    $errorMsg=$bb->error;
    if($bb->action == "log"){
      array_unshift($yy,"<td>$errorType</td><td>$errorMsg</td>");
    }
  }


  $nr=count($yy);
  if($nr==0)$nr=1;

  //  $lastID=$tID-1;
  //  $userSql="select command from PROBLEM_ATTEMPT_TRANSACTION where tID=$lastID";
  $userClientID=$myrow["clientID"];
  $userSql="SELECT command from PROBLEM_ATTEMPT_TRANSACTION WHERE clientID='$userClientID' AND tID<$tID ORDER BY tID DESC LIMIT 1";

  $userResult=mysql_query($userSql);
  $myResult=mysql_fetch_array($userResult);
  $userCommand=$myResult["command"];
  $a=$json->decode($userCommand);
  $method=$a->method;
  $aa=$json->encode($a->params);
  // Escape html codes so actual text is seen.
  $aa=str_replace("&","&amp;",$aa);
  $aa=str_replace(">","&gt;",$aa);
  $aa=str_replace("<","&lt;",$aa);
  // add space after commas, for better line wrapping
  $aa=str_replace("\",\"","\", \"",$aa);
  // forward slashes are escaped in json, which looks funny
  $aa=str_replace("\\/","/",$aa);

  echo "<tr class=\"$method\"><td rowspan=\"$nr\">$startTime</td>";
  echo "<td rowspan=\"$nr\">$aa</td>";
  if(count($yy)==0){
    echo  "<td></td><td></td>";
  } else {
    echo array_shift($yy);
  }

  echo "<td rowspan=\"$nr\"><a href=\"javascript:;\" onclick=\"openTrace('OpenTrace.php?x=$dbuser&amp;sv=$dbserver&amp;pwd=$dbpass&amp;d=$dbname&amp;u=$userName&amp;p=$userProblem&amp;s=$userSection&amp;t=$tID');\">Session&nbsp;log</a><br><a href=\"javascript:;\" onclick=\"copyRecord('\Save.php?x=$dbuser&amp;sv=$dbserver&amp;pwd=$dbpass&amp;d=$dbname&amp;a=$adminName&amp;a=$adminName&amp;u=$userName&amp;p=$userProblem&amp;s=$userSection&amp;t=$usertID');\">Solution</a></td></tr>\n";

  foreach ($yy as $bb) {
    echo "<tr class=\"$method\">$bb</tr>\n";
  }

 }

echo "</table>\n";

mysql_close();
?>
</body>
</html>
