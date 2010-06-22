<?
$dbuser= "root";
$dbserver= "localhost";
$dbname= "andes";
$dbpass= $_POST['passwd'];
$adminName=$_POST['adminName'];
                                                                                                                                                                       mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO THE DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE");         

$sql="select count(*) from PROBLEM_ATTEMPT AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 where P2.command like '%error-type%' AND P2.command like '%BACKTRACE%' AND P2.clientID=P1.clientID AND P1.extra=0";
$result=mysql_query($sql);
$myrow = mysql_fetch_array($result);
$count=$myrow["count(*)"];
echo "<h2>The Errors and Warnings are as given below:(Count = $count)</h2>";

$sql="select userName,userProblem,userSection,tID,command from PROBLEM_ATTEMPT AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 where P2.command like '%error-type%' AND P2.command like '%BACKTRACE%' AND P2.clientID=P1.clientID AND P1.extra=0 order by P2.tID DESC";
$result=mysql_query($sql);

  echo "<table border=1>";
  echo "<tr><th>TID</th><th>Error Type</th><th>Error Message</th><th>Additional</th></tr>";

while ($myrow = mysql_fetch_array($result)) {
  $tID=$myrow["tID"]-1;  
  $usertID=$tID-3;
  $userName=$myrow["userName"];
  $userProblem=$myrow["userProblem"];
  $userSection=$myrow["userSection"];
  $command1=$myrow["command"];
  $command2=explode("\"error-type\":\"",$command1);
  $command3=explode(",\"backtrace\"",$command2[1]);
  $command4=explode("\",\"error\":",$command3[0]);
  $errorType=$command4[0];
  $errorMsg=$command4[1];

  $firstChar=substr($errorType,1,1);
  if(firstChar!="("){
  $userSql="select command from PROBLEM_ATTEMPT_TRANSACTION where tID=$tID";
  $userResult=mysql_query($userSql);
  $myResult=mysql_fetch_array($userResult);
  $userCommand=$myResult["command"];

  echo "<tr><td>$tID</td><td>$errorType</td><td>$errorMsg</td><td><a href=\"javascript:;\" onclick=\"copyRecord('\Save.php?a=$adminName&u=$userName&p=$userProblem&s=$userSection&t=$usertID');\">View-Solution</a></td></tr>";
  $tID=$tID+1;
  echo "<tr><td>-</td><td>-</td><td><a href=\"javascript:;\" onclick=\"openTrace('\OpenTrace.php?u=$userName&p=$userProblem&s=$userSection&t=$tID');\">$userCommand</a></td><td>-</td><tr>";}
 }

echo "</table>";
mysql_close();
?>


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

function copyRecord($url){
  var oXmlHttp = createXMLHttp();
  oXmlHttp.open("GET",$url,true);
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

function openTrace($url){  
  window.open($url);
}

</script>