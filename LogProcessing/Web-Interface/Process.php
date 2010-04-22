<?
$dbuser= "root";
$dbserver= "localhost";
$dbname= "andes";

$dbpass= $_POST['passwd'];;
$adminName = $_POST['adminName'];
$orderBy = $_POST['item'];
$order = $_POST['order'];
$filter=$_POST['filter'];

//******** BEGIN LISTING THE CONTENTS OF  testTable*********                                                                                                            
//CONNECTION STRING                                                                                                                                                     
mysql_connect($dbserver, $dbuser, $dbpass)
     or die ("UNABLE TO CONNECT TO THE DATABASE");
mysql_select_db($dbname)
     or die ("UNABLE TO SELECT DATABASE");                                                                                                                                  
setcookie("userName",$adminName,time()+(8*60*60));
setcookie("passWord",$dbpass,time()+(8*60*60));

echo "<h2>Comments given by the Andes Users, sorted in $order order of $orderBy, are as follows:</h2><BR>";
if($order=='Descending')
  $order = "DESC";
 else
   $order = "";
if($filter=='All'){
$sql = "SELECT * FROM PROBLEM_ATTEMPT AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 WHERE P1.clientID = P2.clientID AND P2.initiatingParty = 'client' AND P2.command LIKE '%\"action\":\"get-help\",\"text\":%' ORDER BY $orderBy $order";
 }
 else if($filter=='Student'){
$sql = "SELECT * FROM PROBLEM_ATTEMPT AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 WHERE P1.extra<=0 AND P1.clientID = P2.clientID AND P2.initiatingParty = 'client' AND P2.command LIKE '%\"action\":\"get-help\",\"text\":%' ORDER BY $orderBy $order";
}
 else{
$sql = "SELECT * FROM PROBLEM_ATTEMPT AS P1,PROBLEM_ATTEMPT_TRANSACTION AS P2 WHERE P1.extra>0 AND P1.clientID = P2.clientID AND P2.initiatingParty = 'client' AND P2.command LIKE '%\"action\":\"get-help\",\"text\":%' ORDER BY $orderBy $order";
 }

$result = mysql_query($sql);
if ($myrow = mysql_fetch_array($result)) {
  echo "<table border=1>";
  echo "<tr><th>Solved</th><th>My Comment</th><th>User Name</th><th>Problem</th><th>Section</th><th>Starting Time</th><th>Comment</th><th>Additional</th></tr>";
do
  {
    $tID=$myrow["tID"];
    $tempSql = "SELECT * FROM PROBLEM_ATTEMPT_TRANSACTION WHERE tID = ($tID+1) and command LIKE '%your comment has been recorded%'";
    $tempResult = mysql_query($tempSql);    
    if(mysql_fetch_array($tempResult))
      {
       $tempResult = NULL;
       $tempSql = NULL;
       $userName=$myrow["userName"];
       $userProblem=$myrow["userProblem"];
       $userSection=$myrow["userSection"];
       $startTime=$myrow["startTime"];
       $tempCommand1=$myrow["command"];
       $tempCommand2 =explode("get-help\",\"text\":\"",$tempCommand1);
       $command=explode("\"}",$tempCommand2[1]);

       $rButton="UNCHECKED";
       $extraQuery="SELECT MAX(radioID),myComment FROM REVIEWED_PROBLEMS WHERE adminName = '$adminName' AND tID = $tID";
       $extraRes=mysql_query($extraQuery);
       if ($myExtrarow = mysql_fetch_array($extraRes)) {
	 $extraField = $myExtrarow["MAX(radioID)"];
         if($extraField == 1)
	 $rButton="CHECKED";         
	 $myCom=$myExtrarow["myComment"];
	 if($myCom == null)
	   $myCom="NA";
       }
       
       echo "<tr><td><INPUT TYPE=checkbox NAME=$tID $rButton onclick=\"UpdateRecord('RecordUpdate.php?t=$tID&a=$adminName&u=$userName')\"></td><td>$myCom</td><td>$userName</td><td>$userProblem</td><td>$userSection</td><td>$startTime</td><td>$command[0]</td><td><a href=\"javascript:;\" onclick=\"copyRecord('\Save.php?a=$adminName&u=$userName&p=$userProblem&s=$userSection&t=$tID');\">View-Solution</a></td></tr>";
      }
  }
 while ($myrow = mysql_fetch_array($result));
 echo "</table>";
 }
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

function UpdateRecord($url){
  var comm=prompt("Please enter your comments","");
  var encodedComment=escape(comm);
  var oXmlHttp = createXMLHttp();
  oXmlHttp.open("GET",$url+"&c="+encodedComment,true);
  oXmlHttp.setRequestHeader("Content-Type","application/x-www-form-urlencoded");
  oXmlHttp.onreadystatechange = function(){
    if(oXmlHttp.readyState==4) {
      if(oXmlHttp.responseText.indexOf('Success')==-1){   
	alert(oXmlHttp.responseText);
	return false;      
      } else {       		
      }
    }
  }
  oXmlHttp.send(null);
}
</script>
