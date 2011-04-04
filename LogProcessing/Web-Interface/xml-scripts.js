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
	console.log("copyRecord called with ",$url);
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
