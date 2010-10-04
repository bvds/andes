<?php
  /*
   * this include file builds up the global cgi variables $_POST and/or $_GET from 
   * the following sources (below) in the order specified.  This means that any 
   * values can be clobbered by a later source, which is the intended behavior
   *
   *  1) an rc file (currently hardcoded below as ~/.debugPhprc)
   *  2) env vars $POST & $GET 
   *  3) cmd line args 
   *
   *  the syntax is POST:var1=val1&var2=val2.. and/or GET:var1=val1&var2=val2...
   *  the POST: and GET: prefixes will be automatically added to env vars
   *  if missing. the rc file and 
   */


function parseVarLine($line){
  $line = rtrim($line);
  $tmpGet = array();
  $tmpPost = array();
  if ("GET" == substr($line, 0, 3)) {
    parse_str(substr($line, 4), $tmpGET);
    print "tmpGET = ";
    print_r($tmpGET);
    foreach(array_keys($tmpGET) as $key) {
      $_GET[$key] = $tmpGET[$key];
    }
    //print "_GET = ";
    //print_r($_GET);
  }
  else if ("POST" == substr($line, 0, 4)) {
    parse_str(substr($line, 5), $tmpPOST);
    //print "tmpPOST = ";
    //print_r($tmpPOST);
    foreach (array_keys($tmpPOST) as $key){
      $_POST[$key] = $tmpPOST[$key];
    }
    //print "_POST = ";
    //print_r($_POST);
  }
  //else{
  //  print $line . "\n";
  //}
}




function processVarFile($fileName) {
  if (file_exists($fileName)) {
    //print "processing " . $fileName . " for GET and POST var values\n";
    $fh = fopen($fileName, "r");
    while (!feof($fh)) {
      $line = fgets($fh);
      if ('#' == $line[0]) {
	//comment line
	continue;
      }
      else {
	parseVarLine($line);
      }
    }
    fclose($fh);
  }
}


function processEnvVar($varName) {
  if (array_key_exists($varName, $_ENV)) {
    //print "Processing " . $varName . "env var\n";
    $line = $_ENV[$varName];
    $hdr = $varName . ":";
    if ($hdr != substr($line, 0, strlen($hdr))) {
      $line = $hdr . $line;
    }
    parseVarLine($line);
  }
}



function dispVars() {
  print "========================\n";
  if (count($_GET) > 0) {
    print "_GET = ";
    print_r($_GET);
  }
  if (count($_POST) > 0 ) {
    print "_POST = ";
    print_r($_POST);
  }
}




//process the rc file first
$rcFile = $_ENV["HOME"] . "/.debugPhprc";
processVarFile($rcFile);

// followed by env vars 
processEnvVar("GET");
processEnvVar("POST");

//and finally the cmd line
$n = $_SERVER["argc"];
$args = $_SERVER["argv"];
for ($i=1; $i<$n; $i++) {
  parseVarLine($args[$i]);
 }

//display the final values
dispVars();



?>
