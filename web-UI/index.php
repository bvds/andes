<?php
// From https://github.com/IMSGlobal/LTI-Tool-Provider-Library-PHP/wiki/Installation
/**
 * Autoload a class file.
 *
 * @param string $class The fully-qualified class name.
 */
spl_autoload_register(function ($class) {

    // base directory for the class files
    $base_dir = 'lti/src/';

    if (strpos($class, 'IMSGlobal\\LTI\\') === 0) {
      $class = substr($class, 14);
    }

    // replace the namespace prefix with the base directory, replace namespace
    // separators with directory separators in the relative class name, append
    // with .php
    
    $file = $base_dir . preg_replace('/[\\\\\/]/', DIRECTORY_SEPARATOR, $class) . '.php';
    
    // if the file exists, require it
    if (file_exists($file)) {
      require($file);
    }

});

// Suppress timezone warning.
// One can set the timeszone in the php.ini file.
if(!ini_get('date.timezone')) {
    date_default_timezone_set('GMT');
}

require "db-login.php";

use IMSGlobal\LTI\ToolProvider\DataConnector;
$db_connector = DataConnector\DataConnector::getDataConnector('', $db);

// Register new tool
// See https://github.com/IMSGlobal/LTI-Tool-Provider-Library-PHP/wiki/Usage
// This should be moved to its own service?
use IMSGlobal\LTI\ToolProvider;
if(True) {
  $cName = 'testing2.edu';
  $consumer = new ToolProvider\ToolConsumer($cName, $db_connector);
  $consumer->name = 'Testing';
  $consumer->secret = 'ThisIsASecret!';
  $consumer->enabled = TRUE;
  $consumer->save();
  print("New tool consumer $cName");
}


?>
