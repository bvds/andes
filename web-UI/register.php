<?php
/*
 Taken from https://github.com/IMSGlobal/LTI-Sample-Tool-Provider-PHP/blob/master/src/register.php
*/
/**
 * rating - Rating: an example LTI tool provider
 *
 * @author  Stephen P Vickers <svickers@imsglobal.org>
 * @copyright  IMS Global Learning Consortium Inc
 * @date  2016
 * @version 2.0.0
 * @license http://www.gnu.org/licenses/gpl.html GNU General Public License, Version 3.0
 */
/*
 * This page displays a UI for registering the tool with a tool consumer.
 */
  use IMSGlobal\LTI\ToolProvider;
  use IMSGlobal\LTI\ToolProvider\DataConnector;
  use IMSGlobal\LTI\Profile;
  require_once('rating_tp.php');
// Initialise session and database
  $page = '';
  $db = NULL;
  if (init($db)) {
// Register
    if ($_SERVER['REQUEST_METHOD'] == 'POST') {
      $error_msg = '';
      $url = $_SESSION['return_url'];
      if (strpos($url, '?') === FALSE) {
        $sep = '?';
      } else {
        $sep = '&';
      }
      $data_connector = DataConnector\DataConnector::getDataConnector(DB_TABLENAME_PREFIX, $db);
      $tool = new RatingToolProvider($data_connector);
      $tool->consumer = ToolProvider\ToolConsumer::fromRecordId($_SESSION['consumer_pk'], $data_connector);
      $do = $_POST['do'];
      if ($do == 'Register') {
        $ok = $tool->doToolProxyService($_SESSION['tc_profile_url']);
        if ($ok) {
          $guid = $tool->consumer->getKey();
          header("Location: {$url}{$sep}lti_msg=The%20tool%20has%20been%20registered&status=success&tool_proxy_guid={$guid}");
          exit;
        } else {
          $error_msg = 'Error setting tool proxy';
        }
      } else if ($do == 'Cancel') {
        $tool->consumer->delete();
        header("Location: {$url}{$sep}lti_msg=The%20tool%20registration%20has%20been%20cancelled&status=failure");
        exit;
      }
    }
    $page .= <<< EOD
<form action="register.php" method="post">
EOD;
    if (!empty($error_msg)) {
      $page .= <<< EOD
<p style="color: #f00; font-weight: bold;">
  {$error_msg}
</p>
EOD;
    }
    $page .= <<< EOD
<div class="box">
  <p>
    Your system meets the minimum requirements for this tool.  Click the button below to complete the registration process.
  </p>
  <p>
    <input type="submit" name="do" value="Cancel" />
    &nbsp;&nbsp;&nbsp;<input type="submit" name="do" value="Register" />
  </p>
</div>
</form>
EOD;
  }
  $title = APP_NAME;
  $page = <<< EOD
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html lang="en" xml:lang="en" xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="content-language" content="EN" />
<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
<title>{$title}</title>
<link href="css/rating.css" media="screen" rel="stylesheet" type="text/css" />
</head>
<body>
<h1>Rating Application Registration</h1>
{$page}
</body>
</html>
EOD;
// Display page
  echo $page;
?>
