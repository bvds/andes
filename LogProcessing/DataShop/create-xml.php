<?php
  //
  // This expects the database tables CLASS_INFORMATION SET and
  // STUDENT_DATASET to have entries for given section.
  // See README file for details
  //
  // (Sigh) The code that Anirudh wrote contains lots of 
  // code duplication.

include 'JSON.php';
$userSection='^asu_7e256268bab914fb5asul1_';
$startTime='2011-04-01';

// To log into database, create file db_user_password
// with user name and password.  then "chmod 600 db_user_password"
$myFile = "db_user_password";
$fh = fopen($myFile, 'r');
$dbuser = chop(fgets($fh));
$dbpass = chop(fgets($fh));
$dbname = chop(fgets($fh));
if(strlen($dbname)==0){
  $dbname='andes3';
 }
fclose($fh);

$db = new PDO("mysql:dbname=$dbname;host=localhost",$dbuser,$dbpass);
// The following tells PDO we want it to throw Exceptions for every error.
// This is far more useful than the default mode of throwing php errors
$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_WARNING);

// Get list of problems for Summer 2012 experiment
$query="SELECT userName,value from OPEN_STUDENT_STATE where userSection REGEXP '$userSection' and property='EXPERIMENT-PROBLEMS'";
$experimentProblems=array();
foreach($db->query($query) as $stud){
  foreach(explode(' ',trim($stud['value'],'()')) as $prob){
    $experimentProblems[$stud['userName']][strtolower(trim($prob))]=1;
  }
}
//print_r($experimentProblems);
  
// Since this is for reasearch purposes, access only anonymized ids.
$query="select distinct pa.userName FROM OPEN_PROBLEM_ATTEMPT pa WHERE pa.userSection REGEXP '$userSection' and pa.startTime > '$startTime'";
// Since we do queries inside the loop, get result array all at once.
foreach($db->query($query)->fetchAll() as $rowfirst)
{
  $userName = $rowfirst['userName'];
  // For debuggging
  if(false && preg_match('/^(bvds|md5:3f6382444a)/',$userName)==0){
    continue;
  }
  echo "*********************\nCreating $userName ";
  //Create context message element
  $doc = new DOMDocument();  // Creating new document
  $doc->load( 'structure.xml' ); // Loading from the default structure
  $root = $doc->getElementsByTagName('tutor_related_message_sequence')->item(0);
  $isValid = TRUE;
  //selecting a specific username from problem attempt table and creating a context message for various clientId from problem attempt table of the username
  $query="SELECT pa.clientID, pa.startTime, pa.userProblem, pa.userSection, ci.name, ci.school, ci.period, ci.description, ci.instructorName, ci.schoolyearInfo, ci.datasetID, sd.datasetname, sd.modulename, sd.groupname, sd.problemname from OPEN_PROBLEM_ATTEMPT pa, CLASS_INFORMATION ci, STUDENT_DATASET sd WHERE pa.userSection = ci.classSection and ci.datasetID = sd.datasetID and pa.userSection REGEXP '$userSection' and pa.startTime > '$startTime' and pa.userName = '$userName'";
  //echo "query: $query\n";  // for debug
  $ss=0;
  echo "0 ";
  // Since we do queries inside the loop, get result array all at once.
  foreach ($db->query($query)->fetchAll() as $row)
    { 
      $ss++; echo "$ss ";
      $context_msg_el = $doc->createElement("context_message");
      $context_msg_el->setAttribute('context_message_id', $row['clientID']);
      $context_msg_el->setAttribute('name', 'START_PROBLEM');
      
      // Creating Meta tag
      
      $meta = $doc->createElement("meta");
       //$toolmeta = $doc->createElement("meta");
      
      $user_id = $doc->createElement("user_id");
      $user_id->setAttribute('anonFlag', "true");
      $user_id->nodeValue = $userName;
      
      // Append user id into meta
       $meta->appendChild($user_id);
       $sessionID = $doc->createElement("session_id");
       //$user_id->setAttribute('anonFlag', "true");
       $sessionID->nodeValue = $row['clientID'];
       $meta->appendChild($sessionID);
       // Append session_id to meta
       $timerec = $doc->createElement("time");
       //$user_id->setAttribute('anonFlag', "true");
       $timerec->nodeValue = $row['startTime'];
       $meta->appendChild($timerec);
       // Append time to meta
       $timezone = $doc->createElement("time_zone");
       //$user_id->setAttribute('anonFlag', "true");
       $timezone->nodeValue = 'MST';
       $meta->appendChild($timezone);
       //Append timezone to meta
    
       
       $context_msg_el->appendChild($meta);
       //Append meta as child element to context_message
        //*******
       
       //Creating child element class
       $class = $doc->createElement("class");
       
       $class_name = $doc->createElement("name");
       $class_name->nodeValue = $row['name'];
    
       // Append name as child element to class
       $class->appendChild($class_name);
       
       $school_name = $doc->createElement("school");
       $school_name->nodeValue = $row['school'];
    
       // Append school as child element to class
       $class->appendChild($school_name);
       
       //$period = $doc->createElement("period");
       //$period->nodeValue = $row['period'];
    
        // Append period as child element to class
       //$class->appendChild($period);
       
       $description = $doc->createElement("description");
       $description->nodeValue = $row['description'];
    
       // Append description as child element to class
       $class->appendChild($description);
       
       $instructor = $doc->createElement("instructor");
       $instructor->nodeValue = $row['instructorName'];
    
       // Append instructor as child element to class
       $class->appendChild($instructor);
       $context_msg_el->appendChild($class);
       // Append class as child element to context_message
       // *************
       //
      
       
        

       
       //Creating dataset
       
       $dataset = $doc->createElement("dataset");
       
       $dsName = $doc->createElement("name");
       $dsName->nodeValue = $row['datasetname'];
    
        // Append name as child element to dataset
       $dataset->appendChild($dsName);
       
    
       $dslevel = $doc->createElement("level");
       $dslevel->setAttribute('type', "subject");
       
       $dslevelName = $doc->createElement("name");
       $dslevelName->nodeValue = $row['modulename'];
       
       // Append name as child element to level
       $dslevel->appendChild($dslevelName);
       
       //////
       
       //$dslevel2 = $doc->createElement("level"); 
       //$dslevel2->setAttribute('type', "section");
       //$dslevel2_name = $doc->createElement("name"); 
       //$dslevel2_name->nodeValue =''; //April 8th 2012
       $dslevel2_prob = $doc->createElement("problem"); 
       $dslevel2_prob_name = $doc->createElement("name");
       $dslevel2_prob_name->nodeValue = $row['userProblem'];
       // Append name as child element to problem
       $dslevel2_prob->appendChild($dslevel2_prob_name);
       // Append name as child element to level
       //$dslevel2->appendChild($dslevel2_name);
       // Append problem as child element to level
       $dslevel->appendChild($dslevel2_prob);
       // Append level with module attribute as child element to dataset       
       $dataset->appendChild($dslevel);
        // Append level with section attribute as child element to dataset     
       //$dslevel->appendChild($dslevel2);
        // Append dataset as child element to context_message    
       $context_msg_el->appendChild($dataset);
        //Creating element condition
       $condition = $doc->createElement("condition");
       $condition_name = $doc->createElement("name");
       $condition_name->nodeValue = $row['userSection'];
       $condition->appendChild($condition_name);

       // Two experimental conditions for Summer 2012 class
       if(isset($experimentProblems[$userName][$row['userProblem']])){
	 $condtype = 'experimental';
	 $conddesc ='Improved help policy experiment, experimental condition';   
       } else {
	 $condtype = 'control';
	 $conddesc ='Improved help policy experiment, control condition';   
       }
       //echo "$condtype " . $userName . " " . $row['userProblem'] . "\n";

       $condition_type = $doc->createElement("type");
       $condition_type->nodeValue = $condtype; //April 8th
       $condition->appendChild($condition_type);
       $condition_desc = $doc->createElement("description");
       $condition_desc->nodeValue = $conddesc;
       $condition->appendChild($condition_desc);
       $context_msg_el->appendChild($condition);
       // Append context_message as child element to tutor_related_message_sequence
       $root->appendChild($context_msg_el);
       
       
       //for a specific clientID in the problem attempt table get the corresponding transactions from the step transaction table
       $query="SELECT st.tid, st.client, st.server, pa.clientID, pa.userName, pa.startTime, pa.userProblem, pa.userSection FROM STEP_TRANSACTION st, OPEN_PROBLEM_ATTEMPT pa WHERE st.clientID = pa.clientID and st.clientID = '".$row['clientID']."'";
       foreach ($db->query($query)->fetchAll() as $row2)
       {
           $sat19_action_eval = '';
           $sat19_id = '';
           //Create tool message 
           $tool_msg_el = $doc->createElement("tool_message");
           $tool_msg_el->setAttribute('context_message_id',$row['clientID']);
           
                     
           $json = new Services_JSON();
           // Initialize isValid as True
           $isValid = TRUE;
           $b=$json->decode($row2['client']);
           
           $transaction_id_tool_tutor = "";
           $name_temp_value = "";
           
           if($b != null)
           {
               
               $toolmeta = $doc->createElement("meta");              
                $user_id = $doc->createElement("user_id");
                $user_id->setAttribute('anonFlag', "true");
                $user_id->nodeValue = $userName;    
           // Append user_id as a child to meta
                $toolmeta->appendChild($user_id);
                $sessionID = $doc->createElement("session_id");
           //$user_id->setAttribute('anonFlag', "true");
                 $sessionID->nodeValue = $row['clientID'];
           // Append session_id as a child to meta
                 $toolmeta->appendChild($sessionID);            
                $timerec = $doc->createElement("time");
                //Added on Jan-30-2012
                if(isset ($b->params))
                {
                    $params1 = $b->params;
                    if(isset ($params1->time))
                    {
                        $timetrans = $params1->time;
                        $srttime = strtotime($row['startTime']);
                        $newtime = date('Y-m-d H:i:s', $srttime + $timetrans);
            //$user_id->setAttribute('anonFlag', "true");
                        $timerec->nodeValue = ($newtime);
            // Append time as a child to meta
                        $toolmeta->appendChild($timerec);
                    }
                }
                $timezone = $doc->createElement("time_zone");
            //$user_id->setAttribute('anonFlag', "true");
                $timezone->nodeValue = 'MST';
            // Append timezone as a child to meta
                $toolmeta->appendChild($timezone);     
                  
           // Append meta as a child to tool_message
                $tool_msg_el->appendChild($toolmeta);
               //create semantic_event if client in steptransaction is not null
                
                $semantic_evnt_trans = $doc->createElement("semantic_event");
                //if client has a field id, set attribute of semantic_event as id
                if(isset ($b->id))
                {

                     $semantic_evnt_trans->setAttribute('transaction_id', $b->id);
                     //initialize transaction_id_tool_tutor to id of client
                     $transaction_id_tool_tutor = $b->id;
                     $name_temp_value = "";
                     $method_val = (string)$b->method;
                     //if method field in client has one of the values than set name_temp_value accordingly
                     switch ($method_val)
                     {
                         case "solution-step":
                             $name_temp_value = "ATTEMPT";
                             break;
                         case "seek-help":
                             $name_temp_value = "HINT REQUEST";
                             break;
                         case "suggest-word":
                             $name_temp_value = "SUGGEST WORD";
                             break;
                         case "record-action":
                             $name_temp_value = "RECORD ACTION";
                             break;
                         case "open-problem":
                             $name_temp_value = "OPEN PROBLEM";
                             break;
                         case "close-problem":
                             $name_temp_value = "CLOSE PROBLEM";
                             break;
                         default :
                             $isValid = FALSE;
                             break;
                       }
                
                }
                //if there is no id field in client set isValid to FALSE
                else
                {
                    $isValid = FALSE;
                }
                // if there is id field in client
                if($isValid)
                {
                    //Set the attribute of sematic_event name correspondingly
                    $semantic_evnt_trans->setAttribute('name', $name_temp_value);
                    //Append semantic event as child to tool_message
                    $tool_msg_el->appendChild($semantic_evnt_trans);
                    //Create element event_descriptor
                    $event_descriptor = $doc->createElement("event_descriptor");
                    //Create element selection
                    $event_child_selection = $doc->createElement("selection");
                     //Create element action
                    $event_child_action = $doc->createElement("action");
                     //Create element input
                    $event_child_input = $doc->createElement("input");
                    
                    //if client has a field called params
                    if(isset($b->params))
                    {
                        
                        $parms = $b->params;
                        //if params has field called id, input that into selection and make it a child to event_descriptor
                        if(isset($parms->id))
                        {
                            $event_child_selection->nodeValue = $parms->id;
                            
                            $event_descriptor->appendChild($event_child_selection);
                        }
                        //if params has field called action, input that into action and make it a child to event_descriptor
                        if(isset($parms->action))
                        {
                            $event_child_action->nodeValue = $parms->action;
    
                            $event_descriptor->appendChild($event_child_action);
                        }
                        //if params has field called text, input that into input and make it a child to event_descriptor
                        if(isset($parms->text))
                        {
                            $input_text = $parms->text;
                            if ($input_text != '')
                            {
                                //$input_text = str_replace("&nbsp", "", $input_text);
                                //$input_text = str_replace("&", "&amp;", $input_text);
                                //$input_text = str_replace("<", "", $input_text);
                                
                                $ct = $doc->createCDATASection($input_text); //April 16th
                                $event_child_input-> appendChild($ct);
                                $event_descriptor->appendChild($event_child_input);
                            }
                        }
                        
                        
                    }
                  // Added on Jan 19 to remove warnings due to event_descriptor
                  if($method_val != "record-action" && $method_val != "open-problem" && $method_val != "close-problem" )
                    {
                    //Append event_descriptor as child to tool_message
                      if($method_val == "suggest-word" && $input_text != '' || $method_val == "solution-step" || $method_val == "seek-help")
                        {
                            $tool_msg_el->appendChild($event_descriptor);
                        }                       
                    //Append tool_message as a child to root
                    }
                    $root->appendChild($tool_msg_el);
                    
                }
                
           }
           else
           {
               $isValid = FALSE;
           }
           if($isValid)
           {
                 $server = $json->decode($row2['server']); // Added on 18 Jan                
                 if(isset($server->result)) // Added on 18 Jan
                 {
//create tutor_message
                $tutor_msg_el = $doc->createElement("tutor_message");
                $tutor_msg_el->setAttribute('context_message_id', $row['clientID']);
                //create meta element
                $tutormeta = $doc->createElement("meta");
                //$tutormeta = $doc->createElement("meta");
    
                $user_id = $doc->createElement("user_id");
                $user_id->setAttribute('anonFlag', "true");
                $user_id->nodeValue = $userName;
             
                // Append user id into meta
                $tutormeta->appendChild($user_id);
                $sessionID = $doc->createElement("session_id");
                //$user_id->setAttribute('anonFlag', "true");
                $sessionID->nodeValue = $row['clientID'];
                $tutormeta->appendChild($sessionID);
                
                $timerec = $doc->createElement("time");
                //$user_id->setAttribute('anonFlag', "true");
                $timerec->nodeValue = ($newtime);
                $tutormeta->appendChild($timerec);
                
                $timezone = $doc->createElement("time_zone");
                //$user_id->setAttribute('anonFlag', "true");
                $timezone->nodeValue = 'MST';
                $tutormeta->appendChild($timezone);
                //Append meta as a child to tutor_message
                $tutor_msg_el->appendChild($tutormeta);
                //<semantic_event transaction_id="6" name="HINT_MSG" subtype="Ask-Sought" /> 
                
                $tutor_semantic_event = $doc->createElement("semantic_event");
                //The attribute value transaction_id of semantic event is set to value of attribute transaction_id_tool_tutor
                $tutor_semantic_event->setAttribute("transaction_id", $transaction_id_tool_tutor);
                $tutor_semantic_name = "";
                //Set the type of event in tutor corresponding to the type of event in tool
                switch ($name_temp_value)
                {
                    
                    case "ATTEMPT":
                        $tutor_semantic_name = "RESULT";                        
                        break;
                    case "HINT REQUEST":
                        $tutor_semantic_name = "HINT MESSAGE";
                        break;
                    case "RECORD ACTION":
                        $tutor_semantic_name = "FEEDBACK";
                         break;
                    case "OPEN PROBLEM":
                        $tutor_semantic_name = "NEW PROBLEM"; // Added on 18 Jan
                         break;
                     case "CLOSE PROBLEM":
                        $tutor_semantic_name = "END PROBLEM"; // Added on 18 Jan
                          break;
                     case "SUGGEST WORD":
                        $tutor_semantic_name = "NEXT WORDS"; // Added on 18 Jan
                        break;
                    default :
                        $isValid = FALSE;
                        break;
                        
                }
                if($isValid)
                {
                    $tutor_semantic_event->setAttribute("name",$tutor_semantic_name);
                    //Append semantic_event as a child to tutor_message
                    $tutor_msg_el->appendChild($tutor_semantic_event);
                    
                    //continue - Populate tutor message
                    $server = $json->decode($row2['server']);
                    //If server has a field called result start to iterate
                                        
                    //If the type of event is Result and server has a field called result
                    if($tutor_semantic_name == 'RESULT')
                    {
                        if(isset($server->result))
                        {
                            
                            $error_type = "error-type";
                            $error_type_val = "";
                            foreach($server->result as $resultval1)
                            {
                               if(isset($resultval1->$error_type))
                               {
                                   $error_type_val = $resultval1->$error_type; //April 1st                           
                               }
                               if(isset($resultval1->mode ) )
                               {
                                   //create element action_evalution if there is a mode field in result
                                   $action_evaluation = $doc->createElement("action_evaluation");
                                   if($error_type_val != "")
                                   {
                                      $error_type_val = str_replace("(", "", $error_type_val);
                                      $error_type_val = str_replace(")", "", $error_type_val);
                                      $error_type_val = explode(" ",$error_type_val);//April 1st
                                      $action_evaluation->setAttribute("classification", $error_type_val[0]); //April 1st
                                   }
                                   $action_evaluation->nodeValue = $resultval1->mode;
                                   //Set the variables sat19_id and sat19_action_eval to the id and mode fields of result in server
                                   $sat19_action_eval = $resultval1->mode;
                                   $sat19_id = $resultval1->id;                                                               
                                   
                                   //Append action_evaluation as a child to tutor message
                                   $tutor_msg_el->appendChild($action_evaluation);
                               }
                            }                        
                            
                        }
                    }
                    //If the type of event is HINT MESSAGE 
                    elseif($tutor_semantic_name == 'HINT MESSAGE')
                    {
                        $hint_id = "";
                        $apr16_assoc_set = FALSE;
                        
                        if(isset($server->result))
                        {
                            //Iterate through the whole result field to get hint_id attribute
                            foreach($server->result as $resultval2)
                            {
                               if(isset( $resultval2->assoc ) )
                               { 
                                   $apr16_assoc_set = TRUE;
                                   $hint_scan = $json->encode($resultval2->assoc);
                                   $hint_scan = str_replace("{", "", $hint_scan); //April 1st
                                    $hint_scan = str_replace("}", "", $hint_scan);
                                    $hint_scan = str_replace('"', "", $hint_scan);
                                    //$hint_scan = str_replace("(", "", $hint_scan);
                                   $hint_temp = explode(":",$hint_scan);
                                   $hint_classif = $hint_temp[0];
                                   if($hint_classif == "IEA")
                                   {
                                       $hint_temp[1] = str_replace("(", "", $hint_temp[1]);
                                       $hint_id = $hint_temp[1];
                                   }
                                   elseif($hint_classif == "OPHINT")
                                   {
                                       $hint_id_array = explode(" ", substr($hint_temp[1], 1 + strpos($hint_temp[1], "(", 1)));
                                       $hint_id = $hint_id_array[0];
                                   }
                                   elseif($hint_classif == "NSH"||$hint_classif=="DEFAULT-WRONG-DIR"||$hint_classif=="EQUATION-SYNTAX-ERROR"||$hint_classif=="GOAL"||$hint_classif=="HANDLE-LINK"||$hint_classif=="HANDLE-TEXT"||$hint_classif=="INAPPROPRIATE-TOOL"||$hint_classif=="NO-LABEL")
                                   {
                                    $hint_temp[1] = str_replace("(", "", $hint_temp[1]);
                                    $hint_id_array = explode(" ",$hint_temp[1]);
                                    $hint_id = $hint_id_array[0]; //April 1st
                                   }
                                   else
                                   {
                                       $hint_id = "";
                                   }
                                   //April 1st
                               }
                            }
                        }
                        
                        
                        //Create action_evaluation element
                        $action_evaluation = $doc->createElement("action_evaluation");
                        $action_evaluation->nodeValue = "HINT";
                        if($hint_classif != "")
                        {
                            $action_evaluation->setAttribute("classification",$hint_classif);
                            if($hint_id != "")
                            {
                            $action_evaluation->setAttribute("hint_id",$hint_id);    
                            }
                        }
                        //Append action_evaluation as a child to tutor message
                        $tutor_msg_el->appendChild($action_evaluation);
                    }
                    if(isset($server->result))
                    {
                        
                        foreach($server->result as $resultval)
                            
                        {
                            //$rmode = $resultval->mode;
                            if(($tutor_semantic_name != "NEW PROBLEM") && isset ($resultval->text)) // Added on 18 Jan
                            {
                                //Create tutor_advice
                                $text_val = $resultval->text;
                                //$text_val = str_replace("&nbsp", "", $text_val);
                                //$text_val = str_replace("&", "&amp;", $text_val);
                                //$text_val = str_replace("&lt;.","&amp;lt;", $text_val);
                                 $tutAdvice = $doc->createElement("tutor_advice");
                                 $ct1 = $doc->createCDATASection($text_val); //April 16th
                                $tutAdvice-> appendChild($ct1);
                                 
                                 //Append tutor_advice as child to tool_message
                                 $tutor_msg_el->appendChild($tutAdvice);
                            }
                            
                        }                        
                        
                    }
                    //If server has field called result
                     if($tutor_semantic_name == 'HINT MESSAGE')
                     {
                         if(!$apr16_assoc_set)
                           ////April 16
                        {
			  $query="SELECT server FROM STEP_TRANSACTION WHERE tid > " . $row2['tid'] . " and server like '%\"mode\":\"correct\"%' and clientID = '" . $row2['clientID'] . "'";
			  foreach($db->query($query) as $apr16)
                                    {
                                       if(!$apr16_assoc_set)
                                       {
                                        $apr16_temp=$json->decode($apr16['server']);
                                        if(isset($apr16_temp->result))
                                        {
                                            foreach($apr16_temp->result as $resultval_apr)
                                            {
                                                if(isset($resultval_apr->assoc))
                                                {
                                                    $apr16_assoc_set = TRUE;
                                                    $assoc_val16 = $json->encode($resultval_apr->assoc);
                                                    $assoc_val16 = str_replace("{", "", $assoc_val16);
                                                    $assoc_val16 = str_replace("}", "", $assoc_val16);
                                                    $assoc_val16 = str_replace('"', "", $assoc_val16);
                                                    $temp_array16 = explode(",", $assoc_val16);
                                                    foreach($temp_array16 as $skill_16)
                                                    {
                                                        $skill_16_temp = explode(":",$skill_16);
                                                        $skill_16_el = $doc->createElement("skill");
                                                        $skill_16_name_el = $doc->createElement("name");                                    

                                                        $skill_16_name_el->nodeValue = $skill_16_temp[0];
                                                        $skill_16_model = $doc->createelement("model_name");
                                                        $skill_16_model->nodeValue = "Imputed_Time";
                                                        $skill_16_el->appendChild($skill_16_name_el);
                                                        $skill_16_el->appendChild($skill_16_model);
                                                        $tutor_msg_el->appendChild($skill_16_el);

                                                    }
                                                }
                                            }
                                        }
                                    }
                                  }
                            
                        }
                         }//April 16
                        
                    if(isset($server->result))
                    {
                        //Create variable sat19_assoc_set and set it to FALSE
                        $sat19_assoc_set = FALSE;
                        foreach($server->result as $resultval4)
                        {
                            if(isset($resultval4->assoc))
                            {
                                //If there is an assoc field in result than set sat19_assoc_set to TRUE
                                $sat19_assoc_set = TRUE;
                                $assoc_val = $json->encode($resultval4->assoc);
                                $assoc_val = str_replace("{", "", $assoc_val);
                                $assoc_val = str_replace("}", "", $assoc_val);
                                $assoc_val = str_replace('"', "", $assoc_val);
                                $temp_array = explode(",", $assoc_val);
                                foreach($temp_array as $skill)
                                {
                                    $skill_temp = explode(":",$skill);
                                    //Create element skill
                                    $skill_el = $doc->createElement("skill");
                                    //Create element name
                                    $skill_name_el = $doc->createElement("name");                                    
                                    
                                    //Case where assoc has an OPHINT
                                    if($skill_temp[0] == "OPHINT")
                                    {
                                        $temp_skill_catName = explode(" ", substr($skill_temp[1], 1 + strpos($skill_temp[1], "(", 1)));
                                        $skill_name_el->nodeValue = $temp_skill_catName[0];
                                        $skill_el->appendChild($skill_name_el);
                                        $tutor_msg_el->appendChild($skill_el);
                                    }
                                    else
                                    {
                                        $skill_name_el->nodeValue = $skill_temp[0];
                                       // $skill__cat_el = $doc->createElement("category"); //ON MAR 20
                                        //$skill__cat_el->nodeValue = $skill_temp[1];
                                        $skill_el->appendChild($skill_name_el);
                                       // $skill_el->appendChild($skill__cat_el);
                                        //Append skill to tutor_message
                                        $tutor_msg_el->appendChild($skill_el);
                                    }
                                }
                            }
                        }
                        //If assoc value is not seen for an incorrect step
                        if(!$sat19_assoc_set)
                        {
                            if($sat19_action_eval == 'incorrect')
                            { 
                                 ////Addition on Dec 25 - To incorporate slips(Heuristic-ErrorTypes}
                              //if($error_type_val == '')
                              //{
                                // {Addition for Heuristic-1} if(rows>1)- elseif(rows==1)
			      $query="SELECT count(*) FROM STEP_TRANSACTION WHERE server like '%" . $sat19_id . "%' and clientID = '" . $row2['clientID'] . "'";
                                $rows = $db->query($query)->fetch();
                                //Addition for Heurisitc- 2 $res has  tid > ".$row2['tid']." if(rows>0) - elseif(rows==0)
                                
                                if($rows && $rows[0] > 1)
                                {  
                                  //scroll down look for ID and mode = correct, get assoc value                                   
				  $query="SELECT server FROM STEP_TRANSACTION WHERE tid > " . $row2['tid'] . " and server like '%" . $sat19_id . "%' AND server like '%\"mode\":\"correct\"%' and clientID = '" . $row2['clientID'] . "'";
				  foreach($db->query($query) as $sat19)
                                {
                                    if(!$sat19_assoc_set)
                                    {
                                    $sat19_temp=$json->decode($sat19['server']);
                                    if(isset($sat19_temp->result))
                                    {
                                        foreach($sat19_temp->result as $resultval_sat)
                                        {
                                            if(isset($resultval_sat->assoc))
                                            {
                                                $sat19_assoc_set = TRUE;
                                                $assoc_val = $json->encode($resultval_sat->assoc);
                                                $assoc_val = str_replace("{", "", $assoc_val);
                                                $assoc_val = str_replace("}", "", $assoc_val);
                                                $assoc_val = str_replace('"', "", $assoc_val);
                                                $temp_array = explode(",", $assoc_val);
                                                foreach($temp_array as $skill)
                                                {
                                                    $skill_temp = explode(":",$skill);
                                                    $skill_el = $doc->createElement("skill");
                                                    $skill_name_el = $doc->createElement("name");                                    

                                                    $skill_name_el->nodeValue = $skill_temp[0];
                                                    //$skill__cat_el = $doc->createElement("category");
                                                    //$skill__cat_el->nodeValue = $skill_temp[1];
                                                    $skill_model = $doc->createelement("model_name");
                                                    $skill_model->nodeValue = "Imputed_Location";
                                                    $skill_el->appendChild($skill_name_el);
                                                    //$skill_el->appendChild($skill__cat_el);
                                                    $skill_el->appendChild($skill_model);
                                                    $tutor_msg_el->appendChild($skill_el);
                                                    
                                                }
                                            }
                                        }
                                    }
                                }
                                }
                            }
                                // Delete elseif and corresponding if for Heurisitic-3
                                elseif($rows && $rows[0]== 1)
                                {
                                   
                                if(!$sat19_assoc_set)
                                {
				  $query="SELECT server FROM STEP_TRANSACTION WHERE tid > ".$row2['tid']." and server like '%\"mode\":\"correct\"%' and clientID = '".$row2['clientID']."'";
				  foreach($db->query($query) as $sat20)
                                    {
                                       if(!$sat19_assoc_set)
                                       {
                                        $sat20_temp=$json->decode($sat20['server']);
                                        if(isset($sat20_temp->result))
                                        {
                                            foreach($sat20_temp->result as $resultval_sat)
                                            {
                                                if(isset($resultval_sat->assoc))
                                                {
                                                    $sat19_assoc_set = TRUE;
                                                    $assoc_val = $json->encode($resultval_sat->assoc);
                                                    $assoc_val = str_replace("{", "", $assoc_val);
                                                    $assoc_val = str_replace("}", "", $assoc_val);
                                                    $assoc_val = str_replace('"', "", $assoc_val);
                                                    $temp_array = explode(",", $assoc_val);
                                                    foreach($temp_array as $skill)
                                                    {
                                                        $skill_temp = explode(":",$skill);
                                                        $skill_el = $doc->createElement("skill");
                                                        $skill_name_el = $doc->createElement("name");                                    

                                                        $skill_name_el->nodeValue = $skill_temp[0];
                                                        //$skill__cat_el = $doc->createElement("category");
                                                       // $skill__cat_el->nodeValue = $skill_temp[1];
                                                        $skill_model = $doc->createelement("model_name");
                                                        $skill_model->nodeValue = "Imputed_Time";
                                                        $skill_el->appendChild($skill_name_el);
                                                        //$skill_el->appendChild($skill__cat_el);
                                                        $skill_el->appendChild($skill_model);
                                                        $tutor_msg_el->appendChild($skill_el);

                                                    }
                                                }
                                            }
                                        }
                                    }
                                  } //Here
                                }
                                }
                                
                           
                            }
                        
                        }
                        
                        foreach($server->result as $resultval3)
                        {
                            
                            if(($tutor_semantic_name != "NEW PROBLEM") && isset($resultval3->parse))
                            {
                                    $custom_field = $doc->createElement("custom_field");
                                    $custom_field_name = $doc->createElement("name");
                                    $custom_field_value = $doc->createElement("value");
                                    
                                    $custom_field_name->nodeValue = "parse";
                                    $custom_field_value->nodeValue = $resultval3->parse;
                                    $custom_field->appendChild($custom_field_name);
                                    $custom_field->appendChild($custom_field_value);
                                    $tutor_msg_el->appendChild($custom_field);
                            }
                            if(($tutor_semantic_name != "NEW PROBLEM") && isset($resultval3->score))
                            {
                                    $custom_field = $doc->createElement("custom_field");
                                    $custom_field_name = $doc->createElement("name");
                                    $custom_field_value = $doc->createElement("value");
                                    
                                    $custom_field_name->nodeValue = "score";
                                    $custom_field_value->nodeValue = $resultval3->score;
                                    $custom_field->appendChild($custom_field_name);
                                    $custom_field->appendChild($custom_field_value);
                                    $tutor_msg_el->appendChild($custom_field);
                            }
                            
                        }
                        
                    }
                    
                }
                $root->appendChild($tutor_msg_el);
            }
           }



       }
    }
   
  //make the output pretty
  $doc->formatOutput = true;
  
  //echo $doc->saveXML();
  // Saving the generated XML
  $doc->save(str_replace(":","-",$userName) .".xml");
}

?>
