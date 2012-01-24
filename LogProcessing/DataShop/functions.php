<?php

function createContextMessage(&$doc){
    
    $root = $doc->getElementsByTagName('tutor_related_message_sequence')->item(0);
    $context_msg_el = $doc->createElement("context_message");
    $context_msg_el->setAttribute('context_message_id', "test id");
    $context_msg_el->setAttribute('name', "test name");
    
    // Creating Meta tag
    $meta = $doc->createElement("meta");
    
    
    $user_id = $doc->createElement("user_id");
    $user_id->setAttribute('anonFlag', "true");
    $user_id->nodeValue = "ANIRUDH";
    
    // Append user id into meta
    $meta->appendChild($user_id);
    
    $context_msg_el->appendChild($meta);
    
    $root->appendChild($context_msg_el);
    //$doc->appendChild($context_msg_el);
    
}
?>
