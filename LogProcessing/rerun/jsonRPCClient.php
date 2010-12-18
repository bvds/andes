<?php

class jsonRPCClient {
    /* 
     * Debug state
     * @var boolean
     */
    public $debug;
    public $debugMessage;
    /* 
     * Server URL
     * @var String
     */
    private $uri;
    private $connection;

    /* 
     *  Constructor of class
     *  Takes the connection parameters
     *
     *  @param String $url
     *  @param Boolean $debug
     */
    public function __construct($uri, $debug = false) {
        $this->uri = $uri;
        $this->debug =$debug;
    }

    /*
     * Performs a jsonRPCRequest and gets the results as an array;
     * 
     * @param $method (String) 
     * @param $params (Array)
     * @return Array
     */
    public function message($request,$clientId) {
      
      $this->debug && $this->debugMessage = "\n".'**** Client Request ******'."\n".$request."\n".'**** End of Client Request *****'."\n";
      
      $this->connection = curl_init($this->uri); 
      curl_setopt($this->connection, CURLOPT_USERAGENT, 'jsonRPCClient');
      curl_setopt($this->connection, CURLOPT_HEADER,false);
      curl_setopt($this->connection, CURLOPT_HTTPHEADER, 
		  array('Content-Type: application/json',
			'Content-Length: ' . strlen($request),
			'Client-Id: ' . $clientId));
      curl_setopt($this->connection, CURLOPT_RETURNTRANSFER, true);
      curl_setopt($this->connection, CURLOPT_POST, true);   
      curl_setopt($this->connection, CURLOPT_POSTFIELDS,$request);   
      $response = curl_exec($this->connection);
      curl_close($this->connection);
         
      if($this->debug) {
	echo nl2br($this->debugMessage);
      }
      
      // Don't know if trim is needed.
      return trim($response);
    }

}
?>