dojo.provide("andes.api");
dojo.require("andes.rpc");

(function(){

	var startTime = null,
	    requestInFlight = false,
	    queue = [],
	    tries = 0;

	var MAX_RETRIES = 5,
	    RETRY_TIMEOUT = 2000; // milliseconds

	// AOP-style function replacement that performs before-advice
	// to add to the headers on all XHR requests. See dojox/rpc/Client.js
	(function(){
		andes._originalXhr = dojo.xhr;
		dojo.xhr = function(method,args){
			var headers = args.headers = args.headers || {};
			headers["X-Client-Id"] = andes.projectId;
			headers["Client-Id"] = andes.projectId;
			return andes._originalXhr.apply(dojo,arguments);
		};
	})();

	function prepRequest(req){
		// add common elements to our requests
		var tm = ((new Date()).getTime() - (startTime || (new Date()).getTime()))/1000.0;
		return dojo.mixin({ time:tm }, req || {});
	}

	function sendRequest(req){
		// send an RPC request
		var request = prepRequest(req.params);
		requestInFlight = true;
		andes.rpc[req.method](request).addCallbacks(
			function(result){
				// FIXME: introspect this to look for errors
				req.dfd.callback(result);
				requestInFlight = false;
				nextRequest();
			},
			function(error){
				// FIXME: What other kind of errors come up here? Are we
				//        safe in assuming this is something we can simply
				//        try again?
				requestInFlight = false;
				if(++tries <= MAX_RETRIES){
					setTimeout(function(){
						sendRequest(req);
					}, RETRY_TIMEOUT);
				}else{
					req.dfd.errback(error);
					console.error("TODO: do a dialog here to notify the user that the connection is wrecked.");
				}
			}
		);
	}

	function nextRequest(){
		// process the next request in the queue, if any
		if(!requestInFlight){
			var req = queue.shift();
			if(req){
				tries = 0;
				sendRequest(req);
			}
		}
	}

	function queueRequest(method, params){
		// add a request to the queue for processing when the current
		// pending request (if any) returns
		var dfd = new dojo.Deferred();
		queue.push({dfd:dfd, method:method, params:params});
		if(queue.length == 1){
			nextRequest();
		}
		return dfd;
	}

	andes.api = {
		open: function(params){
			startTime = (new Date()).getTime();
			return queueRequest("open-problem", params);
		},

		step: function(params){
			return queueRequest("solution-step", params);
		},

		help: function(params){
			return queueRequest("seek-help", params);
		},

		close: function(params){
			return queueRequest("close-problem", params);
		}
	};

})();

