var foo = // this line is here so my IDE parser works with json
{
	"envelope": "JSON-RPC-2.0",
	"transport": "POST",
	"target": "/help",
	"SMDVersion": "2.0",
	"parameters": [
		//jsonRPC ID
		{"name": "time", "type": "number"}
	],
	"returnParameters": [
		{},
		{},
		{}
		{"log"},
		{"name": "time", "type": "number"}
	],
	"services": {
		"open-problem": {
			"parameters": [
				{"name": "user", "type": "string"},
				{"name": "problem", "type": "string"}
			],
			"returns":{
				"properties":{
					"score":{
						"type": ["object","number"],
						"optional": false
					},
					"items":{
						"type":"array",
						"items":[
							{
								"type":"object",
								"properties":{
									"id":{
										"type": "string",
										"optional": false,
										"description": "Identifier for each drawn item, set by the creator of the object."
									},
									"type":{
										"type": "string",
										"enum": ["text", "graphics", "equation", "circle", "rectangle", "axes", "vector", "line"],
										"optional": false,
										"description": "kind of drawn object"
									},
									"mode":{
										"type": "string",
										"enum": ["unknown","right","wrong","locked","fade"],
										"optional": true,
										"description": "State of the item; unknown: black; correct: green; " +
														"wrong: red; locked: black(not user selectable); "+
														"fade: gray (not user selectable)"
									},
									"x":		{"type": "number"},
									"y":		{"type": "number"},
									"width":	{"type": "number",  "optional": true}, 
									"height":	{"type": "number",	"optional": true},
									"text":		{"type": "string", 	"optional": true},
									"radius":	{"type": "number", 	"optional": true},
									"symbol":	{"type": "string", 	"optional": true},
									"x-label":	{"type": "string", 	"optional": true},
									"y-label":	{"type": "string", 	"optional": true},
									"angle":	{"type": "number", 	"optional": true}
								}
							}
						]
					}
				}
			}
		},
		
		
		
		"solution-step": {
			"parameters": [
				{
					"name": "action",
					"type": "string",
					"optional": false,
					"enum":["new-object","modify-object", "delete-object"]
				},
				{
					"name": "id",
					"type": "string",
					"optional": false,
					"description": "Identifier for each drawn item, set by the creator of the object. " +
									"Used only by actions new-object, modify-object, and delete-object"
				},
				{
					"name": "type",
					"type": "string",
					"enum": ["text", "graphics", "equation", "circle", "rectangle", "axes", "vector", "line"],
					"optional": true,
					"description": "kind of drawn object; manditory for new-object and optional for modify-object or delete-object"
				},
				{
					"name": "mode",
					"type": "string",
					"enum": ["unknown","right","wrong","locked","fade"],
					"optional": true,
					"description": "manditory for new-object and optional for modify-object or delete-object "+
									"unknown: black; correct: green; wrong: red; locked:  black (not user selectable) "+
									"fade:  gray (not user selectable)"
				},
				{"name": "x", 		"type": "number"},
				{"name": "y",	 	"type": "number"},
				{"name": "width", 	"type": "number", 	"optional": true}, 
				{"name": "height", 	"type": "number",	"optional": true},
				{"name": "text", 	"type": "string", 	"optional": true},
				{"name": "radius", 	"type": "number", 	"optional": true},
				{"name": "symbol", 	"type": "string", 	"optional": true},
				{"name": "x-label", "type": "string", 	"optional": true},
				{"name": "y-label", "type": "string", 	"optional": true},
				{"name": "angle", 	"type": "number", 	"optional": true}
			],
			
			"returns":{
				"properties":{
					"score":{
						"type": ["object","number"],
						"optional": false
					},
					"items":{
						"type":"array",
						"items":[
							{
								"type":"object",
								"properties":{
									"id":{
										"type": "string",
										"optional": false,
										"description": "Identifier for each drawn item, set by the creator of the object."
									},
									// I THINK WE COULD USE AN ADDITIONAL MODE: DELETED
									"mode":{
										"type": "string",
										"enum": ["unknown","right","wrong","locked","fade"],
										"optional": true,
										"description": "State of the item; unknown: black; correct: green; " +
														"wrong: red; locked: black(not user selectable); fade: gray (not user selectable)"
									}
								}
							}
						]
					}
				}
			}
		},
		
		
		"seek-help": {
			//
			// may mod score
			//
			"parameters": [
				{
					"name": "action",
					"type": "string",
					"enum":["get-help","help-button","principles-menu"],
					"description": 	"The choice get-help is only from the client while the choices " +
									"set-score, show-hint, show-hint-link, and focus-hint-text-box " +
									"are only from the server. Log actions are not read by the client " +
									"and my be stripped from any response sent to the client. This is " +
									"just a first attempt at the client-server API, any suggestions are welcome."
				},
				{"name": "href", "type": "string", "optional": true},
				{"name": "value", "type": "string", "optional": true},
				{"name": "text", "type": "string", "optional": true}
			],
			"returns":{
				"properties":{
					// NOTE - DON'T KNOW IF THIS IS RIGHT SINCE I DON'T KNOW WHAT 'HELP' RETURNS
					"name":"content",
					"type":"string",
					"enum":[
						"help-button",
						"set-score",
						"show-hint",
						"show-hint-link",
						"focus-hint-text-box"
					]
				}
			}
		},
		
		"submit-problem": {
			"parameters": [
				{
					"name": "projectId",
					"type": "string",
					"description":	"Called when user explicitly hits the Submit button."
				}
			],
			"returns":{
				"name":"action",
				"type": ["boolean","string"],
				"description":	"Either returns an URL for where to navigate next, " + 
								"or returns 'true' (or 'close') if the window is to be closed."
			}
		},
		
		"close-problem": {
			"parameters": [
				{
					"name": "json smd bug: can't just inherit parameters and get named parameters",
					"type": "string",
					"description":	"Called when user explicitly closes problem, like from menu-close. (For SOW2 or Future)"
				}
			],
			"returns":{
				"name":"action",
				"type": "boolean",
				"description":	"Confirmation of problem closed. App or user may now open another problem."
			
			}
		}
	},
	
	
	
	
	
	
	// LEFT FOR REFERENCE - WILL BE REMOVED:
	"returns": {
		"type": "array",
		"items": {
			"type": "object",
			"properties": {
				"action": {
					"type": "string",
					"description": "",
					"enum":[
							"new-object",
							"modify-object",
							"delete-object",
							"log",
							"get-help",
							"help-button",
							"set-score",
							"show-hint",
							"show-hint-link",
							"focus-hint-text-box"
					]
				},
			"id": {
				"description": "Identifier for each drawn item, set by the creator of the object.  Used only by actions new-object, modify-object, and delete-object",
				"type": "string",  //CHANGED - was number
				"optional": true
			},
			"type": {
				"description": "kind of drawn object; manditory for new-object and optional for modify-object or delete-object",
				"type": "string",
				"enum": [
					"text", "graphics", "equation", "circle", "rectangle", "axes", "vector", "line"
				],
				"optional": true
			},
			"mode": {
				"description": "",
				"type": "string",
				"enum": ["unknown","right","wrong","locked","fade"],
				"optional": true
			},
			"score": {"type": ["object","number"], "optional": true}
			
			
			// on startup, this needs to return everything from above
			
			
		}
	}
}}