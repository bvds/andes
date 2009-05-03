var foo = // this line is here so my IDE parser works with json
{
	"envelope": "JSON-RPC-2.0",
	"transport": "POST",
	"target": "/help",
	"SMDVersion": "2.0",
	"parameters": [
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
										"optional": true,
										"description": "kind of drawn object"
									},
									"mode":{
										"type": "string",
										"enum": ["unknown","right","wrong","locked","fade"],
										"optional": true,
										"description": "State of the item; unknown: black; correct: green; wrong: red; locked: black(not user selectable); fade: gray (not user selectable)"
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
					"enum":["new-object","modify-object", "delete-object"]
				},
				{
					"name": "id",
					"type": "string",
					"optional": true,
					"description": "Identifier for each drawn item, set by the creator of the object. Used only by actions new-object, modify-object, and delete-object"
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
					"description": "manditory for new-object and optional for modify-object or delete-object\n  unknown:  turn black\n  correct:  turn green\n  wrong:  turn red\n  locked:  black, not user selectable\n  fade:  gray, not user selectable"
				},
				{"name": "x", 		"type": "number", 	"optional": true},
				{"name": "y",	 	"type": "number", 	"optional": true},
				{"name": "width", 	"type": "integer", 	"optional": true}, 
				{"name": "height", 	"type": "integer",	"optional": true},
				{"name": "text", 	"type": "string", 	"optional": true},
				{"name": "radius", 	"type": "number", 	"optional": true},
				{"name": "symbol", 	"type": "string", 	"optional": true},
				{"name": "x-label", "type": "string", 	"optional": true},
				{"name": "y-label", "type": "string", 	"optional": true},
				{"name": "angle", 	"type": "number", 	"optional": true}
			],
			"returns":{
				"parameters": [
					{
						"name":"score",
						"type": ["object","number"],
						"optional": true
					},
					{
						"name":"items",
						"type":"array",
						"items":[
							{
								"name":"item",
								"type":"object"
							},
							{
								"name": "id",
								"type": "string"
							},
							{
								"name": "mode",
								"type": "string",
								"enum": ["unknown","right","wrong","locked","fade"],
								"description": "manditory for new-object and optional for modify-object or delete-object\n  unknown:  turn black\n  correct:  turn green\n  wrong:  turn red\n  locked:  black, not user selectable\n  fade:  gray, not user selectable"
							}
							
						]
					}
				]
			}
		},
		
		
		"seek-help": {
			"parameters": [
				{
					"name": "action",
					"type": "string",
					"enum":["get-help","help-button","principles-menu"]
				},
				{"name": "href", "type": "string", "optional": true},
				{"name": "value", "type": "string", "optional": true},
				{"name": "text", "type": "string", "optional": true}
			],
			"returns":{
				"parameters": [
					{
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
				]
			}
		},
		
		
		"close-problem": {
			"parameters": [
				{
					"name": "json smd bug: can't just inherit parameters and get named parameters",
					"type": "string",
					"optional": true
				}
			],
			"returns":{
				// NOTE - DON'T KNOW IF THIS IS RIGHT - DOES CLOSE RETURN ANYTHING?
				"name":"success",
				"type": "boolean"
			}
		}
	},
	
	
	// LEFT FOR REFERENCE - DISREGARD:
	"returns": {
		"type": "array",
		"items": {
			"type": "object",
			"properties": {
				"action": {
					"type": "string",
					"description": "The choice get-help is only from the  client while\nthe choices set-score, show-hint, show-hint-link, and focus-hint-text-box are only from the server. \nLog actions are not read by the client and my be stripped from any response sent to the client. This is just a first attempt at the client-server API, any suggestions are welcome.",
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
				"description": "manditory for new-object and optional for modify-object or delete-object\n  unknown:  turn black\n  correct:  turn green\n  wrong:  turn red\n  locked:  black, not user selectable\n  fade:  gray, not user selectable",
				"type": "string",
				"enum": ["unknown","right","wrong","locked","fade"],
				"optional": true
			},
			"score": {"type": ["object","number"], "optional": true}
			
			
			// on startup, this needs to return everything from above
			
			
		}
	}
}}