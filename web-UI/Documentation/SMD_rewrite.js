var foo = // this line is here so my IDE error-checker will work
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
				"type": "object",
				"score": {
					"type": ["object","number"],
					"optional": true
				},
				"items":{
					"type": "array",
					"item": {
						"type": "object",
						"id": {
							"description": "Identifier for each drawn item, set by the creator of the object.",
							"type": "string", 
							"optional": true
						},
						"type": {
							"description": "The kind of drawn object",
							"type": "string",
							"enum": [
								"text", "graphics", "equation", "circle", "rectangle", "axes", "vector", "line"
							],
							"optional": true
						},
						"mode": {
							"description": "The state of the item. unknown: black; correct: green; wrong: red; locked: black, not user selectable; fade: gray, not user selectable",
							"type": "string",
							"enum": ["unknown","right","wrong","locked","fade"],
							"optional": true
						},
						"x": 		{"type": "number",	"optional": true, "description": "The x location of item"},
						"y": 		{"type": "number",	"optional": true, "description": "The y location of item"},
						"width": 	{"type": "integer",	"optional": true, "description": "Width of item"},
						"height": 	{"type": "integer",	"optional": true, "description": "Height of item"},
						"text":		{"type": "string",	"optional": true, "description": "Text content of item"},
						"radius":	{"type": "number",	"optional": true, "description": "Radius of item"},
						"symbol": 	{"type": "string",	"optional": true, "description": "Variable-name of item"},
						"x-label":	{"type": "string",	"optional": true, "description": "The x-axis label of an axes"},
						"y-label":	{"type": "string",	"optional": true, "description": "The x-axis label of an axes"},
						"angle": 	{"type": "number",	"optional": true, "description": "The angle of an item"}
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
					"name":"items",
					"type":"array",
					"parameters": [
						{
							"name":"item",
							"type":"object"
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
							"description": "The state of the item. unknown: black; correct: green; wrong: red; locked: black, not user selectable; fade: gray, not user selectable"
						},
						{"name": "x", 		"type": "number", 	"optional": true},
						{"name": "y", 		"type": "number", 	"optional": true},
						{"name": "width", 	"type": "integer", 	"optional": true}, 
						{"name": "height", 	"type": "integer", 	"optional": true},
						{"name": "text", 	"type": "string", 	"optional": true},
						{"name": "radius", 	"type": "number",	"optional": true},
						{"name": "symbol", 	"type": "string", 	"optional": true},
						{"name": "x-label", "type": "string", 	"optional": true},
						{"name": "y-label", "type": "string", 	"optional": true},
						{"name": "angle", 	"type": "number", 	"optional": true}
					]
				}
			],
			
			"returns":{
				"type": "object",
				"score": {
					"type": ["object","number"],
					"optional": true
				},
				"items":{
					"type":"array",
					"item":{
						type:"array",
						"id": {
							"type": "string"
						},
						"mode": {
							"type": "string",
							"enum": ["unknown","right","wrong","locked","fade"]
						}
					}
				}
			}
		},
		"seek-help": {
			"parameters": [
				{
					"name": "action",
					"type": "string",
					"enum":["get-help","help-button","principles-menu"]
				},
				{"name": "href", 	"type": "string", "optional": true},
				{"name": "value", 	"type": "string", "optional": true},
				{"name": "text", 	"type": "string", "optional": true}
			],
			"returns":{
				"type": "object",
				"helpContent":{
					"type":"string"
				}
				//MAY NEED MORE PARAMS HERE, MULTI CHOICE, HINTS, ETC
			}
		},
		"close-problem": {
			"parameters": [
				{
					"name": "json smd bug: can't just inherit parameters and get named parameters",
					"type": "string",
					"optional": true
				}
			]
		}
	}
}}