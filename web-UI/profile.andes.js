dependencies = {
	stripConsole: "normal",

	layers: [
		
		{
			name: "dojo.js",
			dependencies: [
				"andes.profile",
			]
		}
	],

	prefixes: [
		[ "dijit", 		"../dijit" ],
		[ "dojox", 		"../dojox" ],
		[ "andes", 		"../andes"],
		[ "images", 	"../images" ],
		[ "css", 		"../css" ]
	]
}
