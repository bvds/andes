dependencies = {
	stripConsole: "normal",

//Test to see if performance is affected by no layers.
//Dojo is having trouble with andes.profile dependency here
/*
	layers: [
		
		{
			name: "dojo.js",
			dependencies: [
				"andes.profile",
			]
		}
	],
*/
	prefixes: [
		[ "dijit", 		"../dijit" ],
		[ "dojox", 		"../dojox" ],
		[ "andes", 		"../andes"],
		[ "images", 	"../images" ],
		[ "css", 		"../css" ]
	]
}
