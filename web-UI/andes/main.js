define([
	// In the pre-AMD version, these were all after the body of main.js:
	"andes/defaults",
	"andes/PreferenceRegistry",
	"andes/convert",
	"andes/drawing",
	"andes/menu",
	"andes/help",
	"andes/api",
	"andes/error",
	"andes/variablename"
],function(){ 
    // Pre-AMD body of main.js moved to startup.js.
    // Pre-AMD version had "andes.drawing.load();" at very end of the file.
    andes.drawing.load();
});
