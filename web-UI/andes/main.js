define([
	"andes/drawing",
    "andes/startup",
	// In the pre-AMD version, these were all after the body of main.js:
	"andes/defaults",
	"andes/PreferenceRegistry",
	"andes/convert",
	"andes/menu",
	"andes/help",
	"andes/api",
	"andes/error",
        "andes/variablename",
    // new: pulled out of drawing.js
    "andes/tracking"
],function(drawing){ 
    // Pre-AMD body of main.js moved to startup.js.
    // Pre-AMD version had "andes.drawing.load();" at very end of the file.
    console.log("object returned by andes/drawing: ",drawing);
    d= new drawing();
    d.load();
    return d;
});
