define([
	"andes/drawing",
    "andes/startup",
	// In the pre-AMD version, these were all after the body of main.js:
//    "dojox/drawing",  // must come after andes/defaults
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
    console.log("andse/main.js:  create new andes.drawing object.");
    var d= new drawing();
    d.load();
  console.info("Finished with andes/main.js");
    return d;
});
