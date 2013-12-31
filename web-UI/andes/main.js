define([
    "andes/drawing",
	// In the pre-AMD version, these were all after the body of main.js:
//    "dojox/drawing",  // must come after andes/defaults
	"andes/PreferenceRegistry",
	"andes/menu",
	"andes/error",
        "andes/variablename",
    // new: pulled out of drawing.js
    "andes/tracking"
],function(drawing){ 
    // Pre-AMD body of main.js moved to startup.js.
    // Pre-AMD version had "andes.drawing.load();" at very end of the file.
    console.log("andes/main.js:  load andes/drawing.");
    drawing.load("drawing"); // id in index.html
  console.info("Finished with andes/main.js");
});
