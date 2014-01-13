/* global define */
define([
    "andes/startup",
    // In the pre-AMD version, these were all after the body of main.js:
    // separate from rest of andes load.
    // "andes/defaults", 
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
    console.log("andes/main.js:  load andes/drawing.");
    window.andes.drawing.load(); 
    console.log("Finished with andes/main.js");
});
