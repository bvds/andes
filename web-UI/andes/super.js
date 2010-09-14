dojo.provide("andes.super");

(function() {
	
	var flashLoaded = false;
	
	dojo.addOnLoad(function(){
	
		loadFlash();
	
	});
		  
	function loadFlash() {
		console.debug("adding listeners...");
		dojox.flash.addLoadedListener(flashReady);
		dojox.flash.setSwf("andes/super/SuperActivity.swf", false);
	};
		
	function flashReady(){
		console.debug("flashReady");
		if (flashLoaded) {
		  return; // prevent double loads
		}
		
		flashLoaded = true;
	};
		  
})();