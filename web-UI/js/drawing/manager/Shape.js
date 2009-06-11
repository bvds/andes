dojo.provide("drawing.manager.Shape");

(function(){
	var surface;
	drawing.manager.Shape = drawing.util.oo.declare(
		function(options){
			surface = options.surface;
		}
		
	);
})();
