dojo.provide("andes.drawing");

(function(){
	var drawingId = "drawing";
	var drawing;
	var domLoaded = false;
	var stencils = {
		statement:"text",
		graphics:"image"
	}
	dojo.addOnLoad(function(){
		domLoaded = true;
		drawing = dijit.byId(drawingId);
		console.info("check for surface...")
		var cn = dojo.connect(drawing, "onSurfaceReady", function(){
			console.info("----------------->surafce loaded.")
			dojo.disconnect(cn);
			andes.drawing.onSurfaceReady();
		});
		
	});
	andes.drawing = {
		onSurfaceReady: function(){
			console.info("do smething")
			//drawing.addStencil("image", {data:{src:"tests/images/BallOnWall.png", x:300, y:200, width:"auto"}});
			dojo.forEach(this._initialData, function(obj){
				if(obj.action =="new-object"){
					obj.src = obj.href;
					drawing.addStencil(stencils[obj.type], {data:obj});
				}
			});
		},
		load: function(auth){
			this.user = auth.u;
			this.project = auth.p;
			andes.api.open({user:this.user, problem:this.project}).addCallback(this, "onLoad").addErrback(this, "onError");
		},
		onLoad: function(data){
			console.info("Project Data Loaded");
			this._initialData = data;
			console.dir(data);
		},
		onError: function(err){
			console.error("There was an error loading project data:", err);
		}
	};
	
})();

/*
 var cb = function(){ console.warn("IN CALLBACK"); };
var eb = function(){ console.error("IN ERRBACK"); };

andes.api.open({user:"joe", problem:"s2e"}).addCallbacks(cb,eb);

andes.api.step({action:"new-object", id:"a3", type:"circle", mode:"unknown", x:66, y:188, radius:25, text:"ball"});

andes.api.close();
*/