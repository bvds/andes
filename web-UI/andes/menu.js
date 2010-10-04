dojo.provide("andes.menu");
dojo.require("andes.options");

dojo.addOnLoad(function(){
	
        // Add problem name to menu
	dojo.byId("problemName").innerHTML = andes.projectId;
	
	// shortcut for adding an onClick handler to a dijit
	function wireItem(item, fn){
		var o = dijit.byId(item);
		if(o){
			o.onClick = fn;
		}
	}
	
	// wire up our menu items
	var spec = {
		"menuPhysics1":function(){
			andes.principles.externP();
		},

		"menuPhysicsQ":function(){
			andes.principles.review('quantities.html','Quantities');
		},
		
		"menuPhysics2":function(){
			andes.principles.review('units.html','Units');
		},
		
		"menuPhysics3":function(){
			andes.principles.review('constants.html','Constants');
		},
		
		"menuIntroductionText":function(){
			andes.principles.review('introduction.html','Intro Text');
		},

		"menuIntroduction":function(){
			// add 10px padding.
			andes.principles.review('vec1a-video.html','Intro Video',"width=650,height=395");
		},		
		
	        "menuSlides":function(){
			andes.principles.review('try11/andes.intro.try11_controller.swf',
						'Slide show',"width=640,height=385");
		},
		
		"menuManual":function(){
			andes.principles.review('manual.html','Manual');
		},
		
		"menuOptions":function(){
			var options = dijit.byId("options");
			options.show();
			//options.focus();
		}
		
	};
	for(var i in spec){
		wireItem(i, spec[i]);
	}
	
	//Instantiate options object to handle menuOptions
	var menuOptionsController = new andes.options();
});
