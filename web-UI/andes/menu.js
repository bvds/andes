dojo.provide("andes.menu");

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

		"menuPhysics2":function(){
		  andes.principles.review('units.html','Units');
		},

		"menuPhysics3":function(){
		  andes.principles.review('constants.html','Constants');
		},

		"menuIntroduction":function(){
		  andes.principles.review('introduction.html','Introduction');
		},

	        "menuSlides":function(){
		  andes.principles.review('andes3intro/andes3intro.mod2.html',
		  'Slide show');
		},

		"menuManual":function(){
		  andes.principles.review('manual.html','Manual');
		}

	};
	for(var i in spec){
		wireItem(i, spec[i]);
	}

});
