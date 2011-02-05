dojo.provide("andes.menu");
dojo.require("andes.options");

dojo.addOnLoad(function(){
	
        // Add problem name to menu
	dojo.byId("problemName").innerHTML = andes.projectId;
	
	// shortcut for adding an onClick handler to a dijit
	function wireItem(item, fn){
		var o = dijit.byId(item);
		if(o){
			// Wrapper function which adds logging to server
			// when menu item is selected.
          		var extendfn = function(){
				andes.api.recordAction({
					type: "menu-choice",
					name: item
				});
				fn();
			};			
			o.onClick = extendfn;
		} else {
			console.warn("Missing DOM object for ",item);
		}
	}
	
	// wire up our menu items
	// Second arg of andes.principles.review(...) should match keyword :title
	// in calls to open-review-window-html.
	var spec = {
		"menuPhysics1":function(){
			andes.principles.review('principles-tree.html','Principles');
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
			andes.principles.review('vec1a-video.html','Intro Video',null,"width=650,height=395");
		},		
		
	        "menuSlides":function(){
			andes.principles.review('try11/andes.intro.try11_controller.swf',
						'Slide show',null,"width=640,height=385");
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
