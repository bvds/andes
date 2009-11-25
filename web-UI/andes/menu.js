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

		"menuIntroVideo":function(){
		  window.open('andes3video.test1.htm','Intro Video');
		},

	  	"menuFAQ":function(){
		andes.principles.review('FAQ.html','How to ...');
		}

	};
	for(var i in spec){
		wireItem(i, spec[i]);
	}

});
