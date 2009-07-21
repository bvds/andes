dojo.provide("andes.menu");

dojo.addOnLoad(function(){

	// shortcut for adding an onClick handler to a dijit
	function wireItem(item, fn){
		var o = dijit.byId(item);
		if(o){
			o.onClick = fn;
		}
	}

	// wire up our menu items
	var spec = {
		"menuFile1":function(){
			console.log("Clicked File #1");
		},

		"menuFile2":function(){
			console.log("Clicked File #2");
		},

		"menuEdit1":function(){
			console.log("Clicked Edit #1");
		},

		"menuEdit2":function(){
			console.log("Clicked Edit #2");
		}
	};
	for(var i in spec){
		wireItem(i, spec[i]);
	}

});
