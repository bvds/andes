define([
	"andes/options",
	"dijit/Menu",
	"dijit/MenuSeparator"
],function(){  
	// In the pre-AMD version, the body was wrapped
        // in "dojo.addOnLoad(function(){ ... })
	
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
		"menuPrinciples":function(){
			andes.principles.review('principles-tree.html','Principles');
		},

		"menuQuantities":function(){
			andes.principles.review('quantities.html','Quantities');
		},
		
		"menuUnits":function(){
			andes.principles.review('units.html','Units');
		},
		
		"menuConstants":function(){
			andes.principles.review('constants.html','Constants');
		},
		
		"menuIntroText":function(){
			andes.principles.review('introduction.html','IntroText');
		},

		"menuIntroVideo":function(){
			// add 10px padding.
			// should match call in drawing.js
			andes.principles.review('vec1a-video.html','IntroVideo',null,"width=650,height=395");
		},		
		
	        "menuIntroSlides":function(){
			andes.principles.review('try11/andes.intro.try11_controller.swf',
						'IntroSlides',null,"width=640,height=385");
		},
		
		"menuManual":function(){
			andes.principles.review('manual.html','Manual');
		},
		
		"menuOptions":function(){
			// This options is the dijit dialog,
			// the options class controls its contents
			var options = dijit.byId("options");
			options.show();
			//options.focus();
		}
		
	};
	
	// Setup contextMenu and children
	andes.contextMenu = new dijit.Menu();
	var contextOptions = {};
	for(var i in spec){
		wireItem(i, spec[i]);
		contextItem(i, spec[i]);
	}
	
	function contextItem(desc, fn){
		var label = dijit.byId(desc).get("label");
		// Hack I'll fix later
		if(label=="Options" || label=="Introduction"){
			andes.contextMenu.addChild(new dijit.MenuSeparator());
		};
		contextOptions[label] = new dijit.MenuItem({
			label:label,
			onClick:fn
		});
		andes.contextMenu.addChild(contextOptions[label]);
	};
	
	// Set up option menu and right click menu
	andes.options = new andes.options();
	var _drawing = dijit.byId("drawing");
	
	// Setup the menu onScreen
	var cn = dojo.connect(_drawing, "onSurfaceReady", function(){
		dojo.disconnect(cn);
		
		var node = null;
		dojo.connect(_drawing.mouse, "onDown", function(evt){
			// console.log("On down evt: ", evt);
			// Dynamically prepare menu depending on the target
			// if it's a stencil, allow delete
			andes.contextMenu.unBindDomNode(node);
			node = evt.id=="canvasNode" ? dojo.byId("drawing") : dojo.byId(evt.id);
			andes.contextMenu.bindDomNode(node);
		});
		
	});
	
	// Allow for dynamic updating of the context
	function updateContext(){
		console.log("yeps");
	}
});
