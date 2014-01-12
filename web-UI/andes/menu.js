/*global define*/
define([
    "dojo/dom",
    "dijit/registry",
    "dojo/ready",
    "dojo/_base/connect",  // This needs to be replaced by dojo/on or dojo/aspect
    "dijit/MenuItem",
    // pre-AMD requires:
    "andes/options",
    "dijit/Menu",
    "dijit/MenuSeparator"
],function(dom, registry, ready, connect, MenuItem, options, Menu, MenuSeparator){  
	// In the pre-AMD version, the body was wrapped
        // in "dojo.addOnLoad(function(){ ... })
        // Should change to "dojo/domReady!" above
    ready(function(){
        // Add problem name to menu
	dom.byId("problemName").innerHTML = window.andes.projectId;
	
	// shortcut for adding an onClick handler to a dijit
	function wireItem(item, fn){
		var o = registry.byId(item);
		if(o){
		    console.log("wiring up menu item ",item," to ",o);
			// Wrapper function which adds logging to server
			// when menu item is selected.
		    connect.connect(o,"onClick",function(){
			window.andes.api.recordAction({
			    type: "menu-choice",
			    name: item
			});
			fn();
		    });
		} else {
			console.warn("Missing DOM object for ",item);
		}
	}
	
	// wire up our menu items
	// Second arg of andes.principles.review(...) should match keyword :title
	// in calls to open-review-window-html.
	var spec = {
		"menuPrinciples":function(){
			window.andes.principles.review('principles-tree.html','Principles');
		},

		"menuQuantities":function(){
			window.andes.principles.review('quantities.html','Quantities');
		},
		
		"menuUnits":function(){
			window.andes.principles.review('units.html','Units');
		},
		
		"menuConstants":function(){
			window.andes.principles.review('constants.html','Constants');
		},
		
		"menuIntroText":function(){
			window.andes.principles.review('introduction.html','IntroText');
		},

		"menuIntroVideo":function(){
			// add 10px padding.
			// should match call in drawing.js
			window.andes.principles.review('vec1a-video.html','IntroVideo',null,"width=650,height=395");
		},		
		
	        "menuIntroSlides":function(){
			window.andes.principles.review('try11/andes.intro.try11_controller.swf',
						'IntroSlides',null,"width=640,height=385");
		},
		
		"menuManual":function(){
			window.andes.principles.review('manual.html','Manual');
		},
		
		"menuOptions":function(){
			// This options is the dijit dialog,
			// the options class controls its contents
			var options = registry.byId("options");
			options.show();
			//options.focus();
		}
		
	};
	
	// Setup contextMenu and children
	window.andes.contextMenu = new Menu();
	var contextOptions = {};
	for(var i in spec){
		wireItem(i, spec[i]);
		contextItem(i, spec[i]);
	}
	
        function contextItem(desc, fn){
		var label = registry.byId(desc).get("label");
		// Hack I'll fix later
		if(label=="Options" || label=="Introduction"){
			window.andes.contextMenu.addChild(new MenuSeparator());
		};
		contextOptions[label] = new MenuItem({
			label:label,
			onClick:fn
		});
		window.andes.contextMenu.addChild(contextOptions[label]);
	};

	// Set up option menu and right click menu
	window.andes.options = new options();
	var _drawing = registry.byId("drawing");
        console.assert(_drawing,"Widget \"drawing\" not found.");

	// Setup the menu onScreen
	console.log("menu.js: about to connect to onSurfaceReady");
	var cn = connect.connect(_drawing, "onSurfaceReady", function(){
		connect.disconnect(cn);
		
		var node = null;
	        connect.connect(_drawing.mouse, "onDown", function(evt){
			console.log("menu.js:  mouse onDown event ", evt);
			// Dynamically prepare menu depending on the target
			// if it's a stencil, allow delete
			window.andes.contextMenu.unBindDomNode(node);
			node = evt.id=="canvasNode" ? dom.byId("drawing") : dom.byId(evt.id);
			window.andes.contextMenu.bindDomNode(node);
		});
		
	});

	// Allow for dynamic updating of the context
	function updateContext(){
		console.log("yeps");
	}
    });
});
