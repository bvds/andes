define([
    "dojo/_base/connect",  // This needs to be replaced by dojo/on or dojo/aspect
    "dojox/drawing/util/typeset",
    "dojo/ready",
    "dijit/registry",
    "dojo/dom",
    "dijit/focus",
    "dojo/_base/array",
    "dojo",  // for dojo.xhrGet (deprecated)
    // pre-AMD require:
    "andes/api"
],function(connect, typeset, ready, registry, dom, focusUtil, array, dojo){ // Pre-AMD version had a function wrapper.

	function handleHelp(result){
		// summary:
		//	Handles text returned from server
		//
		if(!registry.byId("helpPane")){
		    console.warn("helpPane not found, try again later.");
			window.setTimeout(function(){
				handleHelp(result);
			}, 500);
			return;
		}
	        console.info("helpPane found");
		var hlp = registry.byId("helpContentPane");
		array.forEach(result, function(r){
			var c = hlp.get("content");
			// note:
			//	setting to the node and not with attr
			// 	because ContentPane is throwing errors that way
			// note:
			//      helpPane open() tests if pane is already opened
			//      so multiple calls are OK.
			switch(r.action){
			    case "show-hint-link":
				registry.byId("helpPane").open();
				var fn = r.href ? "link" : "explain",
				val = r.href || r.value;
				hlp.containerNode.innerHTML = c + "\n<p><a href=\"#\" onclick=\"andes.help." + fn + "('" + val + "'); return false\">" + r.text + "</a></p>";
				break;
			    case "show-hint":
				registry.byId("helpPane").open();
				var style = r.style ? " class=\""+ r.style + "\"":"";
				// Student math symbols may be echoed in help statements.
				hlp.containerNode.innerHTML = c + "\n<p" + style +">" + typeset.convertLaTeX(r.text) + "</p>";
				break;
			    case "echo-get-help-text":
				registry.byId("helpPane").open();
		  		// Escape any html codes on input text echo.
               		        // Should use future function dojo.string.escape
                                // See http://trac.dojotoolkit.org/ticket/8995
				window.andes.help.echo(r.text.replace(/\&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;"));
				break;
			    case "focus-hint-text-box":
				registry.byId("helpPane").open();
				focusUtil.focus(dom.byId("helpInput"));
				break;
			    case "focus-major-principles":
                                dom.byId("majorModalTreeText").innerHTML = r.text ? "<p class=\"tall\">" + r.text + "</p>\n": "";
			        registry.byId("majorPrinciples").show();
				break;
			    case "focus-all-principles":
                		dom.byId("allModalTreeText").innerHTML = "<p class=\"tall\">" + r.text + "</p>\n";
			        registry.byId("allPrinciples").show();
				break;
			    case "log":
			    default:
				// no-op
			}
		});
		
		hlp.domNode.scrollTop =  hlp.domNode.scrollHeight;
	}
	
	ready(function(){
		console.info("andes/help.js:  Wire up the help button");
		connect.connect(registry.byId("helpSubmit"), "onClick", function(){
			var q = registry.byId("helpInput").get("value"),
			h = q ? {action:"get-help", text:q} : {action:"help-button"};
			
			// Escape any html codes on input text echo.
		        // Should use future function dojo.string.escape
                        // See http://trac.dojotoolkit.org/ticket/8995
			window.andes.help.echo(q.replace(/\&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;"));
			registry.byId("helpInput").set("value", "");
			window.andes.api.help(h).addCallback(handleHelp);
		});
	});	

	// AMD conversion addition:
	window.andes.help={};

	window.andes.help.echo = function(value){
		// summary:
		//	Echo any input text in the Tutor pane.
		//
		if(value == '!'){
			value = "Ha! A rotten easter egg!";
		}
		if(value.length>0){
		        var hlp = registry.byId("helpContentPane");
			var c = hlp.get("content");
			// note:
	                //	setting to the node and not with attr
	                // 	because ContentPane is throwing errors that way
      			hlp.containerNode.innerHTML = c + "\n<p><span class=\"comment\">" + value + "</span></p>";
			hlp.domNode.scrollTop = hlp.domNode.scrollHeight;
		}
	};
	
	window.andes.help.processStep = function(result){
		// summary:
		// look for any help coming back from the server (such as in
		// the results from andes.api.step()
		handleHelp(result);
	};
	
	window.andes.help.explain = function(s){
		window.andes.api.help({action:"get-help", value:s}).addCallback(handleHelp);
	};
	
   	window.andes.help.principles = function(s){
		window.andes.api.help({action:"principles-menu", value:s}).addCallback(handleHelp);
	};
	
	window.andes.help.link = function(href){
		// summary:
		//	Calls api after a link in Tutor pane has been clicked.
		dojo.xhrGet({
			url: href,
			handleAs: "text",
			load: function(result){
				// FIXME: This is untested. Should we stuff the content directly into the pane like this?
				registry.byId("helpContentPane").attr("content", result);
			}
		});
	};
	
	window.andes.help.score = function(value){
		// summary:
		// updates score
		return registry.byId("helpPane").score(value);
	};

	window.andes.help.link = function(name,value){
		var s={type: "tutor-link",name: name};
		if(value){
			s.value=value; // value is optional
		}
		window.andes.api.recordAction(s);
	};

});
