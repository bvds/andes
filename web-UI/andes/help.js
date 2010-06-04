dojo.provide("andes.help");
dojo.require("andes.api");

(function(){
	
	function handleHelp(result){
		// summary:
		//	Handles text returned from server
		//
		if(!dijit.byId("helpPane")){
			setTimeout(function(){
				handleHelp(result);
			}, 500);
			return;
		}
		var hlp = dijit.byId("helpContentPane");
		dijit.byId("helpPane").open();
		dojo.forEach(result, function(r){
			var c = hlp.get("content");
			// note:
			//	setting to the node and not with attr
			// 	because ContentPane is throwing errors that way
			switch(r.action){
			    case "show-hint-link":
				var fn = r.href ? "link" : "explain",
				val = r.href || r.value;
				hlp.containerNode.innerHTML = c + "\n<p><a href=\"#\" onclick=\"andes.help." + fn + "('" + val + "'); return false\">" + r.text + "</a></p>";
				break;
			    case "show-hint":
				// Student math symbols may be echoed in help statements.
				hlp.containerNode.innerHTML = c + "\n<p>" + andes.typeset.convertLaTeX(r.text) + "</p>";
				break;
			    case "echo-get-help-text":
		  		// Escape any html codes on input text echo.
               		        // Should use future function dojo.string.escape
                                // See http://trac.dojotoolkit.org/ticket/8995
				andes.help.echo(r.text.replace(/\&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;"));
				break;
			    case "focus-hint-text-box":
				dijit.focus(dojo.byId("helpInput"));
				break;
			    case "focus-major-principles":
                                dojo.byId("majorModalTreeText").innerHTML = r.text ? "<p class=\"tall\">" + r.text + "</p>\n": "";
			        dijit.byId("majorPrinciples").show();
				break;
			    case "focus-all-principles":
                		dojo.byId("allModalTreeText").innerHTML = "<p class=\"tall\">" + r.text + "</p>\n";
			        dijit.byId("allPrinciples").show();
				break;
			    case "log":
			    default:
				// no-op
			}
		});
		
		hlp.domNode.scrollTop =  hlp.domNode.scrollHeight;
	}
	
	dojo.addOnLoad(function(){
		dojo.connect(dijit.byId("helpSubmit"), "onClick", function(){
			var q = dijit.byId("helpInput").get("value"),
			h = q ? {action:"get-help", text:q} : {action:"help-button"};
			
			// Escape any html codes on input text echo.
		        // Should use future function dojo.string.escape
                        // See http://trac.dojotoolkit.org/ticket/8995
			andes.help.echo(q.replace(/\&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;"));
			dijit.byId("helpInput").set("value", "");
			andes.api.help(h).addCallback(handleHelp);
		});
	});
	
	andes.help.echo = function(value){
		// summary:
		//	Echo any input text in the Tutor pane.
		//
		if(value == '!'){
			value = "Ha! A rotten easter egg!"
		}
		if(value.length>0){
		        var hlp = dijit.byId("helpContentPane");
			var c = hlp.get("content");
			// note:
	                //	setting to the node and not with attr
	                // 	because ContentPane is throwing errors that way
      			hlp.containerNode.innerHTML = c + "\n<p><em>" + value + "</em></p>";
			hlp.domNode.scrollTop = hlp.domNode.scrollHeight;
		}
	};
	
	andes.help.processStep = function(result){
		// summary:
		// look for any help coming back from the server (such as in
		// the results from andes.api.step()
		handleHelp(result);
	};
	
	andes.help.explain = function(s){
		andes.api.help({action:"get-help", value:s}).addCallback(handleHelp);
	};
	
   	andes.help.principles = function(s){
		andes.api.help({action:"principles-menu", value:s}).addCallback(handleHelp);
	};
	
	andes.help.link = function(href){
		// summary:
		//	Calls api after a link in Tutor pane has been clicked.
		dojo.xhrGet({
			url: href,
			handleAs: "text",
			load: function(result){
				// FIXME: This is untested. Should we stuff the content directly into the pane like this?
				dijit.byId("helpContentPane").attr("content", result);
			}
		});
	};
	
	andes.help.score = function(value){
		// summary:
		// updates score
		return dijit.byId("helpPane").score(value);
	};
})();

