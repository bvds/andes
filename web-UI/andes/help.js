dojo.provide("andes.help");
dojo.require("andes.api");

(function(){

	function handleHelp(result){
		// summary:
		//	Handles text returned from server
		//
		var hlp = dijit.byId("helpContentPane");
		dijit.byId("helpPane").open();
		dojo.forEach(result, function(r){
			var c = hlp.attr("content");
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
					hlp.containerNode.innerHTML = c + "\n<p>" + r.text + "</p>";
					break;
				case "focus-hint-text-box":
					dijit.focus(dojo.byId("helpInput"));
					break;
				case "log":
				default:
					// no-op
			}
		});
		
		hlp.domNode.scrollTop = 10000;
	}

	dojo.addOnLoad(function(){
		dojo.connect(dijit.byId("helpSubmit"), "onClick", function(){
			var q = dijit.byId("helpInput").attr("value"),
			    h = q ? {action:"get-help", text:q} : {action:"help-button"};
				
			andes.help.echo(q);
			dijit.byId("helpInput").attr("value", "");
			andes.api.help(h).addCallback(handleHelp);
		});
	});
	
	andes.help.echo = function(value){
		// summary:
		//	Remove text from Input and place it in the help pane.
		//
		if(value == '!'){
			value = "You are a lover of the mystery of life, yet sometimes you long for straightforward answers. Unfortunately, there's no easy way out; you must be satisfied exploring for a solution to your questions, rather than finding exactly what you're looking for today. However, keep in mind that this is not about giving up; it's about letting go."
		}
		value = '<p><em>'+value+'</em></p>';
		var hlp = dijit.byId("helpContentPane");
		var c = hlp.attr("content");;
		c  += value;
		hlp.attr("content", c);
		hlp.domNode.scrollTop = 10000;
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

	andes.help.link = function(href){
		// summary:
		//	Calls api after a link in help pane has been clicked.
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

