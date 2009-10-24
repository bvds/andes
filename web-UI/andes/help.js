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
				case "focus-major-principles":
			                dijit.byId("majorPrinciples").show();
					break;
				case "focus-all-principles":
			                dijit.byId("allPrinciples").show();
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
		//	Echo any input text in the help pane.
		//
		if(value == '!'){
			value = "Ha! A rotten easter egg!"
		}
		if(value.length>0){
		        var hlp = dijit.byId("helpContentPane");
			var c = hlp.attr("content");
			// note:
	                //	setting to the node and not with attr
	                // 	because ContentPane is throwing errors that way
      			hlp.containerNode.innerHTML = c + "\n<p><em>" + value + "</em></p>";
			hlp.domNode.scrollTop = 10000;
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

