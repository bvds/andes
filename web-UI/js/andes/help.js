dojo.provide("andes.help");
dojo.require("andes.api");

(function(){

	function handleHelp(result){
		var hlp = dijit.byId("helpContentPane");
		hlp.attr("content", "");
		dojo.forEach(result, function(r){
			var c = hlp.attr("content");
			switch(r.action){
				case "show-hint-link":
					var fn = r.href ? "link" : "explain",
					    val = r.href || r.value;
					hlp.attr("content", c + "\n<p><a href=\"#\" onclick=\"andes.help." + fn + "('" + val + "'); return false\">" + r.text + "</a></p>");
					break;
				case "show-hint":
					hlp.attr("content", c + "\n<p>" + r.text + "</p>");
					break;
				case "focus-hint-text-box":
					dijit.focus(dojo.byId("helpInput"));
					break;
				case "log":
				default:
					// no-op
			}
		});
	}

	dojo.addOnLoad(function(){
		dojo.connect(dijit.byId("helpSubmit"), "onClick", function(){
			var q = dijit.byId("helpInput").attr("value"),
			    h = q ? {action:"get-help", text:q} : {action:"help-button"};
			andes.api.help(h).addCallback(handleHelp);
		});
	});

	andes.help.explain = function(s){
		andes.api.help({action:"get-help", value:s}).addCallback(handleHelp);
	};

	andes.help.link = function(href){
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
		return dijit.byId("helpPane").score(value);
	};
})();

