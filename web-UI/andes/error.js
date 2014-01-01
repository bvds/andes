// Pre-AMD version had provides for "andes.error" and "andes.error._Error"
// Pre-AMD version had a function wrapper.
define([
    "dojo/ready",
    "dojo/_base/declare",
    // pre-AMD requires:
    "dijit/Dialog",
    "dijit/form/Button"
], function(ready,declare,dialog){


	window.andes.errorLog = function(spec){
		dojo.xhrPost({
			url: "client_log.php",
			content: {
				"Client-Id": window.andes.sessionId,
				tag: spec.title,
				text: spec.message
			}});
	};				

	window.andes.error = function(spec){
		var message = spec.message || "An unknown error occurred.",
		    title = spec.title || "Error",
		    dialogType = spec.dialogType || 0;
		dialog.set({
			content: message,
			title: title,
			dialogType: dialogType
		});
		dialog.show();
		if(!spec.noLog){
			window.andes.errorLog({
				title: title,
			        message: message
			});
		}
	};
		
	// dialogType constants
	window.andes.error.FATAL = 0;
	window.andes.error.OK = 1;

       // In pre-AMD version, nothing outside this file uses _Error.
	var _Error = declare(dialog, {
		postCreate: function(){
			this.inherited(arguments);
			var container = dojo.create("div", {className:"dijitDialogPaneContent", style:"border-top:none;"});

			// andesButtonPageDefault -- just an OK button
			var props = {className:"andesButtonPage", id:"andesButtonPageDefault", style:"display:none;"};
			var page1 = dojo.create("div", props, container);
			var btn_OK = new dijit.form.Button({
				label:"OK",
				type:"submit"
			}, dojo.create("div", null, page1));

			dojo.place(container, this.domNode);
			this.buttonsNode = container;
		},

		_onKey: function(evt){
			if(this.dialogType == window.andes.error.FATAL){
				if(evt.charOrCode == dojo.keys.ESC || evt.charOrCode == dojo.keys.TAB){
					dojo.stopEvent(evt);
				}
				return;
			}
			this.inherited(arguments);
		},

		show: function(){
			dojo.query(".andesButtonPage", this.buttonsNode).style("display", "none");
			var node = this._chooseButtonPageNode();
			if(node){
				dojo.style(node, "display", "block");
				dojo.style(this.closeButtonNode, "display", "block");
			}else{
				dojo.style(this.closeButtonNode, "display", "none");
			}
			this.inherited(arguments);
		},

		_chooseButtonPageNode: function(){
			switch(this.dialogType){
				case window.andes.error.FATAL:
					return null; // fatal errors won't have any dialog buttons
					break;
				case window.andes.error.OK:
				default:
					return "andesButtonPageDefault";
			}
			return null;
		}
	});

    if(true){
	ready(function(){
		console.info("andes/error.js:  start logging errors");
		dialog = new _Error({
			id: "andesErrorDialog",
			title: "Error",
			style: "width:400px"
		});

                // This clobbers any existing onerror handler.
		window.onerror = function(msg, url, line){
			window.andes.errorLog({
				title:  "javascript-error",
				message: url + ":" + line + " " + msg
			});
		    console.log("Window error: ",msg,"; url: ",url, "; line: ",line,".");
		    // Row later:  Returning 'false' triggers the execution of the built-in error handler.
		    // return !dojoConfig.isDebug;
		};
	});
    };
});
