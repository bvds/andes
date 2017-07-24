/* global define */
// Pre-AMD version had provides for "andes.error" and "andes.error._Error"
// Pre-AMD version had a function wrapper.
define([
    "dojo/_base/declare",
    "dojo/dom-construct",
    "dojo/dom-style",
    "dojo/keys",
    "dojo/query",
    "dojo/request/xhr",
    // pre-AMD requires:
    "dijit/Dialog",
    "dijit/form/Button",
    "dojo/domReady!"
], function(declare, domConstruct, domStyle, keys, query, xhr, dijitDialog, Button){

    var dialog = null;

    window.andes.errorLog = function(spec){
        xhr.post("client_log.php", {
            data: {
                "Client-Id": window.andes.sessionId,
                tag: spec.title,
                text: spec.message
            }});
    };

    window.andes.error = function(spec){
        var message = spec.message || "An unknown error occurred.",
            title = spec.title || "Error",
            dialogType = spec.dialogType || 0;
        // In pre-AMD version, dialog was set using addOnLoad
        // Instead, we will use dojo/domReady! above
        if(!dialog){
            dialog = new _Error({
                id: "andesErrorDialog",
                title: "Error",
                style: "width:400px"
            });
        }
        console.log("dialog defined");
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
        return dialog.buttonsNode;
    };

    // dialogType constants
    window.andes.error.FATAL = 0;
    window.andes.error.OK = 1;

    // In pre-AMD version, nothing outside this file uses _Error.
    var _Error = declare(dijitDialog, {
        postCreate: function(){
            this.inherited(arguments);
            var container = domConstruct.create("div", {className:"dijitDialogPaneContent", style:"border-top:none;"});

            // andesButtonPageDefault -- just an OK button
            var props = {className:"andesButtonPage", id:"andesButtonPageDefault", style:"display:none;"};
            var page1 = domConstruct.create("div", props, container);
            var btn_OK = new Button({
                label:"OK",
                type:"submit"
            }, domConstruct.create("div", null, page1));

            domConstruct.place(container, this.domNode);
            this.buttonsNode = container;
        },

        _onKey: function(evt){
            if(this.dialogType == window.andes.error.FATAL){
                if(evt.charOrCode == keys.ESC || evt.charOrCode == keys.TAB){
                    evt.preventDefault();
                }
                return;
            }
            this.inherited(arguments);
        },

        show: function(){
            query(".andesButtonPage", this.buttonsNode).style("display", "none");
            var node = this._chooseButtonPageNode();
            if(node){
                domStyle.set(node, "display", "block");
                domStyle.set(this.closeButtonNode, "display", "block");
            }else{
                domStyle.set(this.closeButtonNode, "display", "none");
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
        console.info("andes/error.js:  start logging errors");
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
    };
});
