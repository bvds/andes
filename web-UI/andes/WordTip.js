/* global define */
define([
    "dojo/_base/array",
    "dojo/_base/declare",
    "dojo/_base/connect",  // This needs to be replaced by dojo/on or dojo/aspect
    "dojo/dom",
    "dojo/dom-attr",
    "dojo/keys",
    "dojo/_base/lang",
    "dijit/Tooltip",
    "dojo/domReady!"  // Needed to locate "conEdit"
    // pre-AMD version had no requires
], function(array, declare, connect, dom, domAttr, keys, lang, Tooltip){
    return declare(null,{

        // Summary:
        //      Singleton whose job is to watch the textbox
        //      and connect to the server when there are changes
        //      to show them to the student.

        conEdit: null,
        stencil: null,

        constructor: function(){
            console.info("Constructing WordTip, this=",this);
            this.conEdit = dom.byId("conEdit");
            console.assert(this.conEdit,"conEdit is missing (already removed by drawing?).");
            connect.connect(this.conEdit, "keydown", this, "textMonitor");
            console.log("I've got conedit now", this.conEdit);
        },

        hasTip: {
            "rectangle": true,
            "ellipse": true,
            "vector": true,
            "statement": true
        },

        add: function(obj){
            this.theDrawing = obj;
        },

        textMonitor: function(evt){
            if(evt.keyCode == keys.SPACE || evt.keyCode == keys.TAB || evt.keyCode == 188){
                console.log("andes.WordTip.textMonitor this=",this);
                var tx = lang.trim(this.conEdit.innerHTML);//this.statement.cleanText(conEdit.innerHTML);
                tx = this.removeBreaks(tx);
                var symbol = window.andes.variablename.parse(tx);
                console.log("---Text for word-suggest----> ", tx,symbol);
                this.sendToServer(tx,symbol);
            }
            if(evt.keyCode == keys.ENTER || evt.keyCode == keys.ESCAPE){
                Tooltip.hide(this.conEdit);
            }
            var cn = connect.connect(document,"mouseup",this, function(evt){
                console.log("WordTip.js:  responding to mouseup event");
                connect.disconnect(cn);
                Tooltip.hide(this.conEdit);
            });
        },

        removeBreaks: function(txt){
            array.forEach(['<br>', '<br/>', '<br />', '\\n', '\\r'], function(br){
                txt = txt.replace(new RegExp(br, 'gi'), " ");
            });
            return txt;
        },

        sendToServer: function(text,symbol){
            console.assert(this.theDrawing,"WordTip needs drawing initialized");
            var current;
            var andesTypes = window.andes.convert.andesTypes;

            // The most recent stencil will either be the last selected or the last
            // tool.  Thus find out the id, if it matches the last selected that's
            // it.  If not then it must be the current tool.

            // console.log("stencils......>>",this.theDrawing.stencils.getRecentStencil(), "attr check: ", dojo.attr(this.conEdit.parentNode, "id"));
            var stencilID = domAttr.get(this.conEdit.parentNode, "id"),
            stencilLastSelected = this.theDrawing.stencils.getRecentStencil(),
            type = stencilLastSelected.combo ? stencilLastSelected.combo.master.type : stencilLastSelected.type,
            sid = stencilLastSelected.combo ? stencilLastSelected.combo.statement.id : stencilLastSelected.id;

            if(stencilID!=sid){
                // Current statement or equation
                type = this.theDrawing.currentType;
                current = andesTypes[type];
            }else{
                // Everything else, meaning combo objects created or an item
                // is being selected
                // console.log("Selected: ", this.theDrawing.stencils.stencils[stencilID]);
                var tmp = this.theDrawing.stencils.stencils[stencilID];
                current = tmp.customType || andesTypes[type];
            }
            // console.log("current: ",current);
            if(current && this.hasTip[current]){
                window.andes.api.suggestWord({type: current, text: text, symbol:symbol});
            }
        },

        processResults: function(results){
            // Return may also include log messages and other directives.
            // Here, we ignore any other directives.
            array.forEach(results, function(line){
                if(line.action=="next-words"){
                    Tooltip.hide(this.conEdit);
                    if(line.words.length > 0 || line["last-word"]){
                        var size = Math.min(7, line.words.length);
                        var wrd = line["last-word"] ? "&lt;done&gt;" : "";
                        for(var i=0; i<size; i++){
                            if(wrd.length>0) {wrd += ", ";}
                            wrd += line.words[i];
                        }
                        if(i<line.words.length){
                            wrd += ", &#8230;";
                        }
                        console.log("Successfully retrieved tips: ", line.words.join(), " \nminimized to: ", wrd);
                        // At some point, place.around() started taking a list of positions
                        // instead of a string
                        Tooltip.show(wrd, this.conEdit, ["above"]);   
                    }
                }
            },this);
        }
    });
});
