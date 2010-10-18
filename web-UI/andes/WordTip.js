dojo.provide("andes.WordTip");

dojo.declare("andes.WordTip", null, {
    // Summary:
    //      Singleton whose job is to watch the textbox
    //      and connect to the server when there are changes
    //      to show them to the student.
    //stub: null,
    conEdit: null,
    stencil: null,
    constructor: function(){
        this.conEdit = dojo.byId("conEdit");    
        dojo.connect(this.conEdit, "keydown", this, "textMonitor");
        console.log("I've got conedit now", this.conEdit);
    },
    
    add: function(obj){
        this[obj.id] = obj;
    },
    
    textMonitor: function(evt){
        if(evt.keyCode == dojo.keys.SPACE || evt.keyCode == dojo.keys.TAB || evt.keyCode == 188){
	    console.log("this=",this);
            var tx = dojo.trim(this.conEdit.innerHTML);//this.statement.cleanText(conEdit.innerHTML);
            tx = this.removeBreaks(tx);
            var symbol = andes.variablename.parse(tx);
	    console.log("---Text for word-suggest----> ", tx,symbol);
	    this.sendToServer(tx,symbol);
        };
        if(evt.keyCode == dojo.keys.ENTER || evt.keyCode == dojo.keys.ESCAPE){
            dijit.hideTooltip(this.conEdit);
        }
        var cn = dojo.connect(document,"mouseup",this, function(evt){
            dojo.disconnect(cn);
            dijit.hideTooltip(this.conEdit);
        })
    },
    
    removeBreaks: function(txt){
        dojo.forEach(['<br>', '<br/>', '<br />', '\\n', '\\r'], function(br){
            txt = txt.replace(new RegExp(br, 'gi'), " ");
        });
        return txt;
    },
    
    sendToServer: function(text,symbol){
        var andesTypes = {
		"dojox.drawing.stencil.Line":"line",
		"dojox.drawing.stencil.Rect":"rectangle",
		"dojox.drawing.stencil.Ellipse":"ellipse",
		"dojox.drawing.tools.custom.Vector":"vector",
		"dojox.drawing.tools.custom.Axes":"axes",
		"dojox.drawing.tools.custom.Equation":"equation",
		"dojox.drawing.stencil.Image":"graphics",
		"dojox.drawing.tools.TextBlock":"statement"
	};
        
        var current = "statement";
        if(this.drawing){
            var type = this.drawing.currentStencil ? this.drawing.currentStencil.type : dojo.attr(this.conEdit.parentNode, "id");
            current = andesTypes[type];
        };
        //console.log("Current-------------------------", current);
        andes.api.suggestWord({type: current, text: text, symbol:symbol});
    },
    
    processResults: function(results){
        dojo.forEach(results, function(line){
            console.log("starting line ",line);
            if(line.action=="next-words"){
                dijit.hideTooltip(this.conEdit);
                if(line.words.length > 0){
                    var size = Math.min(5, line.words.length);
                    var wrd = "";
                    for(var i=0; i<size; i++){
                        wrd += line.words[i];
                        wrd += size-1 == i ? "" : ", ";
                    };
                    console.log("Successfully retrieved tips: ", line.words.join(), " \nminimized to: ", wrd);
                    dijit.showTooltip(wrd, this.conEdit, "above");   
                };
            };
        },this);
    }
});