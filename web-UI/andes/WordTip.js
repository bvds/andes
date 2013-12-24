define([
    'dojo/_base/declare',
    'andes/startup'
], function(declare,andes){
	return declare(null, {

    // Summary:
    //      Singleton whose job is to watch the textbox
    //      and connect to the server when there are changes
    //      to show them to the student.

    conEdit: null,
    stencil: null,
    
    constructor: function(){
        this.conEdit = dojo.byId("conEdit");    
        dojo.connect(this.conEdit, "keydown", this, "textMonitor");
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
        if(evt.keyCode == dojo.keys.SPACE || evt.keyCode == dojo.keys.TAB || evt.keyCode == 188){
	    console.log("andes.WordTip.textMonitor this=",this);
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
	console.assert(this.theDrawing,"WordTip needs drawing initialized");
        var current;
        var andesTypes = andes.convert.andesTypes;

        // The most recent stencil will either be the last selected or the last
        // tool.  Thus find out the id, if it matches the last selected that's
        // it.  If not then it must be the current tool.
            
        // console.log("stencils......>>",this.theDrawing.stencils.getRecentStencil(), "attr check: ", dojo.attr(this.conEdit.parentNode, "id"));
        var stencilID = dojo.attr(this.conEdit.parentNode, "id"),
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
        };
        // console.log("current: ",current);
	if(current && this.hasTip[current]){
	    andes.api.suggestWord({type: current, text: text, symbol:symbol});
	}
    },
    
    processResults: function(results){
        // Return may also include log messages and other directives.
        // Here, we ignore any other directives.
        dojo.forEach(results, function(line){
            if(line.action=="next-words"){
                dijit.hideTooltip(this.conEdit);
                if(line.words.length > 0 || line["last-word"]){
                    var size = Math.min(7, line.words.length);
		    var wrd = line["last-word"] ? "&lt;done&gt;" : "";
                    for(var i=0; i<size; i++){
			if(wrd.length>0) {wrd += ", ";}
                        wrd += line.words[i];
                    };
		    if(i<line.words.length){
			wrd += ", &#8230;";
		    }
                    console.log("Successfully retrieved tips: ", line.words.join(), " \nminimized to: ", wrd);
                    dijit.showTooltip(wrd, this.conEdit, "above");   
                };
            };
        },this);
    }
});
});
