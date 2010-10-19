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
	    // BvdS:  this call fails(?) when modifying an existing object.
	    this.sendToServer(tx,symbol);
        };
        if(evt.keyCode == dojo.keys.ENTER || evt.keyCode == dojo.keys.ESCAPE){
            dijit.hideTooltip(this.conEdit);
        }
    },
    
    removeBreaks: function(txt){
        dojo.forEach(['<br>', '<br/>', '<br />', '\\n', '\\r'], function(br){
            txt = txt.replace(new RegExp(br, 'gi'), " ");
        });
        return txt;
    },
    
    sendToServer: function(text,symbol){
        /* This retrieves the parentNode.  Currently we're using drawing
          to get the currentStencil but this may be a better way to do
          it.  Leaving here for future reference.
        var parentObj = dojo.attr(this.conEdit.parentNode,"id");
        console.log("parentOBj: ",parentObj);
        */
        var andesTypes = {
		"dojox.drawing.stencil.Line":"line",
		"dojox.drawing.stencil.Rect":"rectangle",
		"dojox.drawing.stencil.Ellipse":"ellipse", // or circle
		"dojox.drawing.tools.custom.Vector":"vector",
		"dojox.drawing.tools.custom.Axes":"axes",
		"dojox.drawing.tools.custom.Equation":"equation",
		"dojox.drawing.stencil.Image":"graphics",
		"dojox.drawing.tools.TextBlock":"statement", // or statement.... hmmmm
		"andes.buttonCombo":"button"
	};
        
        // BvdS:  This strategy doesn't work in the case of modifying
	// a statement after drawing a vector.
        var current = "statement";
        if(this.drawing){
            var type = this.drawing.currentStencil.type;
            current = andesTypes[type];
        };
        console.log("Suggest for -----------------------", this);
        andes.api.suggestWord({type: current, text: text, symbol:symbol});
    },
    
    processResults: function(results){
        dojo.forEach(results, function(line){
            if(line.action=="next-words"){
                dijit.hideTooltip(this.conEdit);
                if(line.words.length > 0 || line["last-word"]){
                    var size = Math.min(7, line.words.length);
		    var wrd = line["last-word"]?"&lt;done&gt;":"";
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