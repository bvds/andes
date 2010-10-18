dojo.provide("andes.WordTip");

dojo.declare("andes.WordTip", null, {
    // Summary:
    //      Singleton whose job is to watch the textbox
    //      and connect to the server when there are changes
    //      to show them to the student.
    //stub: null,
    conEdit: null,
    constructor: function(){
        this.conEdit = dojo.byId("conEdit");    
        dojo.connect(this.conEdit, "keydown", this, "textMonitor");
        console.log("I've got conedit now", this.conEdit);
    },
    
    textMonitor: function(evt){
        if(evt.keyCode == dojo.keys.SPACE || evt.keyCode == dojo.keys.TAB || evt.keyCode == 188){
            var tx = dojo.trim(this.conEdit.innerHTML);//this.statement.cleanText(conEdit.innerHTML);
            tx = this.removeBreaks(tx);
            console.log("---Text for word-suggest----> ", tx);
            this.sendToServer(tx);
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
    
    sendToServer: function(text){
        andes.api.suggestWord({type: "statement", text: text});
    },
    
    processResults: function(results){
        console.log("Words server reply: ",results);
        if(results.words){
            console.log("Successfully retrieved tips: ", results.words.join());
            dijit.showTooltip(results.words.join(),this.conEdit, "above");
        };   
    }
});