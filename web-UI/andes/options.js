dojo.provide("andes.options");
dojo.require("dijit.ColorPalette");

dojo.declare("andes.options",null,{
    values: {
        angleSnap: null,
        clickMode: null,
        correct: null,
        incorrect: null
    },
    
    constructor: function(){
        // Get objects
        this.angleSnap = dijit.byId("optionsAngleSnap");
        this.clickMode = dijit.byId("optionsClickMode");
        this.save = dijit.byId("optionsSave");
        this.dialog = dijit.byId("options");
        this.correct = dojo.byId("colorCorrect");
        this.incorrect = dojo.byId("colorIncorrect");
        this.showTimer = dijit.byId("showTimer");
        
        // Initialize values
        this.angleSnap.set('value', myDrawing.defaults.angleSnap);
        this.clickMode.set('label', myDrawing.defaults.clickMode ? "enabled" : "disabled");
        this.showTimer.set('label', andes.timer.display ? "enabled" : "disabled");
        dojo.style(this.correct, "background", myDrawing.defaults.correct.color);
        dojo.style(this.incorrect, "background", myDrawing.defaults.incorrect.color);
        
        var ops = this;
        this.picker = new dijit.ColorPalette({
            id:"picker",
            open:false,
            onChange: function(value){
                this.onExecute();
            },
            onExecute: function(){
                this.open = false;
                dijit.popup.close(this);
            },
            onCancel: function(/*Boolean*/ closeAll){
                this.open = false;
                dijit.popup.close(this);
            }}, dojo.doc.createElement("div"));
        dijit.popup.moveOffScreen(this.picker);
        // Set up connections
        dojo.connect(this.angleSnap, "onChange", this, "updateAngleSnap");
        dojo.connect(this.clickMode, "onChange", this, "updateClickMode");
        dojo.connect(this.showTimer, "onChange", this, "updateShowTimer");
        dojo.connect(this.save, "onClick", this, "setChanges");
        dojo.connect(this.correct, "onclick", this, "colorChange");
        dojo.connect(this.incorrect, "onclick", this, "colorChange");
        dojo.connect(this.dialog, "onHide", this, function(){
            this.picker.open && this.picker.onCancel();
        });
    },
    
    // Function to update values, change can only take one at a time.
    updateAngleSnap: function(val){
        //console.log("val: ",val);
        this.values["angleSnap"] = val;
    },
    
    updateClickMode: function(val){
        //console.log("val: ",val);
        this.values["clickMode"] = val;
        this.clickMode.set('label', val ? "enabled" : "disabled");
    },
    
    updateShowTimer: function(val){
        this.timer = val;
        this.showTimer.set('label', val ? "enabled" : "disabled");
    },
    
    // We need two colors
    colorChange: function(evt){
        var c = dojo.connect(this.picker, "onChange", this, function(value){
            dojo.disconnect(c);
            dojo.style(evt.target, "background", value);
            var v = evt.target == this.correct ? "correct" : "incorrect";
            console.log("fill: ",value);
            var R = Math.round(parseInt(value.substr(1,2),16)+50);
            var G = Math.round(parseInt(value.substr(3,2),16)+50);
            var B = Math.round(parseInt(value.substr(5,2),16)+50);
            R = R > 255 ? 255 : R;
            G = G > 255 ? 255 : G;
            B = B > 255 ? 255 : B;
            var fill = "#"+R.toString(16)+G.toString(16)+B.toString(16);
            console.log("R: ", R, " G: ", G, " B: ", B, " fill: ",fill);
            this.values[v] = {color: value, fill: fill};
        });
        this.picker.open = true;
        dijit.popup.open({
            popup: this.picker,
            around: evt.target
        });
    },

    // Function to setDefaults (call update first)
    setChanges: function(){
        // First see what changed, now it's only one check
        console.log("Setting Changes: ");
        for(var nm in this.values){
            if(this.values[nm]!=null){
                //console.log("Changing: ",nm," to: ", this.values[nm]);
                var obj = {};
                obj[nm] = this.values[nm];
                myDrawing.changeDefaults(obj,true);
            }
        }
        this.timer ? andes.timer.displayTimer() : andes.timer.hideTimer();
        this.dialog.hide();
    }
    //This should be instantiated in menu
});