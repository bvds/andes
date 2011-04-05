dojo.provide("andes.options");
dojo.require("dijit.ColorPalette");

dojo.declare("andes.options",null,{
    // Summary:
    //      Options is a pseudo dijit which is contained
    //      in a dijit.Dialog in HTML.  Probably should be a full
    //      blown widget, that would help with event control which
    //      is sloppy here.
    //
    //      This contains values which can be modified and saved by the user.
    //      Because of that it needs to register these prefs with PreferenceRegistry
    //      in order to interface with the server.
    _prefs: {
        "angleSnap":    "setAngleSnap",
        "clickMode":    "setClickMode",
        "timer":        "setShowTimer",
        "correct":      "setCorrectColor",
        "incorrect":    "setIncorrectColor"
    },
    
    // Name value pair of a widget used to set values in this object.
    // The name associated with it first, followed by its ID in the HTML
    userWidgets: {
        "angleSnap": "optionsAngleSnap",
        "clickMode": "optionsClickMode",
        "save":      "optionsSave",
        "dialog":    "options",
        "showTimer": "showTimer"
    },
    
    // Name value pair of html elements showing settings, same idea as
    // userWidgets except these are dojo.byId while widgets are dijit.byId
    userVisuals: {
        "correct":   "colorCorrect",
        "incorrect": "colorIncorrect"
    },
    
    constructor: function(){
        // Link up references to the HTML/Dijits
        for(var nm in this.userWidgets){
            this[nm] = dijit.byId(this.userWidgets[nm]);
        }
        for(var nm in this.userVisuals){
            this[nm] = dojo.byId(this.userVisuals[nm]);
        }
        
        // Register preferences
        for(var nm in this._prefs){
            andes.preferenceRegistry.registerPref(nm, this[this._prefs[nm]], this);
        }
        
        // Initialize values -- myDrawing is a GLOBAL
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
        this.connectMult([
            [this.angleSnap, "onChange", this, "setAngleSnap"],
            [this.clickMode, "onChange", this, "setClickMode"],
            [this.showTimer, "onChange", this, "setShowTimer"],
            [this.correct, "onclick", this, "colorChange"],
            [this.incorrect, "onclick", this, "colorChange"],
            [this.dialog, "onHide", this, function(){ this.picker.open && this.picker.onCancel(); }]
        ]);
    },
    
    connectMult: function(c){
        dojo.forEach(c, function(o){
            dojo.connect.apply(this, o);
        });
    },
    
    // Function to update values, change can only take one at a time.
    setAngleSnap: function(val){
        this._setChange("angleSnap", val);
    },
    
    setClickMode: function(val){
        this.clickMode.set('label', val ? "enabled" : "disabled");
        this._setChange("clickMode", val);
    },
    
    setShowTimer: function(val){
        this.timer = val;
        this.showTimer.set('label', val ? "enabled" : "disabled");
        this._setChange("timer", val, andes.timer.displayTimer, andes.timer);
    },
    
    // TODO:
    //      Right now the fill color is always calculated, not always
    //      perfectly.  Possibly work on the calculation?  or allow
    //      the fill to also be set.
    //
    //      Separate set colors for the preference registry api
    setCorrectColor: function(val){
        this._setColor(this.correct, "correct", val);
    },
    
    setIncorrectColor: function(val){
        this._setColor(this.incorrect, "incorrect", val);
    },
    
    _setColor: function(node, name, val){
        var o = {};
        if(typeof(val)=="object"){
            // We already have a fill value
            o = val;
        }else{
            var fill = this._getFill(val);
            o = { color:val, fill:fill };
        }
        
        dojo.style(node, "background", o.color);
        this._setChange(name, o);
    },
    
    _getFill: function(/*Hexadecimal Number*/color){
        var R = Math.round(parseInt(color.substr(1,2),16)+50),
            G = Math.round(parseInt(color.substr(3,2),16)+50),
            B = Math.round(parseInt(color.substr(5,2),16)+50);
        R = R > 255 ? 255 : R;
        G = G > 255 ? 255 : G;
        B = B > 255 ? 255 : B;
        //console.log("R: ", R, " G: ", G, " B: ", B, " fill: ",fill);
        return "#"+R.toString(16)+G.toString(16)+B.toString(16);
    },
    
    // Connected to the color picker this allows the user to change
    // both correct and incorrect color schemes
    colorChange: function(evt){
        var c = dojo.connect(this.picker, "onChange", this, function(value){
            dojo.disconnect(c);
            if(evt.target == this.correct){
                this.setCorrectColor(value);
            }else{
                this.setIncorrectColor(value);
            }
        });
        this.picker.open = true;
        dijit.popup.open({
            popup: this.picker,
            around: evt.target
        });
    },

    // Function to set the changes in the actual UI
    _setChange: function(name, value, /*function*/f, /*scope*/s){
        if(!f){
            var o = {};
            o[name] = value;
            myDrawing.changeDefaults(o, true);
        }else{
            f.call(s, value);
        }
        andes.preferenceRegistry.savePref(name, value);
    }
    //This should be instantiated in menu
});