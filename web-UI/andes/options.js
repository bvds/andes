dojo.provide("andes.options");

dojo.declare("andes.options",null,{
    values: {
        angleSnap: null,
        clickMode: null
    },
    constructor: function(){
        // Get objects
        this.angleSnap = dijit.byId("optionsAngleSnap");
        this.clickMode = dijit.byId("optionsClickMode");
        this.save = dijit.byId("optionsSave");
        this.dialog = dijit.byId("options");
        
        // Initialize values
        this.angleSnap.set('value', myDrawing.defaults.angleSnap);
        this.clickMode.set('label', myDrawing.defaults.clickMode ? "enabled" : "disabled");
        
        // Set up connections
        dojo.connect(this.angleSnap, "onChange", this, "updateAngleSnap");
        dojo.connect(this.clickMode, "onChange", this, "updateClickMode");
        dojo.connect(this.clickMode, "onBlur", function(evt){dojo.stopEvent(evt);});
        dojo.connect(this.save, "onClick", this, "setChanges");
    },
    
    // Function to update values, change can only take one at a time.
    updateAngleSnap: function(val){
        console.log("val: ",val);
        this.values["angleSnap"] = val;
    },
    
    updateClickMode: function(val){
        console.log("val: ",val);
        this.values["clickMode"] = val;
        this.clickMode.set('label', val ? "enabled" : "disabled");
    },
    
    // Function to setDefaults (call update first)
    setChanges: function(){
        // First see what changed, now it's only one check
        console.log("Setting Changes: ");
        for(var nm in this.values){
            if(this.values[nm]!=null){
                console.log("Changing: ",nm," to: ", this.values[nm]);
                var obj = {};
                obj[nm] = this.values[nm];
                myDrawing.changeDefaults(obj,true);
            }
        }
        this.dialog.hide();
    }
    //This should be instantiated in menu
});