/* global define */
// Pre-AMD version had a function wrapper.
// pre-AMD version had no requires.
define([
],function(){

    // Summary:
    //      User preferences that can be saved are registered here.
    //      When preferences are updated this saves them to the server
    //      via the api, while the opposite direction allows the
    //      server to update values that register here.
    //      Example api:
    //          andes.api.recordAction({type:"set-preference", name: "display-timer", value: true});
    var _prefs = {};

    window.andes.preferenceRegistry = {
        // Prefs are name value pairs with scope so that the server
        // can update them.
        registerPref: function(/*String*/pref, /*function*/setter, scope){
            _prefs[pref] = { set: setter, scope: scope };
        },
        
        // summary:
        //      Once a setting has been registered calling this with the name
        //      value pair will save it to server.  If it hasn't been registered
        //      it will fail and return false.
        savePref: function(pref, value){
            if(!_prefs[pref]){
                console.warn("Must register preference before saving to it");
                return false;
            }else{
                if(_prefs[pref].value != value){
                    // This is user changed
                    window.andes.api.recordAction({type:"set-preference", name:pref, value:value });
                }
                return true;
            }
        },
        
        // summary:
        //      This willl attempt to set in the browser the registered preference
        //      and it will save the value so that duplicate requests aren't sent
        //      to the server
        setPref: function(pref, value){
            if(_prefs[pref]){
                _prefs[pref].value = value;
                var f = _prefs[pref].set,
                    s = _prefs[pref].scope;
                    
                f.call(s, value);
                return true;
            }else{
                console.warn("Attempted to set a preference not registered: ",pref);
                return false;
            }
        }
    };
});
