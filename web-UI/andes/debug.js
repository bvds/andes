dojo.provide("andes.debug");

function(){
    // summary
    //      This is for debugging done via scripts, require separate from profile
    //      as needed.  It has commented execution from drawing.plugins.tools.pan
    //      line 126.
    var watched = {};
    var ta;
    watch = function(name, value){
            return;
            if(!ta) {
                    ta = dojo.create("textarea", null, dojo.body());
            }
            if(typeof(value)=="number"){
                    value = Math.ceil(value);
            }
            watched[name] = value;
            var lines=[];
            for(var n in watched){
                    lines.push(n+": "+watched[n]);
            }
            dojo.attr(ta, "rows", lines.length);
            ta.value = lines.join("\n");
    }
}();