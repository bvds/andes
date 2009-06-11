dojo.provide("drawing.util.oo");

// FIXME:
// may need IE testing for props like toString, etc

// TODO:
// allow a declare without a mixin

drawing.util.oo = {
	declare: function(){
		var f, o, ext=0, a = arguments;
				
		if(a.length<2){ console.error("gfx.oo.declare; not enough arguments")}
		if(a.length==2){
			f = a[0]; o = a[1];
		}else{
			a = Array.prototype.slice.call(arguments);
			o = a.pop();
			f = a.pop();
			ext = 1;
		}
		for(var n in o){
			f.prototype[n] = o[n];
		}
		if(ext){
			a.unshift(f);
			f = this.extend.apply(this, a);
		}
		return f;
	},
	extend: function(){
		var a = arguments, sub = a[0];
		if(a.length<2){ console.error("gfx.oo.extend; not enough arguments")}
		var f = function (){
			sub.prototype.constructor.apply(this, arguments);
			for(var i=1;i<a.length;i++){
				a[i].prototype.constructor.apply(this, arguments);
			}
		}
		for(var i=1;i<a.length;i++){
			for(var n in a[i].prototype){
				f.prototype[n] = a[i].prototype[n];
			}
		}
			
		for(var n in sub.prototype){
			f.prototype[n] = sub.prototype[n];
		}
		return f;
	}
};