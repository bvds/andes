dojo.provide("drawing.stencil._Connection");

drawing.stencil._Connection = drawing.util.oo.declare(
	//
	// TODO: Best name for this?
	//		_Slave could be _Attach
	//
	function(master, id){
		this.master = master;
		this._props = {style:this.master.style, util:this.master.util, parent:this.master.parent, mouse:this.master.mouse};
		dojo.mixin(this, this._props);
		this.id = id || this.util.uid(this.type);
		this.linked = [];
		this._cons = [];
		this.master.connectMult([
			[this.master, "onChangeData", this, function(){
				this.onChangeData(this);
			}],
			[this.master, "onChangeText", this, , function(){
				this.onChangeText(this);
			}]
		]);
	},
	{
		type:"drawing.stencil._Connection",
		onChangeData: function(/*Object*/ stencil){
			console.log("--------------on change combo", stencil.id)
			// summary:
			//	Stub - fires on change of dimensional
			//	properties or a text change of the master
			// or any item
		},
		
		onChangeText: function(value){ // value or 'this' ?
			// summary:
			//	Stub - fires on change of text in a
			//	TextBlock tool only
		},
		
		onDelete: function(value){ // value or 'this' ?
			// summary:
			//	Stub - fires when master or an item is deleted
			//	(which makes this _Connection worthless and it
			//	should be discarded)
		},
		getItem: function(){
			// summary:
			//	Since usually (or at least as first intended)
			//	there is only one item used, this is a
			//	convenience function to get it
			for(var n in this.linked){
				var item = this.linked[n];
				return item;
			}
			return null;
		},
		add: function(/* Stencil */item, /* Array */ masterConnects, /* Array */ itemConnects){
			this.master.connectMult([
				[item, "onChangeData", this, function(){
					this.onChangeData(this);
				}],
				[item, "onChangeText", this, "onChangeText"],
			
				[this.master, "select", item, "highlight"],
				[this.master, "deselect", item, "unhighlight"],
				[item, "select", this.master, "highlight"],
				[item, "deselect", this.master, "unhighlight"],
				[item, "destroy", this.master, function(){
					if(!this.destroyed){
						this.destroy();
					}
				}],
				[this.master, "destroy", item, function(){
					if(!this.destroyed){
						this.destroy();
					}
				}]
				
			]);
			
			dojo.forEach(masterConnects, function(ar){
				this.master.connect(this.master, ar[0], item, ar[1]);	
			}, this);
			
			dojo.forEach(itemConnects, function(ar){
				item.connect(item, ar[0], this.master, ar[1]);	
			}, this);
		
			this.linked.push(item);
			return item;
		},
		
		attr: function(a1, a2){
			this.master.attr.call(this.master, a1, a2);
			for(var n in this.linked){
				var item = this.linked[n];
				item.attr.call(item, a1, a2);
			}
		},
		connect: function(o, e, s, m, /* Boolean*/once){
			// summary:
			//	Convenience method for quick connects
			//	See comments below for possiblities
			//	functions can be strings
			// once:
			//	If true, the connection happens only
			//	once then disconnects. Five args are required
			//	for this functionality.
			//
			var c;
			if(typeof(o)!="object"){
				if(s){
					// ** function object function **
					m = s; s = e; e=o; o = this;
				}else{
					// ** function function **
					m = e; e = o; o = s = this;
				}
			}else if(!m){
				// ** object function function **
				m = s; s = this;
			}else if (once){
				// ** object function object function Boolean **
				c = dojo.connect(o, e, function(evt){
					dojo.hitch(s, m)(evt);
					dojo.disconnect(c);
				});
				this._cons.push(c);
				return c;
			}else{
				// ** object function object function **
			}
			c = dojo.connect(o, e, s, m);
			this._cons.push(c);
			return c;
		},
		
		disconnect: function(handles){
			if(!handles) { return };
			if(!dojo.isArray(handles)){ handles=[handles]; }
			dojo.forEach(handles, dojo.disconnect, dojo);
		},
		XXXconnect: function(){
			this.master.connect.apply(this.master, arguments);
		}
	}
)