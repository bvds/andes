dojo.provide("andes.Combo");

andes.Combo = drawing.util.oo.declare(
	// summary:
	//	A special object used to combine an Andes Stencil
	//	(Vector, Rect, Ellipse)
	//	and a Statement. Objects are implicitly linked.
	//	Selecting one highlights the other. Moving one does
	//	*not* move the other. Deleting one *does* delete the other.
	//
	//	This object is what relays events to andes.drawing, not the
	//	individual items.
	//
	function(options){
		this.master = options.master;
		this.statement = options.statement;
		
		this._props = {style:this.master.style, util:this.master.util, parent:this.master.parent, mouse:this.master.mouse};
		dojo.mixin(this, this._props);
		this.id = options.id || this.util.uid(this.type);
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
		
		this.created = options.onCreate ? false : true
			
		var s = this.statement;
		var m = this.master;
		
		console.log("combo statement:", this.statement)
		
		this.statement.connectMult([
			[this.statement, "onChangeText", this, function(value){
				var label = andes.variablename.parse(value);
				if(label){
					console.log("LABEL", label)
					this.master.setLabel(label);
					this.statement.selectOnExec = true;
				}else{
					console.log("NO LABEL", label)
					this.master.setLabel(value);
					this.statement.setText("");
					this.statement.selectOnExec = false;
				}
				if(!this.created){
					this.created = true;
					options.onCreate();
				}else{
					this.onChangeData(this);
				}
				
				this.onChangeText(this);
			
			}],
			[this.statement, "onChangeData", this, function(){
				this.onChangeData(this);
			}],
			[this.master, "select", this.statement, "highlight"],
			[this.master, "deselect", this.statement, "unhighlight"],
			[this.statement, "select", this.master, "highlight"],
			[this.statement, "deselect", this.master, "unhighlight"],
			[this.statement, "onDelete", this, function(){
				if(!this._masterDestroyed){
					this._masterDestroyed = true;
					!this._statementDestroyed && this.onDelete(this);
					this.master.destroy();
				}
			}],
			[this.master, "onDelete", this, function(){
				if(!this._statementDestroyed){
					this._statementDestroyed = true;
					!this._masterDestroyed && this.onDelete(this);
					this.statement.destroy();
				}
			}]
		]);
	
	},
	{
		type:"andes.Combo",
		_masterDestroyed: false,
		_statementDestroyed: false,
		onChangeData: function(/*Object*/ stencil){
			console.log("--------------on change combo", stencil.id);
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
			console.log("combo delete ", value)
			// summary:
			//	Stub - fires when master or an item is deleted
			//	(which makes this _Connection worthless and it
			//	should be discarded)
		},
		
		getItem: function(){
			// NOT USED
			return this.statement;
		},

		attr: function(a1, a2){
			// see Drawing.stencil._Base
			this.master.attr.call(this.master, a1, a2);
			this.statement.attr.call(this.statement, a1, a2);
		},
		
		//
		// TODO: can I use master.connect if I apply it correctly?
		//
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
		}
	}
)