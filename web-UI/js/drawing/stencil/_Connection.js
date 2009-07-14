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
		this.master.connectMult([
			[this.master, "onChangeData", this, "onChangeData"],
			[this.master, "onChangeText", this, "onChangeText"]
		]);
	},
	{
		type:"drawing.stencil._Connection",
		onChangeData: function(/*Object*/ stencil){
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
		
		add: function(/* Stencil */item, /* Array */ masterConnects, /* Array */ itemConnects){
			this.master.connectMult([
				[item, "onChangeData", this, "onChangeData"],
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
		
		connect: function(){
			this.master.connect.apply(this.master, arguments);
		}
	}
)