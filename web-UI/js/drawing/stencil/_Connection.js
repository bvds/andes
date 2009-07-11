dojo.provide("drawing.stencil._Connection");

drawing.stencil._Connection = drawing.util.oo.declare(
	//
	// TODO: Best name for this?
	//		_Slave could be _Attach
	//
	function(master){
		this.master = master;
		this._props = {style:this.master.style, id:this.master.id, util:this.master.util, parent:this.master.parent, mouse:this.master.mouse};
		this.linked = [];
		
	},
	{
		add: function(/* Stencil */item, /* Array */ masterConnects, /* Array */ itemConnects){
			this.master.connectMult([
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
			console.warn("ATTR:", a1, a2)
			this.master.attr.call(this.master, a1, a2);
			for(var n in this.linked){
				var item = this.linked[n];
				item.attr.call(item, a1, a2);
			}
		},
		withItems: function(func){
			// convenience function
			var f = dojo.hitch(this, func);
			for(var m in this.linked){
				f(this.linked[m]);
			}
		}
	}
)