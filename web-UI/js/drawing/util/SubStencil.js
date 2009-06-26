dojo.provide("drawing.util.SubStencil");

drawing.util.SubStencil = drawing.util.oo.declare(
	
	function(master){
		this.master = master;
		this._props = {style:this.master.style, id:this.master.id, util:this.master.util, parent:this.master.parent, mouse:this.master.mouse};
		this.slaves = [];
		
		this._cons = [];
	},
	{
		add: function(constr, props){
			var s = new constr(dojo.mixin(this._props, props || {}));
			this.master.connectMult([
				[this.master, "select", s, "select"],
				[this.master, "deselect", s, "deselect"],
				[this.master, "render", s, "render"]
			]);
				
			this.slaves.push(s);
			return s;
		},
		
		render: function(){
			dojo.forEach(this.slaves, function(s){
				s.render();
			});
		}
	}
)