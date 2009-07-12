dojo.provide("drawing.tools.custom.Axes");
dojo.require("drawing.stencil.Path");
dojo.require("drawing.stencil._Slave");
dojo.require("drawing.tools.custom._Base");


drawing.tools.custom.Axes = drawing.util.oo.declare(
	drawing.stencil.Path,
	drawing.tools.custom._Base,
	function(options){
		//this.style.norm.fill = null;
		//this.style.selected.fill = null;
		this.closePath = false;
		
		this.slaves = new drawing.stencil._Slave(this);
		this.xArrow = this.slaves.add(drawing.stencil.Path);
		this.yArrow = this.slaves.add(drawing.stencil.Path);
		
		this.connect(this.xArrow, "onBeforeRender", this, function(){
			var o = this.points[0];
			var c = this.points[1];
			this.xArrow.points = this.util.arrowHead(c.x, c.y, o.x, o.y, this.style);
		});
		
		this.connect(this.yArrow, "onBeforeRender", this, function(){
			var o = this.points[2];
			var c = this.points[1];
			this.yArrow.points = this.util.arrowHead(c.x, c.y, o.x, o.y, this.style);
		});
		
		
	},
	{
		draws:true,
		type:"drawing.tools.custom.Axes",
		
		createLabels: function(){
			var props = {style:this.style, align:"middle", valign:"middle", util:this.util, annotation:true, parent:this.parent, mouse:this.mouse, stencil:this};
			this.labelX = new drawing.stencil._Label(dojo.mixin(props,{
				labelPosition:this.setLabelX
			}));
			this.labelY = new drawing.stencil._Label(dojo.mixin(props,{
				labelPosition:this.setLabelY
			}));
		},
		setLabelX: function(){
			var ax = this.points[0];
			var c =  this.points[1];
			var ay = this.points[2];
			
			var dist = 40;
			var offdist = 20;
			var pt, px, py, pt2;
			
			pt = this.util.lineSub(c.x, c.y, ax.x, ax.y, dist);
			px = pt.x + (pt.y -ax.y);
			py = pt.y + (ax.x - pt.x);
			pt2 = this.util.lineSub(pt.x, pt.y, px, py, (dist-offdist));
			
			return {
				x:  pt2.x,
				y:  pt2.y,
				width:20
			};
		},
		setLabelY: function(){
			var ax = this.points[0];
			var c =  this.points[1];
			var ay = this.points[2];
			
			var dist = 40;
			var offdist = 20;
			var pt, px, py, pt2;
			pt = this.util.lineSub(c.x, c.y, ay.x, ay.y, dist);
			px = pt.x + ( ay.y-pt.y);
			py = pt.y + (pt.x - ay.x );
			pt2 = this.util.lineSub(pt.x, pt.y, px, py, (dist-offdist));
			pt2 = this.util.lineSub(pt.x, pt.y, px, py, (dist-offdist));
			
			return {
				x:  pt2.x,
				y:  pt2.y,
				width:20
			};
		},
		setLabel: function(value){
			!this.labelX && this.createLabels();
			var x = "X";
			var y = "Y";
			if(value){
				value = value.replace(/and|(\+)/, " "); // what other words would they use?
				var lbls = value.match(/(\b\w+\b)/g);
				if(lbls.length==2){
					x = lbls[0];
					y = lbls[1];
				}
			}
			this.labelX.setLabel(x);
			this.labelY.setLabel(y);
		},
		
		onTransformEnd: function(anchor){
			//	tell anchor to go to prev point if wrong
			// called from anchor point up mouse up
			this.isBeingModified = false;
			
			var o = this.points[0];
			var c = this.points[1];
			var pt = this.util.constrainAngle({start:{x:c.x, y:c.y}, x:o.x, y:o.y}, 91, 180);
			if(pt.x==o.x && pt.y == o.y){
				return;
			}
			// careful changing these points. The anchors are
			// linked as an instance to the objects
			this.points[0].x = pt.x
			this.points[0].y = pt.y;
			o = this.points[0];
			
			var ox = c.x - (c.y - o.y);
			var oy = c.y - (o.x - c.x);
			
			this.points[2] = {x:ox, y:oy, noAnchor:true};
			
			this.render();	
			
			this.setPoints(this.points);
			this.setLabel();
			
			// a desperate hack in order to get the anchor point to reset.
			var st = this.util.byId("drawing").stencils;
			st.onDeselect(this);
			st.onSelect(this);
			
		},
		
		getBounds: function(){
			console.trace()
			// custom getBounds
			var px = this.points[0],
				pc = this.points[1],
				py = this.points[2],
			
				x1 = py.x,
				y1 = py.y < px.y ? py.y : px.y,
				x2 = px.x,
				y2 = pc.y;
			
			return {
				x1:x1,
				y1:y1,
				x2:x2,
				y2:y2,
				x:x1,
				y:y1,
				w:x2-x1,
				h:y2-y1
			};
		},
		
		_postSetPoints: function(pts){
			this.points[0] = pts[0];
			if(this.pointsToData){
				this.data = this.pointsToData();
			}
		},
		
		onTransform: function(anchor){
			// the xaxis point has changed - the center will not.
			// need to find the yaxis point.
			
			var o = this.points[0];
			var c = this.points[1];
			var ox = c.x - (c.y - o.y);
			var oy = c.y - (o.x - c.x);
			// 'noAnchor' on a point indicates an anchor should
			// not be rendered. This is the Y point being set.
			this.points[2] = {x:ox, y:oy, noAnchor:true};
			
			this.setPoints(this.points);
			if(!this.isBeingModified){
				this.onTransformBegin();
			}
			this.render();	
		},
		pointsToData: function(){
			var p = this.points;
			return {
				x1:p[1].x,
				y1:p[1].y,
				x2:p[0].x,
				y2:p[0].y
			};
		},
		
		onDrag: function(obj){
			
			var pt = this.util.constrainAngle(obj, 91, 180);
			obj.x = pt.x;
			obj.y = pt.y;
			this.points = [{x:obj.x, y:obj.y}, {x:obj.start.x, y:obj.start.y, noAnchor:true}]
			var ox = obj.start.x - (obj.start.y - obj.y);
			var oy = obj.start.y - (obj.x - obj.start.x);
			
			this.points.push({x:ox, y:oy, noAnchor:true});
			this.render();
		},
		onUp:function(){
			this.onRender(this);
			this.setPoints = this._postSetPoints;
		}
	}
);

drawing.tools.custom.Axes.setup = {
	name:"drawing.tools.custom.Axes",
	tooltip:"Axes Tool",
	iconClass:"iconAxes"
};
