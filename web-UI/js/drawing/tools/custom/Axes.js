dojo.provide("drawing.tools.custom.Axes");
dojo.require("drawing.stencil.Path");
dojo.require("drawing.stencil._Slave");
dojo.require("drawing.tools.custom._Base");


drawing.tools.custom.Axes = drawing.util.oo.declare(
	drawing.stencil.Path,
	drawing.tools.custom._Base,
	function(options){
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
		
		this.connect("onDelete", this, function(){
			this.yArrow.destroy();
			this.xArrow.destroy();
		});
	},
	{
		draws:true,
		type:"drawing.tools.custom.Axes",
		minimumSize:30,
		
		createLabels: function(){
			// NOTE: Not passing style into text because it's changing it
			var props = {align:"middle", valign:"middle", util:this.util, annotation:true, parent:this.parent, mouse:this.mouse, stencil:this};
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
		
		
		anchorPositionCheck: function(x, y, anchor){
			// summary:
			//	Gets called from anchor to check if its current
			//	position is ok. If not, its x or y transform will
			// be changed until this passes.
			//
			var pm = this.parent.getParent().getTransform();
			var am = anchor.shape.getTransform();
			
			// the xaxis point has changed and is not yet set as a point
			//	- but the center should be good (except for the transform).
			// Now check the yaxis point.
			
			var p = this.points;
			var o = {x:am.dx+anchor.org.x+pm.dx, y:am.dy+anchor.org.y+pm.dy};
			var c = {x:p[1].x+pm.dx, y:p[1].y+pm.dy};
			var ox = c.x - (c.y - o.y);
			var oy = c.y - (o.x - c.x);
			
			return {x:ox, y:oy};
			
		},
		
		onTransformBegin: function(anchor){
			// called from anchor point up mouse down
			this._isBeingModified = true;
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
			var ox = obj.start.x - (obj.start.y - obj.y);
			var oy = obj.start.y - (obj.x - obj.start.x);
			
			if(ox<0 || oy<0){
				return;
			}
			this.points = [{x:obj.x, y:obj.y}, {x:obj.start.x, y:obj.start.y, noAnchor:true}]
			
			this.points.push({x:ox, y:oy, noAnchor:true});
			this.render();
		},
		onUp:function(){
			var p = this.points;
			var len = this.util.distance(p[1].x,p[1].y,p[0].x,p[0].y);
			if(!p || !p.length){
				return;
			}else if(len < this.minimumSize){
				this.remove(this.shape, this.hit);
				this.xArrow.remove(this.xArrow.shape, this.xArrow.hit);
				this.yArrow.remove(this.yArrow.shape, this.yArrow.hit);
				return;
			}
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
