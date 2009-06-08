dojo.provide("dojox.sketch.LineAnnotation");
dojo.require("dojox.sketch.Annotation");
dojo.require("dojox.sketch.Anchor");

(function(){
	var ta=dojox.sketch;
	ta.LineAnnotation=function(figure, id){
		ta.Annotation.call(this, figure, id);
		this.transform={ dx:0, dy:0 };
		this.start={x:0, y:0};
		this.end={x:200, y:0};
		this.textPosition={ x:0, y:0 };
		this.textOffset=4;  // distance from line to text box
		this.textAlign="middle";
		this.textYOffset=10;  // height of text box
		this.rotation=0;

//		this.property('label',this.id);
//		this.label=this.id;
		this.pathShape=null;
		this.labelShape=null;

		this.anchors.start=new ta.Anchor(this, "start");
		this.anchors.end=new ta.Anchor(this, "end");
	};
	ta.LineAnnotation.prototype=new ta.Annotation;
	var p=ta.LineAnnotation.prototype;
	p.constructor=ta.LineAnnotation;

	p.type=function(){ return 'Line'; };
	p.getType=function(){ return ta.LineAnnotation; };

	p._pos=function(){
		//	text position
	  // label at middle of vetor
	  var x=0.5*(this.start.x+this.end.x);
	  var y=0.5*(this.start.y+this.end.y);
	  // move label a set distance from the line
	  var slope=this.calculate.slope(this.start, this.end);
	  var deltay = this.textOffset/Math.sqrt(1.0+slope*slope);
	  if(this.end.y>this.start.y){deltay = -deltay;}
	  x += -deltay*slope;
	  y += deltay;
	  
	  // want text to be away from start of vector
          // This will make force diagrams less crowded
	  if(this.end.x<this.start.x){
	    this.textAlign="end";
	  } else {
	    this.textAlign="start";
	  }
	  if(this.end.y<this.start.y){
	    y += this.textYOffset;
	  }
	  this.textPosition={ x:x, y:y };
	    };
	
	p.apply=function(obj){
		if(!obj){ return; }
		if(obj.documentElement){ obj=obj.documentElement; }
		this.readCommonAttrs(obj);
		
		for(var i=0; i<obj.childNodes.length; i++){
			var c=obj.childNodes[i];
			if(c.localName=="text"){ 
				this.property('label',c.childNodes.length?c.childNodes[0].nodeValue:'');
			}
			else if(c.localName=="path"){
				//	the line
				var d=c.getAttribute('d').split(" ");
				var s=d[0].split(",");
				this.start.x=parseFloat(s[0].substr(1),10);
				this.start.y=parseFloat(s[1],10);
				s=d[1].split(",");
				this.end.x=parseFloat(s[0],10);
				this.end.y=parseFloat(s[1],10);
				var stroke=this.property('stroke');
				var style=c.getAttribute('style');
				var m=style.match(/stroke:([^;]+);/);
				if(m){
					stroke.color=m[1];
					this.property('fill',m[1]);
				}
				m=style.match(/stroke-width:([^;]+);/);
				if(m){
					stroke.width=m[1];
				}
				this.property('stroke',stroke);
			}
		}
	};
	p.initialize=function(obj){
		//	create, based on passed DOM node if available.
		var font=(ta.Annotation.labelFont)?ta.Annotation.labelFont:{family:"Times", size:"16px"};
		this.apply(obj);

		//	calculate the other positions
		this._pos();

		//	draw the shapes
		this.shape=this.figure.group.createGroup();
		this.shape.getEventSource().setAttribute("id", this.id);
		//if(this.transform.dx||this.transform.dy){ this.shape.setTransform(this.transform); }

		this.pathShape=this.shape.createPath("M"+this.start.x+","+this.start.y+" L"+this.end.x+","+this.end.y)
			//.setStroke(this.property('stroke'));

		this.labelShape=this.shape.createText({
				x:this.textPosition.x, 
				y:this.textPosition.y, 
				text:this.property('label'), 
				align:this.textAlign
			})
			//.setFont(font)
			//.setFill(this.property('fill'));
		this.labelShape.getEventSource().setAttribute('id',this.id+"-labelShape");
		this.draw();
	  // pop up dialog box automatically to get initial text
	  var l=prompt('Define line:',this.property('label'));
	  if(l!=false){
	    this.property('label',l);
	    // pick out any variable name
	    console.log("variable name ",getVariableName(l));
	    this.draw();	    // redraw with label this time
	    }
	};

	p.destroy=function(){
		if(!this.shape){ return; }
		this.shape.remove(this.pathShape);
		this.shape.remove(this.labelShape);
		this.figure.group.remove(this.shape);
		this.shape=this.pathShape=this.labelShape=null;
	};

	p.draw=function(obj){
		this.apply(obj);
		this._pos();

		this.shape.setTransform(this.transform);
	  this.pathShape.setShape("M"+this.start.x+","+this.start.y+" L"+this.end.x+","+this.end.y);

	  this.labelShape.setShape({
				x:this.textPosition.x, 
				y:this.textPosition.y, 
				text:this.property('label'), 
				align:this.textAlign
			})
			.setFill(this.property('fill'));
		this.zoom();
	};

	p.zoom=function(pct){
	  pct = pct || this.figure.zoomFactor;
			ta.Annotation.prototype.zoom.call(this,pct);
	};

          // This is a copy of the corresponding line function
	  // Canvas doesn't like zero-width boxes.
	  // http://trac.dojotoolkit.org/ticket/5271
        p.getBBox=function(){
	  var linewidth=this.property('stroke').width;
	  var x=Math.min(this.start.x, this.end.x)-0.5*linewidth;
	  var y=Math.min(this.start.y, this.end.y)-0.5*linewidth;
	  var w=Math.abs(this.end.x-this.start.x)+linewidth;
	  var h=Math.abs(this.end.y-this.start.y)+linewidth;
          return { x:x, y:y, width:w, height:h };
	};

	p.serialize=function(){
		var s=this.property('stroke');
		return '<g '+this.writeCommonAttrs()+'>'
			+ '<path style="stroke:'+s.color+';stroke-width:'+s.width+';fill:none;" d="'
			+ "M"+this.start.x+","+this.start.y+" "
			+ "L"+ this.end.x+","+this.end.y
			+ '" />'
			+ '<text style="fill:'+s.color+';text-anchor:'+this.textAlign+'" font-weight="bold" '
			+ 'x="' + this.textPosition.x + '" '
			+ 'y="' + this.textPosition.y + '">'
			+ this.property('label')
			+ '</text>'
			+ '</g>';
	};

	ta.Annotation.register("Line");
})();
