dojo.provide("andes.positioning");	

(function(){
	
	// summary:
	//	Overwrites the default methods in Drawing used for
	//	positioning labels.
	//
	//	Is this necessary???
	//
	//	Note: Currently these methods are the same, but this
	//	object helps provide protection from future changes to
	//	Drawing.
	
	var textOffset = 4;  // distance from line to text box
	var textYOffset = 20;  // height of text box
	
	//create the namespace
	dojox.drawing.util.positioning = {};
	dojox.drawing.util.positioning.label = function(start, end){
		// summary:
		//		Returns the optimal text positions for annotations.Label.
		
		// label at middle of vector
		var x = 0.5*(start.x+end.x);
		var y = 0.5*(start.y+end.y);
		
		// move label a set distance from the line
		var slope = dojox.drawing.util.common.slope(start, end);
		var deltay = textOffset/Math.sqrt(1.0+slope*slope);
		
		if(end.y>start.y && end.x>start.x || end.y<start.y && end.x<start.x){
			// Position depending on quadrant.  Y offset
			// positions box aligned vertically from top
			deltay = -deltay;
			y -= textYOffset;
		}
		x += -deltay*slope;
		y += deltay;
		
		// want text to be away from start of vector
		// This will make force diagrams less crowded
		var align = end.x<start.x ? "end" : "start";
		
		return { x:x, y:y, foo:"bar", align:align}; // Object
	};
	
	dojox.drawing.util.positioning.angle = function(start, end){
		
		// angle at first third of vector
	        var x = 0.7*start.x+0.3*end.x;
	        var y = 0.7*start.y+0.3*end.y;
		// move label a set distance from the line
		var slope = dojox.drawing.util.common.slope(start, end);
		var deltay = textOffset/Math.sqrt(1.0+slope*slope);
		
		if(end.x<start.x){deltay = -deltay;}
		x += -deltay * slope;
		y += deltay;
		
		// want text to be clockwise from vector
		// to match angle measurement from x-axis
		var align = end.y>start.y ? "end" : "start";
	        // box vertical aligned from middle
	        y += end.x > start.x ? 0.5*textYOffset :  -0.5*textYOffset;
		
		return { x:x, y:y, align:align};
	};
	
})();
