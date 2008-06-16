// -----------------------------------------------------------------------------------------
//    Drawing object classes
//
// -----------------------------------------------------------------------------------------
package andes;

import java.awt.*; 
import java.awt.geom.*;
import java.applet.*; 
import java.awt.event.*; 
import java.util.*;
import javax.swing.*;
import java.lang.Math;
import java.lang.System;


     //
     // DrawObj -- base class for our drawable objects
     //
     // The base class methods implement default behavior suitable for an object 
     // defined by a bounding rectangle. Subclasses for which this is not appropriate
     // must override. 
     // Might be better to insert an immediate subclass for Rectangular drawobjs.
     //
     public abstract class DrawObj {
	 // Defining shape object. Default drawing is to render this
	 // shape object. But subclasses might use it in other ways.
	 Shape shape;

	 // true if resizing (affects drawing of vectors)
	 boolean resizing = false;

	 // Many objects have an associated label
	 DrawLabel label;

	 // common drawing attributes (line color, line width, fill color) 
	 // could be stored in fields here
	 
	 // following define default drawing attributes
	 final int DEFAULT_LINE_WIDTH = 4;
	 Paint getLineColor() { return Color.black; }
	 float getLineWidth() { return DEFAULT_LINE_WIDTH; }
	 Paint getFillColor() { return Color.blue; }

	 // Draw this object
	 public void Draw(Graphics g) {
	    Graphics2D g2d = (Graphics2D) g;
	   // fill defining shape's interior with color
	    g2d.setPaint(this.getFillColor());
	    g2d.fill(shape);
	    // draw defining shape's outline
            Stroke oldStroke = g2d.getStroke();
	    g2d.setStroke(new BasicStroke(this.getLineWidth()));
	    g2d.setPaint(this.getLineColor());
	    g2d.draw(shape);
            g2d.setStroke(oldStroke);
	 }

	 // for initial resizing: move dragged point to given coords
	 // !!! Get rid of special initial resizing, use general handle moving instead
	 void ResizeTo(int x, int y) {
	   // Note following only works for RectangularShape derivatives.
	   Rectangle2D old= ((RectangularShape) shape).getFrame();
	   ((RectangularShape) shape).setFrameFromDiagonal(old.getX(), old.getY(), x,  y);
	   Drawing.msg.setText("Frame x1:" + old.getX() + " y1: " + old.getY() + " x2: " + x + " y2: " + y);
	}


	// move whole object by delta
	void MoveBy(int delta_x, int delta_y) {
	  // update position.
	  Rectangle2D old= ((RectangularShape) shape).getFrame();
	  ((RectangularShape) shape).setFrame(old.getX() + delta_x, old.getY() + delta_y,
					 old.getWidth(), old.getHeight());

          // if label exists, move it by same amount
          if (label != null)
		   label.MoveBy(delta_x, delta_y);
	}

	// test if mouse hits object
	boolean Hit(int x, int y) {
		// note following default doesn't work for lines
		return shape.contains(x, y);
	}

	 // For dealing with resize handle boxes:
	 static final int HANDLE_W = 8;		// width in pixels
	
	 // Add selected state indication to drawing of an object, normally
	 // border with resizing handles. Assumption is this can be added as an 
	 // embellishment right after drawing the object. If this turns out to 
	 // be false, could add selected flag to Draw method instead.
	 public void DrawSelectBorder(Graphics g) {
	    Graphics2D g2d = (Graphics2D) g;
	    // draw the handles
	    for (int i = 1; i <= this.getHandleCount(); i++) {
		    Rectangle2D.Double handle = this.getHandleRect(this.getHandlePoint(i));
		    // handle is a black rect rendered in xor mode to ensure visible over
		    // whatever is behind it,  as the C++ Workbench did it. 
		    // Other methods possible. Could have handles outside object.
		    // Could try using alphaComposite for transparency instead.
		    g2d.setPaint(Color.black);
		    g2d.setXORMode(Color.white);
		    g2d.fill(handle);
	    }
	    g2d.setPaintMode();
	 }
         
	 // return number of handles to draw for this object
	 // Handles will be identified by index. 
	 // Maybe better to use list of codes identifying direction.
	 int getHandleCount() { return 8; }

         // get point for resize handle location. Used as center
         // Following only works for RectangularShapes, others must override
	 Point2D getHandlePoint(int i) 
	 {
	    Rectangle2D frame = ((RectangularShape) shape).getFrame();
	    double x, y;
	    switch (i) {
		 case 1: // NorthWest corner
			 x = frame.getX();  
			 y = frame.getY();
			 break;
		 case 2: // North
			 x = frame.getX() + frame.getWidth()/2; 
			 y = frame.getY();
			 break;
		 case 3: // NorthEast 
			 x = frame.getX() + frame.getWidth(); 
			 y = frame.getY();
			 break;
		 case 4: // East
			 x = frame.getX() + frame.getWidth();  
			 y = frame.getY() + frame.getHeight()/2;
			 break;
		 default: // shouldn't happen
		 case 5: // SouthEast
			 x = frame.getX() + frame.getWidth();  
			 y = frame.getY() + frame.getHeight();
			 break;
		 case 6: // South
			 x = frame.getX() + frame.getWidth()/2; 
			 y = frame.getY() + frame.getHeight();
			 break;
		 case 7: // SouthWest
			 x = frame.getX();
			 y = frame.getY() + frame.getHeight();
			 break;
		 case 8: // West
			 x = frame.getX();
			 y = frame.getY() + frame.getHeight()/2;
			 break;
	    }
	    return new Point2D.Double(x, y);
	 }

        // Get the cursor to use for given handle index. 
        int getHandleCursor(int nHandle) {
	    switch (nHandle) {
		 case 1: return Cursor.NW_RESIZE_CURSOR;
		 case 2: return Cursor.N_RESIZE_CURSOR;
		 case 3: return Cursor.NE_RESIZE_CURSOR;
		 case 4: return Cursor.E_RESIZE_CURSOR;
		 case 5: return Cursor.SE_RESIZE_CURSOR;
		 case 6: return Cursor.S_RESIZE_CURSOR;
		 case 7: return Cursor.SW_RESIZE_CURSOR;
		 case 8: return Cursor.W_RESIZE_CURSOR;
		 // shouldn't happen:
		 default: return Cursor.DEFAULT_CURSOR;
	    }
	 }

	 // return the rectangle for handle at the given point
	 // default is to center handle on the point
	 Rectangle2D.Double getHandleRect(Point2D p)
	 {
		 return new Rectangle2D.Double(p.getX() - HANDLE_W/2, p.getY() - HANDLE_W/2,
				               HANDLE_W, HANDLE_W);
	 }	
	
       /* 
	* Rectangle handling might use utiltity class like this one to form
        * normalized points after user drags handle to form inverted rect.
	* However, there is still the problem that current handle code then changes 
	* meaning during the crossover -- West may become East, etc. The workbench
	* used a different method: allowed non-normalized position rects w/handle
	* ids that were oriented around the rect.
	*
	class NormalizedRect {
	    double left; double top; double bottom; double right;
	    NormalizedRect(double x1, double y1, double x2, double y2) {
		// left must be <= right
		if (x1 <= x2) {
		    left = x1; right = x2;
		} else { 
		    left = x2; right = x1; 
		}
		// y grows downward, so top should be <= bottom
		if (y1 <= y2) {
		    top = y1; bottom = y2;
		} else { 
		    top = y2; bottom = y1; 
		}
	    }
	}
        */

	// move specified resize handle to given point
	void MoveHandleTo(int nHandle, int x, int y) {
	    Rectangle2D old = ((RectangularShape) shape).getFrame();
	    double x1 = old.getX();
	    double y1 = old.getY();
	    double x2 = x1 + old.getWidth();
	    double y2 = y1 + old.getHeight();
	    switch (nHandle) {
		 case 1: // NorthWest corner
			 x1 = x;
			 y1 = y;
			 break;
		 case 2: // North
			 y1 = y;
			 break;
		 case 3: // NorthEast 
			 y1 = y;
			 x2 = x;
			 break;
		 case 4: // East
			 x2 = x; 
			 break;
		 default:
		 case 5: // SouthEast
			 x2 = x;
			 y2 = y;
			 break;
		 case 6: // South
			 y2 = y;
			 break;
		 case 7: // SouthWest
			 y2 = y;
			 x1 = x;
			 break;
		 case 8: // West
			 x1 = x;
			 break;
	  }
	  // build array and normalize points
	  Drawing.msg.setText("Frame x1:" + x1 + " y1: " + y1 + " x2: " + x2 + " y2: " + y2);
	  // Folllowing doesn't solve the full problem, see comment above
	  //NormalizedRect r = new NormalizedRect(x1, y1, x2, y2);
	  // ((RectangularShape) shape).setFrameFromDiagonal(r.left, r.top, r.right, r.bottom);
	  ((RectangularShape) shape).setFrameFromDiagonal(x1, y1, x2, y2);
	}
	
	// test if mouse hits resize handle. 
	// Returns positive handle index if hit, 0 if none.
	int getHitHandle (int x, int y) {
	    for (int i = 1; i <= this.getHandleCount(); i++) {
		    Rectangle2D.Double handle = this.getHandleRect(this.getHandlePoint(i));
		    if (handle.contains(x,y))
			return i;
	    }
	    return 0;
	}

	// for dealing with labels: default is none
        public DrawLabel CreateLabel() { return null; }

        // Might need this for double-click handling:
        // void onDoubleClick() { }
     }

     // Classes of objects defined by bounding rectangle. Their shapes will 
     // be subclasses of RectangularShape, so they can use the default DrawObj implementation.
     class DrawRect extends DrawObj {
          public DrawRect(int x, int y) {
	    shape = new Rectangle2D.Double(x, y, 0, 0);
	 }
     }
     class DrawEllipse extends DrawObj {
	  public DrawEllipse(int x, int y) {
	    shape = new Ellipse2D.Double(x, y, 0, 0);
	  }
     }
     class DrawRoundRectangle extends DrawObj {
	  public DrawRoundRectangle(int x, int y) {
	    shape = new RoundRectangle2D.Double(x, y, 0, 0, /* corner arc w, h:*/ 45, 45);
	  }
     }

     // line objects are defined by corners of a bounding rectangle, but 
     // can't inherit other DrawObj implementation.
     class DrawLine extends DrawObj {
	 public DrawLine(int x, int y) {
		 shape = new Line2D.Double(x, y, x, y);
	 }

	 public void Draw(Graphics g) {
	     // default shape rendering is OK for basic line
	     super.Draw(g);
             // show angle if resizing
	     if (resizing) 
		drawLineAngle(g);
	 }

	 // Must override because Line2D is not a RectangularShape
	 void ResizeTo(int x, int y) {
		 Point2D P1 = ((Line2D)shape).getP1();
		 ((Line2D)shape).setLine(P1, new Point2D.Double(x, y));
	}
        void MoveBy(int delta_x, int delta_y) {
		// update position.
		 Line2D l = (Line2D)shape;
		 l.setLine(l.getX1() + delta_x, l.getY1() + delta_y, 
		           l.getX2() + delta_x, l.getY2() + delta_y);
		
	}
	boolean Hit (int x0, int y0) {
               Line2D l = (Line2D)shape;
               return l.ptSegDist(x0, y0) <= getLineWidth()/2;
	}
	int getHandleCount() { return 2; }
	Point2D getHandlePoint(int i) 
	{
	    switch (i) {
		 default:
		 case 1: return ((Line2D)shape).getP1();
		 case 2: return ((Line2D)shape).getP2();
	    }
	}
        int getHandleCursor(int nHandle) {
	    switch (nHandle) {
		 case 1: return Cursor.NW_RESIZE_CURSOR;
		 default:
		 case 2: return Cursor.SE_RESIZE_CURSOR;
	    }
	}
	void MoveHandleTo(int nHandle, int x, int y)
	{
                Line2D l = (Line2D)shape;
		Point2D newPt = new Point2D.Double(x, y);
		switch (nHandle) {
		default:
		case 1: l.setLine(newPt, l.getP2());
		        break;
		case 2: l.setLine(l.getP1(), newPt);
			break;
		}
	}
	 
	// Functions for dealing with line directions. Some of these could be moved into
	// utility classes.
	
	// get direction in radians in the Java graphics coordinate system
	// Because y grows downward, this is a clockwise angle measure from horizontal
	public double getDirection()
	{
        	Line2D l = (Line2D)shape;
		return Math.atan2(l.getX2() - l.getX1(), l.getY2() - l.getY1());
	}
	// convert Java coord direction to standard Cartesian
	public double toCartesian(double javaDir) { 
		double dir = javaDir - Math.PI/2.0; 
		if (dir < 0) dir += Math.PI*2.0;
		return dir;
	}
	// projection utilities for drawing
        public int yCor(int len, double dir) {return (int)(len * Math.cos(dir));}
        public int xCor(int len, double dir) {return (int)(len * Math.sin(dir));}

	// draw line angle offset from head of line
        public void drawLineAngle (Graphics g) {
		final int OFFSET = 20;	// radial offset of degree string from head
		final String DEGREE_SIGN = "\u00B0";

	        Line2D l = (Line2D)shape;
		double dir = getDirection();
		long dirDegrees = Math.round(Math.toDegrees(toCartesian(dir)));
		// place midpoint of string radially outward from head
                int cxDeg = (int) (l.getX2() + xCor(OFFSET, dir));
	        int cyDeg = (int) (l.getY2() + yCor(OFFSET, dir));
                // get baseline coords from value text midpoint. Measure 
		// without degree sign because it is so light weight.
		String degreeStr = "" + dirDegrees;
                FontMetrics fm = g.getFontMetrics ();
		int xDeg = cxDeg - fm.stringWidth(degreeStr)/2;
		int yDeg = cyDeg + fm.getAscent()/2;
		// for debugging: show line to text mid point
		// g.drawLine((int)l.getX2(), (int) l.getY2(), cxDeg, cyDeg);
		g.drawString(degreeStr + DEGREE_SIGN, xDeg, yDeg);
	}
     }
    
      // Drawrrrow stores two endpoints in its defining shape, like a line,  so can inherit 
      // some stuff from lines for selecting moving, resizing and hit testing. But the defining 
      // shape is not actually what is rendered.  
      class DrawArrow extends DrawLine {
	      DrawLabel label;

              public DrawArrow(int x, int y) { super(x,y); }
	      public void Draw(Graphics g) {
                     Line2D l = (Line2D)shape;
		     drawArrow((Graphics2D) g, 
		               (int) l.getX1(), (int) l.getY1(), (int) l.getX2(), (int) l.getY2(),
			       getLineWidth());
		     // while sizing, display degree measure
		     if (resizing) 
			 drawLineAngle(g);
	      }

              // following arrow-drawing function found in a Java forum
	      public void drawArrow(Graphics2D g2d, int xCenter, int yCenter, int x, int y, float stroke) {
	      	        double aDir=Math.atan2(xCenter-x,yCenter-y);
      			Polygon tmpPoly=new Polygon();
      			int i1=12+(int)(stroke*2);
      			int i2=6+(int)stroke;	// make the arrow head the same size regardless of the length length 
      			tmpPoly.addPoint(x,y);							// arrow tip
      			tmpPoly.addPoint(x+xCor(i1,aDir+.5),y+yCor(i1,aDir+.5));
      			tmpPoly.addPoint(x+xCor(i2,aDir),y+yCor(i2,aDir));
      			tmpPoly.addPoint(x+xCor(i1,aDir-.5),y+yCor(i1,aDir-.5));
      			tmpPoly.addPoint(x,y);							// arrow tip
      			// draw the shaft, shortening line to base of arrowhead
      			Stroke oldStroke = g2d.getStroke();
      			g2d.setStroke(new BasicStroke(stroke));
      			g2d.drawLine(xCenter,yCenter, x+xCor(i2,aDir), y+yCor(i2,aDir));
      			// draw the arrowhead
      			g2d.setStroke(new BasicStroke(1f));// ensure arrow head solid 
      			g2d.drawPolygon(tmpPoly);
      			g2d.fillPolygon(tmpPoly);		// remove this line to leave arrow head unpainted
      			g2d.setStroke(oldStroke);
      		}

             // create the label object for the arrow
      	     public DrawLabel CreateLabel()
	     {
	       // find midpoint of arrow
	       Line2D l = (Line2D)shape;
	       int xMid = (int) (l.getX1() + (l.getX2() - l.getX1())/2);
	       int yMid = (int) (l.getY1() + (l.getY2() - l.getY1())/2);
	       
               label = new DrawLabel(xMid, yMid);
	       label.owner = this;
	       return label;
             }

	     public void MoveBy(int delta_x, int delta_y)
	     {
		   super.MoveBy(delta_x, delta_y);
                   if (label != null)
			   label.MoveBy(delta_x, delta_y);
	     }
     }


     // DrawDot draws a labelled dot (circle) Normally used to represent a body
     // can be moved but not resized.
     class DrawDot extends DrawEllipse {
	    public DrawDot(int x, int y) {
		super(x, y);
		// might want to create circle
	    }

	    public Color getFillColor() { return Color.black; }

	    // might want to lock this as circle when resizing, rather than allow ellipse
	    
	    // create label for dot 
	    public DrawLabel CreateLabel()
	    {
	       final int TOP_OFFSET = 20;
	       Rectangle2D r = ((RectangularShape) shape).getFrame();
	       int xMid = (int) (r.getX() + r.getWidth()/2);
	       int yMid = (int) (r.getY() + r.getHeight() + TOP_OFFSET);
	       
               label = new DrawLabel(xMid, yMid);
	       label.owner = this;
	       return label;
             }
     }

      // DrawAxes stores two endpoints in its defining shape, like a line,  
      // But the line shape is not actually what is rendered. The defining
      // line is in fact the positive x axis only.
     class DrawAxes extends DrawLine {
             public DrawAxes(int x, int y) { super(x,y); }
	     // Axis lines, computed as side effect of drawing
	     Line2D.Double xAxis;
	     Line2D.Double yAxis;

	     public void Draw(Graphics g) {
		     Graphics2D g2d = (Graphics2D) g;
                     Line2D l = (Line2D)shape;
		     Point2D o = l.getP1();   // origin
		     Point2D xPos = l.getP2();  // positive x axis
		     double dx = xPos.getX() - o.getX();
		     double dy = xPos.getY() - o.getY();
		     Point2D.Double xNeg = new Point2D.Double(o.getX() - dx, o.getY() - dy);
		     Point2D.Double yPos = new Point2D.Double(o.getX() - dy, o.getY() + dx);
		     Point2D.Double yNeg = new Point2D.Double(o.getX() + dy, o.getY() - dx);

		     xAxis = new Line2D.Double(xNeg, xPos);
		     yAxis = new Line2D.Double(yNeg, yPos);
                     Stroke oldStroke = g2d.getStroke();
                     g2d.setStroke(new BasicStroke(this.getLineWidth()));
                     g2d.draw(xAxis);
		     g2d.draw(yAxis);
                     g2d.setStroke(oldStroke);
                     // show angle if resizing
		     if (resizing) 
			 drawLineAngle(g);
	     }
	     public boolean Hit (int x, int y)
	     {
		     return xAxis.ptSegDist(x, y) <= getLineWidth()/2 || 
			    yAxis.ptSegDist(x, y) <= getLineWidth()/2 ;
	     }
     }

     // Text piece. Shape is rectangular bounds marker. We inherit a bit
     // from DrawRect for resizing bounds. When activated, we
     // dynamically create a TextField component on top of it to edit the text.
     // When inactive, we just draw the text
     class DrawText extends DrawRect {
	   String text = ""; 	       // text contents
           JTextField inplace;         // text field for editing in place
	   Font font;		       // font to use for text
	   boolean center = false;

           public DrawText(int x, int y) { 
		   super(x,y); 
		   // default font for text
		   // font = new Font ("Helvetica", Font.PLAIN, 14);
	   }
		   
	   public Color getBackgroundColor() { return Color.white; }

	   // Default handle drawing centers resize handles on the handle position
	   // and rectangular objects places handle positions at the corners of the object's
	   // defining rectangle. To get handles entirely outside the TextField component
	   // used when in-place editing, we we definine the object's bounding box to have 
	   // a border of of HANDLE_W/2 on each side of the edit box. To match drawing
	   // when not in-place editing also have to take account of the internal top and 
	   // bottom margins around string used within the edit control. Haven't found 
	   // an API to get these. Value of HANDLE_W/2 seems to work OK for top internal margin.
	   //
	   //  +-------+ handle
	   //  |       |
	   //  |   +------------------------- text drawobj bounding box
	   //  |   |   |
	   //  +---+-- +--------------------- textfield bounding box 
	   //      |   |                      [top margin inside textfield]
	   //      |   +===================== string bounding box
	   //      |   |
	   //      |   |..................... text baseline
	   //      |   +===================== string bounding box
	   //      |   |                      [bottom margin inside textfield]
	   //      |   +--------------------- textfield bounding box
	   //      |
	   //      +------------------------- text drawobj bounding box
	   //
	   // This is kind of hairy and results in a relatively large boundary around string. 
	   // Maybe better to override the to use textfield bounds as drawobj bounds and
	   // override handle methods to get handles outside.
	   public void Draw (Graphics g) {
		   // no-op if active for in-place editing
		   if (isActive()) return;
		   
		   Graphics2D g2d = (Graphics2D) g;
		   // draw the text
                   // g2d.setFont (font);
                   FontMetrics fm = g2d.getFontMetrics ();
		   
		   // editing may have made text exceed user-drawn boundary box.
		   // widen box if needed as a side effect of drawing here. 
		   // Note we don't shrink box to fit if smaller.
		   RectangularShape r = (RectangularShape) shape;
		   int textWidth = fm.stringWidth(text);
		   if (r.getWidth() < textWidth + HANDLE_W) {
		           // change box width to include text + half handle margins
			   if (! center)
			       r.setFrame(r.getX(), r.getY(), textWidth + HANDLE_W, r.getHeight());
			   else { // adjust both left and right edges to preserve center
			       int xMidPoint = (int) (r.getX() + r.getWidth()/2);
			       r.setFrame(xMidPoint - (textWidth/2 + HANDLE_W/2), r.getY(),
					  textWidth + HANDLE_W, r.getHeight());
			   }
		   }
		   // limit bounding box height to one lineheight plus border
		   // Border on top and bottom is HANDLE_W/2 + extra border approximately matching 
		   // margin around string within a text field, so that base line stays in about the
		   // same place when text field is created. HANDLE_W/2 works OK for that extra,
		   // so total is HANDLE_W on top and bottom.
		   if (r.getHeight() != (fm.getHeight() + 2*HANDLE_W)) {
		           // change height only
			   r.setFrame(r.getX(), r.getY(), r.getWidth(), fm.getHeight()+2*HANDLE_W);
		   }

		   // get bounds around the string itself, exclusive of margins
		   int xStr = center ? (int) ((r.getX() + r.getWidth()/2) - textWidth/2)
			             : (int) (r.getX() + HANDLE_W/2);  // left edge of string
                   Rectangle2D.Double strBounds = new Rectangle2D.Double(xStr, r.getY() + HANDLE_W, textWidth, fm.getHeight());
		   //System.out.println("Str bounds x1=" + strBounds.getX() + " y=" + strBounds.getY() + " W=" + strBounds.getWidth() + " H=" + strBounds.getHeight());
		   // fill background behind text to effect opaque drawing
		   g2d.setPaint(getBackgroundColor());
		   g2d.fill(strBounds);
                   g2d.setPaint(Color.black);

		   double yBaseline = r.getY() + HANDLE_W + fm.getAscent();
		   g2d.drawString(text, xStr,  (int) yBaseline);
		   
                   // temporary, for debugging
		   // show string bounding box we are assuming 
		   g2d.setPaint(Color.RED);
		   g2d.draw(strBounds);
		   g2d.setPaint(Color.BLACK);
		   // show baseline
		   //g2d.setPaint(Color.BLACK);
		   //g2d.drawLine(xStr , (int) yBaseline, xStr + textWidth, (int) yBaseline);
	   }
	   public void DrawSelectBorder (Graphics g) {
		   Graphics2D g2d = (Graphics2D) g;

		   // draw border box
		   RectangularShape r = (RectangularShape) shape;
                   Stroke oldStroke = g2d.getStroke();
		   if (isActive()) {
		        // different border style when active
		   	g2d.setStroke(new BasicStroke(HANDLE_W));
		   	g2d.setPaint(Color.LIGHT_GRAY);
		   } else {
			g2d.setPaint(Color.BLUE);
		   }
                   g2d.draw(r);
                   g2d.setStroke(oldStroke);
		   // parent can draw resize handles as usual
		   super.DrawSelectBorder(g);
	   }
	   // activating means creating in-place edit control
	   public void activate (DrawPanel d)
	   {
		// create in-place edit control
      		inplace = new JTextField(text);
      		d.add(inplace);
		// set its bounds to underlying text object
		syncInPlace();
		// On enter, send "SubmitText" command to container
		inplace.setActionCommand("SubmitText");
		inplace.addActionListener(d);
		// Focus it
		inplace.requestFocusInWindow();
      		//inplace.invalidate();
      		//d.revalidate();
      		d.repaint();
	   }
	   public boolean isActive() { return inplace != null; }
	   public void deActivate(DrawPanel d)
	   {
		// System.out.println ("deActivate called\n");
		if (! isActive()) return;

		// System.out.println ("deActivate: removing inplace edit\n");
		text = inplace.getText();
		d.remove(inplace);
		inplace = null;
		// TODO: after editing, size text box to fit text exactly
		d.repaint();
	   }

	   // Propagate bounding box size/position changes to any inplace edit control
	   // Note that because text scrolls in box, entered text may come to overflow 
	   // box bounds after either box resize or text change. 
	   private void syncInPlace()
	   {
		// set its bounds to that of underlying text object, inset by half-handle
		if (isActive()) {
                	RectangularShape r = (RectangularShape) shape;
      			inplace.setBounds((int)(r.getX() + HANDLE_W/2), (int)(r.getY() + HANDLE_W/2), 
					   (int)(r.getWidth() - HANDLE_W), (int)(r.getHeight() - HANDLE_W));
		}
           }
	   void ResizeTo(int x, int y) {
		   super.ResizeTo(x, y);
		   syncInPlace();
	    }
           void MoveBy(int delta_x, int delta_y) {
		   super.MoveBy(delta_x, delta_y);
		   syncInPlace();
           }
	   void MoveHandleTo(int nHandle, int x, int y)
	   {
		   super.MoveHandleTo(nHandle, x, y);
		   syncInPlace();
	   }
     }   

     //
     // DrawLabel -- subclass of Text object used for object labels. 
     //
     // A label is a text piece that is linked with some other object
     // Unlike regular text it is centered in its box. 
     // It may be in-place editable
     //
     class DrawLabel extends DrawText {
	    DrawObj owner;       // parent object that owns this label, if any
	    static final int DEFAULT_W = 80;
	    static final int DEFAULT_H = 30;

	    DrawLabel(int cx, int cy) {
		    super(cx - DEFAULT_W/2, cy - DEFAULT_H/2);
		    center = true;
		    //System.out.println("Created a DrawLabel at x=" + cx + " y=" + cy);
		    text = "label";
		    // cx and cy give midpoint of the label bounding box
		    // create with default width and height. Should use FontMetrics
		    // adjust size as if by stretching southeast handle
		    MoveHandleTo(5, cx + DEFAULT_W/2, cy + DEFAULT_H/2);
                    RectangularShape r = (RectangularShape) shape;
		    System.out.println("Label bounds x1=" + r.getX() + " y=" + r.getY() + " W=" + r.getWidth() + " H=" + r.getHeight());
	    }
            public void activate (DrawPanel d)
            {
		 // parent does most of the work
	         super.activate(d);
	         // labels should have center alignment
                 inplace.setHorizontalAlignment(JTextField.CENTER);
            }
     }
