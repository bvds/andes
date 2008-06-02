/*
 * Drawing.java -- Andes solution drawing editor
 *
 */

// These classes go in an andes-specific namespace. The Java convention 
// is to ensure unique package names by using reverse-order domain name 
// prefixes. The prefix has no other function than to avoid conflicts.
package andes;

import java.awt.*; 
import java.awt.geom.*;
import java.applet.*; 
import java.awt.event.*; 
import java.util.*;
import javax.swing.*;
import java.lang.Math;
import java.lang.System;



/*
 * Drawing -- the main Applet class
 *
 * This is just a frame around a DrawPanel, which does all the work.
 * Can be used as an applet (via init) or application (via main).
 */
public class Drawing extends JApplet
{     
     // statics so DrawPanel can access them
     static protected JLabel msg;
     static protected JButton btnSelect;

     public void init()  
     { 
      // Note Windows look-and-feel odd for buttons w/o pictures
      try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      } catch (Exception ex){ 
	      //ignore; 
      }

      getContentPane().setLayout(new BorderLayout());

      // The drawing panel
      DrawPanel d = new DrawPanel();
      d.setLayout(null);   // so we can position text boxes for editing
      d.setBackground(Color.white);
      getContentPane().add(d);

      // Toolbar with our tools
      JToolBar btnBar = new JToolBar();
      //btnPanel.setBackground(Color.white);
      //btnPanel.setBorder(BorderFactory.createLineBorder(Color.black));
      String[] toolNames = { "Line", "Arrow", "Axes", "Text", "Body",
	                       "Rectangle", "Ellipse", "RoundRectangle", 
	                      "Select", "Delete" };
      JButton b; 
      for (int i = 0; i < toolNames.length; i++) {
      	b = new JButton(toolNames[i]);
	// add toolbar image from the images subdirectory, if one exists
	// Image img = Toolkit.getDefaultToolkit().getImage("images/" + toolNames[i] + ".gif"); 
        java.net.URL imgURL = Drawing.class.getResource("images/" + toolNames[i] + ".gif");
	if (imgURL != null) {
	    b.setIcon(new ImageIcon(imgURL));
	}
        b.setVerticalTextPosition(AbstractButton.BOTTOM);
        b.setHorizontalTextPosition(AbstractButton.CENTER);
        b.addActionListener(d);
        btnBar.add(b);
	// remember the selection button so can set focus to it
	if (toolNames[i].equals("Select"))
		btnSelect = b;
      }
      getContentPane().add("North", btnBar);

      // label used for status/debug messages
      msg = new JLabel();
      msg.setBorder(BorderFactory.createLineBorder(Color.black));
      getContentPane().add("South", msg);
      msg.setText("Ready");
     } 
     
     // main method when used as an application, not an applet
     public static void main(String s[]) 
     {
        JFrame f = new JFrame("Solution Pad");
        f.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {System.exit(0);}
        });
        JApplet applet = new Drawing();
        f.getContentPane().add("Center", applet);
        applet.init();
        f.pack();
        f.setSize(new Dimension(800,600));
        f.setVisible(true);
    } 
}

// 
// DrawPanel: the drawing canvas
//
class DrawPanel extends JPanel implements MouseListener, MouseMotionListener, ActionListener
{    
     protected DrawText activeText;     // unique currently active text object.
     protected RenderingHints renderHints; 

     public DrawPanel() {
        setBackground(Color.white);
        addMouseListener(this);
        addMouseMotionListener(this); 
	// create our graphics rendering hints
	renderHints = new RenderingHints(null);
        renderHints.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
	renderHints.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        renderHints.put(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP);
     }

     // Drawable object list. Uses generics introduced in Java 1.5
     ArrayList<DrawObj> objects = new ArrayList<DrawObj>();

     // The current selected object. !! May want list for multiple selection
     private DrawObj selection;
     public void setSelection(DrawObj obj) { selection = obj; }
     public boolean isSelected(DrawObj obj) { return selection == obj; }
     public DrawObj getSelection() { return selection; }

     // Name of current major mouse mode (drawing tool):
     String curTool = "Line";

     // mode subtype when curTool == "Select"
     final int NOT_TRACKING_SELECTION = 0;  // haven't started move or resize
     final int MOVING_SELECTION = 1;
     final int RESIZING_SELECTION = 2;
     int selectMode = NOT_TRACKING_SELECTION;
     int nDragHandle= 0;    // index of handle being dragged when resizing
     // Following valid while dragging (from mouse down to mouse up)
     int x1, y1; 	// Mouse position at start of drag
     int xPrev, yPrev;  // Mouse position on previous event


     // render the drawing panel
     public void paintComponent(Graphics g)  
     { 
	  super.paintComponent(g);   
	  ((Graphics2D) g).setRenderingHints(renderHints);
	  for (int i = 0; i < objects.size(); i++) {
                DrawObj o = objects.get(i); {
             	o.Draw(g);
		if (isSelected(o))
		    o.DrawSelectBorder(g);
	     }
	  }
     } 

     // Return object hit at x,y; null if none
     DrawObj getHitObject(int x, int y)
     {
 	for (int i = 0; i < objects.size(); i++) {
             DrawObj o = objects.get(i);
	     if (o.Hit(x, y))
		return o;
	}
	return null;
     }

     // handle action events, fired from button presses or our
     // action for submitting text
     public void actionPerformed(ActionEvent evt) {
      String command = evt.getActionCommand();
      if (command.equals("Delete"))
	  doDeleteCmd();
      else if (command.equals("SubmitText")) {
	  // handle submission of text from in-place edit
	  // cancel in-place editing
	  setActiveObject(null);
      }
      else // all other commands just set current tool name
	  curTool = command;
      }

     // helper restores select mode and focuses it
     public void setSelectMode()
     {
         curTool = "Select";
	 // don't focus select button if active edit has focus
	 // !!! Need better way to show currently pressed button than focus.
	 if (activeText == null)
	 	Drawing.btnSelect.requestFocusInWindow();
         selectMode = NOT_TRACKING_SELECTION;
     }

     // Delete currently selected object
     public void doDeleteCmd()
     {
	  if (getSelection() != null) {
		  objects.remove(getSelection());
	          setSelection(null);
		  repaint();
		  // restore select tool
		  setSelectMode();
	  }
     }
     
     // managing the one and only current object active for in-place editing
     // safe to call with null at any time to clear active object
     public void setActiveObject(DrawObj obj)
     {
	  // deactivate any existing object
	  if (activeText != null) {
		  activeText.deActivate(this);
		  repaint();
	  }
	  // activate this one
	  // right now, only text-derivatives can be active
	  if (obj != null) {
              activeText = (DrawText) obj;
	      ((DrawText) obj).activate(this);
	  }
     }

     // Mouse event handling. 
     
     // Mouse click. Click also invokes mousePressed and mouseReleased
     public void mouseClicked (MouseEvent me) { 
	     // see if it's a double-click on an object
	     DrawObj obj;
	     if (me.getClickCount() == 2 &&
		(obj = getHitObject(me.getX(), me.getY())) != null ){
		     // if it's a DrawText object then activate it
		     // !!! probably want a generic object method for handling double-click
		     if (DrawText.class.isInstance(obj)) {
			     setActiveObject(obj);
		
		     }
	     }
     }

     // handle mouse press
     public void mousePressed (MouseEvent me) {
	   // start tracking the current object
           x1 = me.getX(); 
           y1 = me.getY(); 

	   // if mode is drawing tool, create new (possibly degenerate) object 
	   // and add to display list. 
	   if (! curTool.equals("Select")) {
		DrawObj obj = CreateNew(curTool, x1, y1);
	   	objects.add(obj);
           	// Select it so user can drag to desired size.  Note we are not setting 
		// curTool to select/resize for initial sizing, because may want 
		// custom post-tracking processing for initial draw.
	   	setSelection(obj);
		setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
		obj.resizing = true;
		Drawing.msg.setText("Started object type " + curTool + " at " + x1 + "," + y1);
	   } else { // SELECT mode
		// if there's a selection, see if we grabbed a resize handle 
		DrawObj obj = getSelection();
		if (obj != null && 
		    (nDragHandle = obj.getHitHandle(me.getX(), me.getY())) != 0) {
			selectMode = RESIZING_SELECTION;
                        setCursor(new Cursor(obj.getHandleCursor(nDragHandle)));
			obj.resizing = true;
		}
		// else if we hit an object, start a move
		else if ((obj = getHitObject(me.getX(), me.getY())) != null) {
		     	setSelection(obj);
			selectMode = MOVING_SELECTION;
			setCursor(new Cursor(Cursor.MOVE_CURSOR));
			xPrev = me.getX(); yPrev = me.getY();
	     	} 
		else {  // hit nothing: clear selection
		        setSelection(null);
			setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			// cancel any in-place editing
			setActiveObject(null);
			// Could begin net select via dragged rectangle
		}
           }
	   repaint();
     } 

     // Better would be to use newInstance from a class object fetched by name. But can't
     // pass parameters to constructors that way -- need to use reflection APIs for that.
     // Or could just create default object and then use a method to initialize.
     public DrawObj CreateNew(String toolName, int x, int y) {
	     if (toolName.equals("Line"))
		     return new DrawLine(x, y);
	     else if (toolName.equals("Rectangle"))
		     return new DrawRect(x, y);
	     else if (toolName.equals("Text"))
		     return new DrawText(x, y);
	     else if (toolName.equals("Ellipse"))
		     return new DrawEllipse(x, y);
	     else if (toolName.equals("RoundRectangle"))
		     return new DrawRoundRectangle(x, y);
	     else if (toolName.equals("Arrow"))
		     return new DrawArrow(x, y);
	     else if (toolName.equals("Axes"))
		     return new DrawAxes(x, y);
	     else if (toolName.equals("Body"))
                     return new DrawBody(x, y);
	     else {
                     Drawing.msg.setText("CreateNew: unknown tool! : " + toolName);
		     return null;
	     }
     }

     // handle drag of mouse
     public void mouseDragged(MouseEvent me) {
	  if (getSelection() != null) {
	      if (curTool.equals("Select")) {
		  if (selectMode == MOVING_SELECTION) {
		      getSelection().MoveBy(me.getX() - xPrev, me.getY() - yPrev);
	          } else if (selectMode == RESIZING_SELECTION) {
		     // resize the current handle 
              	      getSelection().MoveHandleTo(nDragHandle, me.getX(), me.getY());
	          }
	      } else { // Some drawing tool and have selection
		// Assume initial sizing mode
              	getSelection().ResizeTo(me.getX(), me.getY());
	      }
	      repaint(); 
	      // update after drag
              xPrev = me.getX(); yPrev = me.getY();
	  }
     }; 

     // handle release of mouse
     public void mouseReleased (MouseEvent me) {
	   // cancel any sizing mode display
	   if (selection != null)
		   selection.resizing = false;

	   // certain tools pop up label edits after adding
	   if (curTool == "Text") {
		// on ending text mode, create edit control to
		// make this text object active for inplace editing
		setActiveObject(selection);
	   } else if (curTool == "Arrow" || curTool == "Body") {
		// create the vector's label and activate it for editing
		DrawLabel l =  selection.CreateLabel();
		if (l != null) {
			objects.add(l);
			setActiveObject(l);
		}
	   } 
	  
	   // always return to SELECT tool mode, even if just drew. Keeping tool
	   // can be annoying, especially in Andes.
           // following will grab focus after select button 
	   setSelectMode();
	   setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	   repaint();
     }  

     // process non-drag mouse move only while not dragging to adjust cursor
     public void mouseMoved(MouseEvent me) { 
	    if (curTool.equals("Select") && selectMode == NOT_TRACKING_SELECTION) {
		// if there's a selection, see if we're over a resize handle 
		DrawObj obj = getSelection();
		if (obj != null && 
		    (nDragHandle = obj.getHitHandle(me.getX(), me.getY())) != 0) {
                        setCursor(new Cursor(obj.getHandleCursor(nDragHandle)));
		} 
		else setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	    }

     };

     // MouseListener interface methods which are no-ops for us:
     public void mouseEntered (MouseEvent me) { } 
     public void mouseExited (MouseEvent me) { }  
} 
