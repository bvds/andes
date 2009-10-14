dojo.provide("andes.profile");

dojo.require("dojo.parser");
dojo.require("andes.ContentPane");
dojo.require("dijit.layout.BorderContainer");
//dojo.require("dijit.layout.ContentPane");
dojo.require("andes.widget.ExpandoPane");
dojo.require("dojox.widget.UpgradeBar");

dojo.require("dijit.MenuBar");
dojo.require("dijit.PopupMenuBarItem");
dojo.require("dijit.Menu");
dojo.require("dijit.MenuItem");
dojo.require("dijit.PopupMenuItem");
dojo.require("dijit.form.Button");
dojo.require("dijit.form.TextBox");

// needs to be loaded before Drawing:
dojo.require("andes.defaults");
dojo.require("andes.main");
dojo.require("andes.draw");
dojo.require("dojox.drawing");

//Necessary if bypassing dojox.drawing.
//In that case remove andes.draw from above.
/* Extras to bypass dojox.drawing----
dojo.require("andes.draw");

dojo.require("dojox.gfx");
dojo.require("dojox.drawing.Drawing");
dojo.require("dojox.drawing.util.oo");
dojo.require("dojox.drawing.util.common");
dojo.require("dojox.drawing.defaults");
dojo.require("dojox.drawing.manager.Canvas");

//interactive managers
dojo.require("dojox.drawing.manager.Undo");
dojo.require("dojox.drawing.manager.keys");
dojo.require("dojox.drawing.manager.Mouse");
dojo.require("dojox.drawing.manager.Stencil");
dojo.require("dojox.drawing.manager.StencilUI"); // plugin? or as a require? good here? in toolbar?
dojo.require("dojox.drawing.manager.Anchors");

// standard stencils
dojo.require("dojox.drawing.stencil._Base");
dojo.require("dojox.drawing.stencil.Line");
dojo.require("dojox.drawing.stencil.Rect");
dojo.require("dojox.drawing.stencil.Ellipse");
dojo.require("dojox.drawing.stencil.Path");
dojo.require("dojox.drawing.stencil.Text");
dojo.require("dojox.drawing.stencil.Image");
*/
// Previous includes in Drawing no longer in Dojox
dojo.require("dojox.drawing.plugins.drawing.Silverlight");

dojo.require("dojox.drawing.tools.TextBlock");
dojo.require("dojox.drawing.tools.Rect");
dojo.require("dojox.drawing.tools.Ellipse");
dojo.require("andes.drawing.Line");
dojo.require("dojox.drawing.tools.Path");
dojo.require("dojox.drawing.tools.Pencil");

//Necessary if bypassing dojox.drawing
/*
dojo.require("dojox.drawing.annotations.Label");
dojo.require("dojox.drawing.annotations.Angle");
dojo.require("dojox.drawing.annotations.Arrow");
dojo.require("dojox.drawing.annotations.BoxShadow");
*/

dojo.require("andes.drawing.Vector");
dojo.require("dojox.drawing.tools.custom.Equation");
dojo.require("dojox.drawing.tools.custom.Axes");
dojo.require("dojox.drawing.tools.Arrow");

dojo.require("andes.toolbar.Pan");
dojo.require("andes.toolbar.Zoom");
dojo.require("dojox.drawing.plugins.drawing.Grid");

dojo.require("andes.toolbar.Toolbar");

//Use these as soon as the programmatic toolbar is fixed
/*
dojo.require("dojox.drawing.plugins.tools.Pan");
dojo.require("dojox.drawing.plugins.tools.Zoom");
dojo.require("dojox.drawing.ui.Button");
dojo.require("dojox.drawing.ui.Toolbar");
dojo.require("dojox.drawing.library.icons");
*/
 
// needs to be loaded after Drawing:
dojo.require("andes.Combo");
dojo.require("andes.positioning");