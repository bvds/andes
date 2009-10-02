dojo.provide("andes.profile");

dojo.require("dojo.parser");
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
dojo.require("dojox.drawing");

// Previous includes in Drawing no longer in Dojox
dojo.require("dojox.drawing.plugins.drawing.Silverlight");

dojo.require("dojox.drawing.tools.TextBlock");
dojo.require("dojox.drawing.tools.Rect");
dojo.require("dojox.drawing.tools.Ellipse");
dojo.require("andes.drawing.Line");
dojo.require("dojox.drawing.tools.Path");
dojo.require("dojox.drawing.tools.Pencil");

//Already required in subfiles, Mike had his here
dojo.require("dojox.drawing.annotations.Label");
dojo.require("dojox.drawing.annotations.Angle");
dojo.require("dojox.drawing.annotations.Arrow");
dojo.require("dojox.drawing.annotations.BoxShadow");

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
dojo.require("andes.ContentPane");