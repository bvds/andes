dojo.provide("andes.profile");

dojo.require("dojo.parser");
dojo.require("dijit.layout.BorderContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("andes.widget.ExpandoPane");
dojo.require("andes.widget.UpgradeBar");

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

dojo.require("drawing.Drawing");
dojo.require("drawing.plugins.drawing.Grid");
dojo.require("drawing.Toolbar");
dojo.require("drawing.tools.custom.Vector");
dojo.require("drawing.tools.custom.Axes");
dojo.require("drawing.tools.custom.Equation");
dojo.require("drawing.tools.Arrow");
dojo.require("drawing.plugins.tools.Pan");
dojo.require("drawing.plugins.tools.Zoom");

// needs to be loaded after Drawing:
dojo.require("andes.Combo");
dojo.require("andes.positioning");	