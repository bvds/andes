#!/bin/sh
echo "Building Andes..."
cd util/buildscripts

java  -Xms1024m -Xmx1024m -classpath ../shrinksafe/js.jar:../shrinksafe/shrinksafe.jar org.mozilla.javascript.tools.shell.Main  build.js profileFile=../../profile.andes.js releaseDir=../../release copyTests=false internStrings=true mini=true version=1.0 optimize="comments" cssOptimize=comments.keepLines cssImportIgnore=../../dijit/themes/dijit.css action=clean,release
