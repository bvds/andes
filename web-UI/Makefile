#
#  There may be some existing files in the git repostory;
#  get these out of the way, first.  Then restore them after.
#  Could use --force if you have svn 1.4 or newer.
#
#  We use the latest release of dojo for everything but dojox/drawing
#  This is because dojox/drawing has some important changes that
#  are not in the latest release.  Eventually, we will just use
#  the release version.

dojo-version=1.5
dojo-svn=http://svn.dojotoolkit.org/src/branches/$(dojo-version)

install:
	-rm -r -f dojo dijit dojox util
	svn co $(dojo-svn)/dojo dojo
	svn co $(dojo-svn)/dijit dijit
	svn co $(dojo-svn)/dojox dojox
	# Use the development version of  dojox/drawing.
	# The command "svn update dojox" will overwrite this change.
	rm -r -f dojox/drawing
	svn co http://svn.dojotoolkit.org/src/dojox/trunk/drawing dojox/drawing
	svn co $(dojo-svn)/util util
	./andes.build.sh
	echo "${dojo-version}" > dojo-install-version	
	@echo "Installed Dojo $(dojo-version); is this the latest?"
	@echo "dojox/drawing is from development version."


#  If the latest version is installed, update, else upgrade to new version
update:
ifeq ($(shell test -f dojo-install-version && cat dojo-install-version),${dojo-version})
	svn update dojo dijit util
	svn update -N dojox
	svn update dojox/*
	./andes.build.sh
	@echo "Updated Dojo $(dojo-version); is this the latest?"
	@echo "dojox/drawing is from development version."
else
	$(MAKE) install
endif
