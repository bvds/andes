#
#  There may be some existing files in the git repostory;
#  get these out of the way, first.  Then restore them after.
#  Could use --force if you have svn 1.4 or newer.
#
dojo-version=trunk
# Work-around for Bug #1886
#   dojo-svn=-r 24327 http://svn.dojotoolkit.org/src
# But try the latest version:
dojo-svn=http://svn.dojotoolkit.org/src

install:
	-rm -r -f dojo dijit dojox util
	svn co $(dojo-svn)/dojo/trunk dojo
	svn co $(dojo-svn)/dijit/trunk dijit
	svn co $(dojo-svn)/dojox/trunk dojox
	svn co $(dojo-svn)/util/trunk util
	# Development branch can over-write dojo libraries.
	-git checkout dojo 
	-git checkout dojox 
	-git checkout dijit 
	-git checkout util
	# In case the build fails, we tag version first.
	echo "${dojo-version}" > dojo-install-version   
	./andes.build.sh
	@echo "Installed Dojo $(dojo-version)."

update:
ifeq ($(shell test -f dojo-install-version && cat dojo-install-version),${dojo-version})
	# Work-around build failing, Bug #1886
	#   svn update -r 24082 *
	# Work-around for load failing on trunk, Bug #1957
	   svn update -r 27116 *
	# But try the latest version:
	# svn update *
	./andes.build.sh
	@echo "Updated Dojo $(dojo-version)."
else
	$(MAKE) install
endif
