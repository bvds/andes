#
#  There may be some existing files in the git repostory;
#  get these out of the way, first.  Then restore them after.
#  Could use --force if you have svn 1.4 or newer.
#
dojo-version=trunk
# Work-around for Bug #1886
dojo-svn=-r 24327 http://svn.dojotoolkit.org/src

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
	./andes.build.sh
	echo "${dojo-version}" > dojo-install-version   
	@echo "Installed Dojo $(dojo-version)."

update:
ifeq ($(shell test -f dojo-install-version && cat dojo-install-version),${dojo-version})
	# Work-around for Bug #1886
	svn update -r 24327 *
	./andes.build.sh
	@echo "Updated Dojo $(dojo-version)."
else
	$(MAKE) install
endif
