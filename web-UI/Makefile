#
#  There may be some existing files in the git repostory;
#  get these out of the way, first.  Then restore them after.
#  Could use --force if you have svn 1.4 or newer.
#
dojo-version=1.4
dojo-svn=http://svn.dojotoolkit.org/src/branches/$(dojo-version)

install:
	-rm -r -f dojo dijit dojox util
	svn co $(dojo-svn)/dojo dojo
	svn co $(dojo-svn)/dijit dijit
	svn co $(dojo-svn)/dojox dojox
	svn co $(dojo-svn)/util util
	-git checkout dojo 
	-git checkout dojox 
	-git checkout dijit 
	-git checkout util
	./andes.build.sh
	@echo "Intalled Dojo $(dojo-version); is this the latest?"	

update:
	svn update dojo dijit dojox util
	./andes.build.sh
