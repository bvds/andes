#
#  There may be some existing files in the git repostory;
#  get these out of the way, first.  Then restore them after.
#  Could use --force if you have svn 1.4 or newer.
#
install:
	-rm -r -f dojo dijit dojox util
	svn co http://svn.dojotoolkit.org/src/dojo/trunk dojo
	svn co http://svn.dojotoolkit.org/src/dijit/trunk dijit
	svn co http://svn.dojotoolkit.org/src/dojox/trunk dojox
	svn co http://svn.dojotoolkit.org/src/util/trunk util
	-git checkout dojo 
	-git checkout dojox 
	-git checkout dijit 
	-git checkout util
	andes.build.sh

update:
	svn update *
	andes.build.sh
