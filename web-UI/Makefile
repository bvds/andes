#
#  There may be some existing files in the git repostory;
#  get these out of the way, first.  Then restore them after.
#  Could use --force if you have svn 1.4 or newer.
#
dojo-version=trunk

install:
	-rm -r -f dojo dijit dojox util
	git clone https://github.com/dojo/dojo.git dojo
	git clone https://github.com/dojo/dijit.git dijit
	git clone https://github.com/bvds/dojox.git dojox
	git clone https://github.com/dojo/util.git util
	# Development branch can over-write dojo libraries.
	-git checkout
	-git checkout dojox 
	-git checkout dijit 
	-git checkout util
	# In case the build fails, we tag version first.
	echo "${dojo-version}" > dojo-install-version   
	./andes.build.sh
	@echo "Installed Dojo $(dojo-version)."

update:
ifeq ($(shell test -f dojo-install-version && cat dojo-install-version),${dojo-version})
	cd dojo; git pull
	cd dijit; git pull
	cd dojox; git pull
	cd util; git pull
	./andes.build.sh
	@echo "Updated Dojo $(dojo-version)."
else
	$(MAKE) install
endif
