#
#  There may be some existing files in the git repostory;
#  get these out of the way, first.  Then restore them after.
#
dojo-version=pre-amd
# Work-around for Bug #1886:
# Install specific dojo versions

install:
	-rm -r -f dojo dijit dojox util
	git clone https://github.com/dojo/dojo.git dojo
	git clone https://github.com/dojo/dijit.git dijit
	git clone https://github.com/dojo/dojox.git dojox
	git clone https://github.com/dojo/util.git util
	cd dojo; git checkout bbee943c58abaadc94622b0b22c91e83d686c438
	cd dijit; git checkout 52e5a4eeb5861a3bc50073168b1668aad1d8c7ef
	cd dojox; git checkout 2b79ddf64fbc0956bbc3688a656e5d2f858e0bdd
	cd util; git checkout 6a35e2f4f859cc263063dd8132a4175453d813a1 
	# Development branch can over-write dojo libraries.
	# In case the build fails, we tag version first.
	echo "${dojo-version}" > dojo-install-version   
	-git checkout dojox
	# echo "build script fails." 
	./andes.build.sh
	@echo "Installed Dojo $(dojo-version)."

update:
ifeq ($(shell test -f dojo-install-version && cat dojo-install-version),${dojo-version})
	cd dojo; git checkout bbee943c58abaadc94622b0b22c91e83d686c438
	cd dijit; git checkout 52e5a4eeb5861a3bc50073168b1668aad1d8c7ef
	cd dojox; git checkout 2b79ddf64fbc0956bbc3688a656e5d2f858e0bdd
	cd util; git checkout 6a35e2f4f859cc263063dd8132a4175453d813a1 
	./andes.build.sh
	@echo "Updated Dojo $(dojo-version)."
else
	$(MAKE) install
endif
