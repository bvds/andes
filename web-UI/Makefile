#
dojo-version=master

install:
	test -d "dojo" || git clone --recursive https://github.com/dojo/dojo.git dojo
	test -d "dijit" || git clone --recursive https://github.com/dojo/dijit.git dijit
	test -d "dojox" || git clone --recursive https://github.com/dojo/dojox.git dojox
	test -d "util" || git clone --recursive https://github.com/dojo/util.git util
	cd dojo; git checkout $(dojo-version)
	cd dijit; git checkout $(dojo-version)
	cd dojox; git checkout $(dojo-version)
	cd util; git checkout $(dojo-version)
	echo "${dojo-version}" > dojo-install-version
	@echo "Andes Javascript build is currently non-functional."
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
