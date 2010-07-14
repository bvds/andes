ifdef SUDO_USER
username=$(SUDO_USER)
else
username=$(USER)
endif


install-server:
	@echo "Please run with superuser privileges."
	# Don't overwrite any existing file!
	test -f /etc/andes-help.conf || cp andes-help.conf /etc
	echo "LUSER=$(username)" >> /etc/andes-help.conf
	echo "ANDES_SERVER_DIR=`pwd`" >> /etc/andes-help.conf
	@echo "The server will be run under user $(username)." 
ifeq ($(shell uname),Darwin)
	cp -r AndesHelp /Library/StartupItems/
	@echo "Need to test if ANDESHELP is already present in hostconfig"
	echo "ANDESHELP=-YES-" >> /etc/hostconfig
else
ifeq ($(shell uname),Linux)
	cp andes-helpd /etc/init.d
	/sbin/chkconfig --add andes-helpd
	/sbin/chkconfig andes-helpd on
	-rm /var/lock/subsys/andes-helpd
else
	$(error "Unknown operating system")
endif
endif

# Once everyone's database is updated, this can be removed, Bug #1773.
update:
ifeq ($(shell test -f /etc/andes-help.conf && grep -c "\-dynamic-memory" /etc/andes-help.conf),1)
	@echo "**** Error in configuration file /etc/andes-help.conf"
	@echo "**** Replace '--dynamic-memory' with '--dynamic-space-size'"
	@test #abort make
endif
