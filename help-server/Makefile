ifdef SUDO_USER
username=$(SUDO_USER)
else
username=$(USER)
endif


install-server:
	@echo "Please run with superuser privileges."
	cp andes-help.conf /etc
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
