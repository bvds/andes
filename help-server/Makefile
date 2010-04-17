ifdef SUDO_USER
username=$(SUDO_USER)
else
username=$(USER)
endif


install-server:
	@echo "Please run with superuser privileges."
	cp help-server/andes-help.conf /etc
	echo "LUSER=$(username)" >> /etc/andes-help.conf
	echo "ANDES_SERVER_DIR=`pwd`" >> /etc/andes-help.conf
ifeq ($(shell uname),Darwin)
	cp -r AndesHelp /Library/StartupItems/
	@echo "Need to test if ANDESHELP is already present"
	echo "ANDESHELP=-YES-" >> /etc/hostconfig
else
ifeq ($(shell uname),Linux)
	cp help-server/andes-helpd /etc/init.d
else
	$(error "Unknown operating system")
endif
endif
