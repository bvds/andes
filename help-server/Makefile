ifdef SUDO_USER
username=$(SUDO_USER)
else
username=$(USER)
endif

sysfile = andes-helpd.service
sysdir = /etc/systemd/system

install-server:
	@echo "Please run with superuser privileges."
ifeq ($(shell test -d /etc/systemd/system && echo 1),1)
	@echo "Using service to systemd"
	cp -n $(sysfile) $(sysdir)
	sed -i -r -- "s|<--LUSER-->|$(username)|" $(sysdir)/$(sysfile)
	sed -i -r -- "s|<--SERVER_DIR-->|`pwd`|" $(sysdir)/$(sysfile)
	systemctl daemon-reload
	systemctl enable andes-helpd
	systemctl start andes-helpd
else   
ifeq ($(shell uname),Darwin)
	cp -n -r AndesHelp /Library/StartupItems/
	@echo "Need to test if ANDESHELP is already present in hostconfig"
	echo "ANDESHELP=-YES-" >> /etc/hostconfig
else
ifeq ($(shell uname),Linux)
	@echo "Adding service to /etc/init.d:"
	cp -n andes-helpd /etc/init.d
	sed -i -r -- "s|<--LUSER-->|$(username)|" /etc/init.d/andes-helpd
	sed -i -r -- "s|<--SERVER_DIR-->|`pwd`|" /etc/init.d/andes-helpd
	/sbin/chkconfig --add andes-helpd
	/sbin/chkconfig andes-helpd on
	-rm /var/lock/subsys/andes-helpd
else
	$(error "Unknown operating system")
endif
endif
endif
	@echo "The server will be run under user $(username)." 

update:
