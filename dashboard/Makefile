
install:
ifeq ($(shell uname),Darwin)
	ln -s dashboard/dashboard-UI/ /Library/WebServer/Documents/dashboard
else
ifeq ($(shell uname),Linux)
	@echo "need to do this as root"
	ln -s dashboard/dashboard-UI/ /var/www/html
else
	$(error "Unknown operating system")
endif
endif
