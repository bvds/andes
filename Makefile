#
#  Install and update Andes Help server
#
sbclrc:
	test -f ~/.sbclrc || cp lisp-site-install/sbclrc-sample ~/.sbclrc
	echo "(pushnew #P\"`pwd`/\" asdf:*central-registry*)" >> ~/.sbclrc

install-site-libraries:
	cd lisp-site-install; $(MAKE) install-site-libraries

install-database:
	@echo "This will destroy any existing database!"
	@echo "Enter mysql password"
	cd LogProcessing/databaseCreationScripts; mysql -u root -p < AndesDatabaseCreationSQL.sql

install-dojo:
	cd web-UI; $(MAKE) install

install-solver:
	cd Algebra/src; $(MAKE) executable

ifeq ($(shell uname),Darwin)
  httpd-document-root = /Library/webServer/Documents
  httpd-conf-dir = /etc/$(if $(shell test -d /etc/apache2 && echo 1),apache2,httpd)/users
else
ifeq ($(shell uname),Linux)
  httpd-document-root = /var/www/html
  httpd-conf-dir = /etc/$(if $(shell test -d /etc/apache2 && echo 1),apache2,httpd)/conf.d
else
  $(error "Unknown operating system")
endif
endif
conf-file = $(shell (/usr/sbin/httpd -v | grep Apache/1. >> /dev/null) && echo -1)

configure-httpd:
	@echo "Please run with superuser privileges."
	cp andes-server$(conf-file).conf $(httpd-conf-dir)
	ln -s `pwd`/web-UI $(httpd-document-root)
	ln -s `pwd`/review $(httpd-document-root)
	ln -s `pwd`/images $(httpd-document-root)
	ln -s `pwd`/LogProcessing/Web-Interface $(httpd-document-root)/log

update:
	git pull
	cd problems; git pull
	cd solutions; git pull
	cd Algebra/src; $(MAKE) executable
	cd web-UI; $(MAKE) update

