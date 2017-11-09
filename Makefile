#
#  Install and update Andes Help server
#

install-site-libraries:
	cd lisp-site-install; $(MAKE) install-site-libraries

install-database:
	cd LogProcessing/database; $(MAKE) install 

install-dojo:
	cd web-UI; $(MAKE) install

install-solver:
	cd Algebra/src; $(MAKE) executable

ifeq ($(shell uname),Darwin)
  httpd-document-root = /Library/webServer/Documents
  httpd-conf-dir = /etc/apache2/other
else
ifeq ($(shell uname),Linux)
  # Ubuntu uses /var/www while RedHat uses /var/www/html
  httpd-document-root = /var/www$(shell test -d /var/www/html && echo /html)
  httpd-conf-dir = /etc/$(if $(shell test -d /etc/apache2 && echo 1),apache2,httpd)/conf.d
else
  $(error "Unknown operating system")
endif
endif

configure-httpd:
	@echo "Please run with superuser privileges."
ifeq ($(shell which a2enmod &> /dev/null && echo 1),1)
	@echo "In Ubuntu, configuring the proxy modules:"
	a2enmod proxy
        a2enmod proxy_http
        a2enmod proxy_html
endif
        # Apache 2
	cp andes-server-2.conf $(httpd-conf-dir)
	ln -s `pwd`/web-UI $(httpd-document-root)
	ln -s `pwd`/review $(httpd-document-root)
	ln -s `pwd`/images $(httpd-document-root)
	ln -s `pwd`/LogProcessing/Web-Interface $(httpd-document-root)/log
	apachectl restart

install-server:
	cd help-server; $(MAKE) install-server


update:
	git pull
	cd lisp-site-install; $(MAKE) update
	cd LogProcessing/database; $(MAKE) update
	cd help-server; $(MAKE) update
	cd problems; git pull
	cd solutions; git pull
	cd Algebra/src; $(MAKE) executable
	cd web-UI; $(MAKE) update
