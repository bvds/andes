#
#    Create system-wide lisp libraries.
#

#
# get sbcl and install it.
#
# The binary-release & source-dir variables should be updated
# as new versions of sbcl are released.
#
# If you want to install sbcl in a directory other than /usr/local
# Set environment variable INSTALL_ROOT before compiling the
# sbcl source code.
#
ifeq ($(shell uname),Darwin)
ifeq ($(shell uname -p),powerpc)
  binary-release = sbcl-1.0.22-powerpc-darwin-binary.tar.bz2
endif
ifeq ($(shell uname -p),i386)
  binary-release = sbcl-1.0.29-x86-darwin-binary-r2.tar.bz2
  # work-around for bug in make-config.sh which guesses the wrong platform
  # on OSX 10.4.11, MacBook Pro.
  platform = SBCL_ARCH=x86; export SBCL_ARCH;
endif
ifeq ($(shell uname -p),x86_64)
  binary-release = sbcl-1.0.29-x86_64-darwin-binary-r2.tar.bz2
endif
endif

ifeq ($(shell uname),Linux)
ifeq ($(shell uname -p),i686)
  binary-release = sbcl-1.0.35-x86-linux-binary.tar.bz2 
endif
ifeq ($(shell uname -m),i686)
  binary-release = sbcl-1.0.35-x86-linux-binary.tar.bz2
endif
ifeq ($(shell uname -p),pentium4) # Andrew linux at CMU
  binary-release = sbcl-1.0.35-x86-linux-binary.tar.bz2
endif 
ifeq ($(shell uname -p),x86_64)
  binary-release = sbcl-1.0.29-x86_64-linux-binary-r2.tar.bz2
endif 
ifeq ($(shell uname -m),x86_64)
  binary-release = sbcl-1.0.29-x86_64-linux-binary-r2.tar.bz2
endif
endif
ifndef binary-release
  $(error "Unknown architecture")
endif 
binary-dir = tar jtf $(binary-release)|grep -o -m 1 "^[^/]*"

# RedHat custom wget returns an error if file exists
get-binary:
	-wget -nc http://downloads.sourceforge.net/sbcl/$(binary-release)
	tar jxf $(binary-release)
	cd `$(binary-dir)`; chmod u+x run-sbcl.sh
binary-install:
	@echo "Needs superuser privileges for install into /usr/local"
	cd `$(binary-dir)`; sh install.sh
thread-test:
	cd `$(binary-dir)`; echo '(format t "Threading is~:[ not~;~] supported on binary release.~%" (find :sb-thread *features*)) (quit)' | ./run-sbcl.sh --noinform --noprint

source-dir = sbcl-1.0.37
source-release = $(source-dir)-source.tar.bz2
get-source:
	-wget -nc http://prdownloads.sourceforge.net/sbcl/$(source-release)
	tar jxf $(source-release)
	# enable thread support (for OS X) and :sb-hash-table-debug
	cp customize-target-features.lisp $(source-dir)
	patch -p 1 -d $(source-dir) < backend-parms.lisp-x86-1.0.36-patch 
	patch -p 1 -d $(source-dir) < backend-parms.lisp-x86_64-1.0.36-patch
	patch -p 1 -d $(source-dir) < parms.lisp-1.0.36-patch
ifdef INSTALL_ROOT 
	perl -i -p -e "s%#define SBCL_HOME .*%#define SBCL_HOME \"${INSTALL_ROOT}/lib/sbcl/\"%" $(source-dir)/src/runtime/runtime.c
endif
	$(platform) cd $(source-dir); sh make.sh $(shell echo "../`$(binary-dir)`/run-sbcl.sh")
ifdef INSTALL_ROOT 
	@echo "Install directory ${INSTALL_ROOT}"
endif


ifdef SUDO_USER
username=$(SUDO_USER)
else
username=$(USER)
endif

source-install:
ifdef INSTALL_ROOT
	@echo "Install into ${INSTALL_ROOT}/bin and ${INSTALL_ROOT}/lib/"
	test -d ${INSTALL_ROOT}/bin || mkdir ${INSTALL_ROOT}/bin
	test -d ${INSTALL_ROOT}/lib || mkdir ${INSTALL_ROOT}/lib
	cd $(source-dir); sh install.sh
	@echo "Installing additional libraries in ${INSTALL_ROOT}/lib/sbcl."
	tar xf sbcl-site-libraries.tar -C ${INSTALL_ROOT}/lib/sbcl
else
	@echo "Needs superuser privileges for install into /usr/local"
	cd $(source-dir); sh install.sh
	@echo "Installing additional libraries in /usr/local/lib/sbcl."
	tar xf sbcl-site-libraries.tar -C /usr/local/lib/sbcl
	chown -R $(username) /usr/local/lib/sbcl
	@echo "The user $(username) will be able to compile the libraries." 
endif

#  The following are instructions for creating the various libraries
#  by hand and packaging them up into sbcl-site-libraries.tgz

#  ;; Install the hunchentoot web server http://www.weitz.de/hunchentoot/
#  ;; using asdf-install.  asdf-install broken in that it will try to load
#  ;; packages we said we didn't need, like cl+ssl and its dependent cffi.
#  ;; Start sbcl by typing "sbcl" and run:
#     (require 'asdf-install)
#     (push :hunchentoot-no-ssl *features*) ;we have apache to do this
#     (asdf-install:install 'hunchentoot) ;here it will say package not trusted multiple times
#     (asdf-install:install 'cl-json)
#     (quit)  ; exit the lisp server
#
#  Also, need to fix error handling in usocket (See Bug #1614).
#  Broken in release 0.4.1 but fixed in development version. 
#  Download svn (the actual fix is in r497) and install.
install-usocket:
	svn co svn://common-lisp.net/project/usocket/svn/usocket/trunk usocket-svn
	rm -r -f /usr/local/lib/sbcl/site/usocket-*
	rm -r -f /usr/local/lib/sbcl/site-systems/usocket*
	mv usocket-svn /usr/local/lib/sbcl/site
	(cd /usr/local/lib/sbcl/site-systems; ln -s ../site/usocket*/*.asd .)

create-site-libraries:
	chmod go+xr /usr/local/lib/sbcl/site/md5*
	chmod -R go+r /usr/local/lib/sbcl/site/md5*
	-cd /usr/local/lib/sbcl/site; rm */*.fasl; rm */*/*.fasl
	-cd /usr/local/lib/sbcl/site; rm */*/*.so
	-cd /usr/local/lib/sbcl/site; rm */*/*.dylib
	tar cf sbcl-site-libraries.tar --directory=/usr/local/lib/sbcl/ site site-systems
