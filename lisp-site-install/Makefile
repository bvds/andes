#
#    Create system-wide lisp libraries.
#

#
# get sbcl and install it.
#
ifeq ($(shell uname),Darwin)
ifeq ($(shell uname -p),powerpc)
  binary-release = sbcl-1.0.22-powerpc-darwin-binary.tar.bz2
endif
ifeq ($(shell uname -p),i386)
  binary-release = sbcl-1.0.29-x86-darwin-binary-r2.tar.bz2
  # work-around for bug in make-config.sh which guesses the wrong platform
  # on OSX 10.4.11, MacBook Pro.
  d-arch = SBCL_ARCH=x86; export SBCL_ARCH;
endif
ifeq ($(shell uname -p),x86_64)
  binary-release = sbcl-1.0.29-x86_64-darwin-binary-r2.tar.bz2
endif
endif
ifeq ($(shell uname),Linux)
ifeq ($(shell uname -p),i686)
  binary-release = sbcl-1.0.35-x86-linux-binary.tar.bz2
endif 
ifeq ($(shell uname -p),x86_64)
  binary-release = sbcl-1.0.29-x86_64-linux-binary-r2.tar.bz2
endif 
endif
ifndef binary-release
  $(error "Unknown architecture")
endif 
binary-dir = tar jtf $(binary-release)|grep -o -m1 "[^/]*"

# RedHat custom wget returns an error if file exists
get-binary:
	-wget -nc http://downloads.sourceforge.net/sbcl/$(binary-release)
	tar jxf $(binary-release)
	cd `$(binary-dir)`; chmod u+x run-sbcl.sh
binary-install:
	@echo "Needs superuser privileges for install into /usr/local"
	cd `$(binary-dir)`; sh install.sh
thread-test:
	cd `$(binary-dir)`; echo '(format t "threading is ~:[not~;~] supported~%" (find :sb-thread *features*)) (quit)' | ./run-sbcl.sh --noinform --noprint

source-dir = sbcl-1.0.35
source-release = $(source-dir)-source.tar.bz2
get-source:
	-wget -nc http://prdownloads.sourceforge.net/sbcl/$(source-release)
	tar jxf $(source-release)
	#cp customize-target-features.lisp $(source-dir) #thread support
	cd $(source-dir); $(d-arch) sh make.sh $(shell echo "../`$(binary-dir)`/run-sbcl.sh")
source-install:
	@echo "Needs superuser privileges for install into /usr/local"
	cd $(source-dir); sh install.sh

#  The following are instructions for creating the various libraries
#  by hand and packaging them up into sbcl-site-libraries.tgz
#  Normally, users will just want to invoke "install-site-libraries"

#  ;; Install the hunchentoot web server http://www.weitz.de/hunchentoot/
#  ;; using asdf-install.  asdf-install broken in that it will try to load
#  ;; packages we said we didn't need, like cl+ssl and its dependent cffi.
#  ;; Start sbcl by typing "sbcl" and run:
#     (require 'asdf-install)
#     (push :hunchentoot-no-ssl *features*) ;we have apache to do this
#     (asdf-install:install 'hunchentoot) ;here it will say package not trusted multiple times
#     (asdf-install:install 'cl-json)
#     (asdf-install:install 'uffi)  ;prereq for clsql, but doesn't load
#     (asdf-install:install 'clsql)
#     (quit)  ; exit the lisp server


#  clsql doesn't link with 64 bit system libraries.  This is a
#  patch to fix this.
clsql-v = 4.3.0
patch-clsql:
	patch -p0 -d /usr/local/lib/sbcl/site < clsql-$(clsql-v).patch

create-site-libraries:
	chmod go+xr /usr/local/lib/sbcl/site/md5*
	chmod -R go+r /usr/local/lib/sbcl/site/md5*
	-cd /usr/local/lib/sbcl/site; rm */*.fasl; rm */*/*.fasl
	-cd /usr/local/lib/sbcl/site; rm */*/*.so
	-cd /usr/local/lib/sbcl/site; rm */*/*.dylib
	tar cf sbcl-site-libraries.tar --directory=/usr/local/lib/sbcl/ site site-systems

#
#  Install sbcl site libraries from copy in git.
#
install-site-libraries:
	@echo "Needs superuser privileges for install into /usr/local/lib/sbcl"
	tar xf sbcl-site-libraries.tar -C /usr/local/lib/sbcl
	chown -R $(USER) /usr/local/lib/sbcl
	@echo "The user" $(USER) "will be able to compile the libraries." 

