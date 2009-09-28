#
#  clsql can be obtained from http://clsql.b9.com
#  However, it does not work well with asdf-install and cffi.
#  So we include a local copy which we patch and install in the system-wide
#  sbcl libraries.
#
install-clsql:
	tar zxf clsql-4.0.5.tgz
	patch -p0 < clsql-4.0.5.patch
	mv -n clsql-4.0.5 /usr/local/lib/sbcl/site
	(cd /usr/local/lib/sbcl/site-systems; ln -s ../site/clsql-4.0.5/*.asd .)
clean-clsql:
	rm -r -f /usr/local/lib/sbcl/site/clsql-4.0.5
	rm -f /usr/local/lib/sbcl/site-systems/clsql*.asd

install-database:
	@echo "This will destroy any existing database!"
	@echo "Enter mysql password"
	(cd LogProcessing/databaseCreationScripts/; mysql -u root -p < AndesDatabaseCreationSQL.sql)