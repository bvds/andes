install-clsql:
	tar zxf clsql-4.0.5.tgz
	patch -p0 < clsql-4.0.5.patch
	mv -n clsql-4.0.5 /usr/local/lib/sbcl/site
	(cd /usr/local/lib/sbcl/site-systems; ln -s ../site/clsql-4.0.5/*.asd .)
clean-clsql:
	rm -r -f /usr/local/lib/sbcl/site/clsql-4.0.5
	rm -f /usr/local/lib/sbcl/site-systems/clsql*.asd	