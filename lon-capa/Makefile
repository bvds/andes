#
#   Create LON-CAPA assignments
#
SERVER=andes.eas.asu.edu

create:
	sbcl --userinit ../sbclrc-sample --load create-sets.cl
	cp default.meta all-problems/
	cp all-maps-default.meta all-maps/default.meta
	cp assigned-maps-default.meta assigned-maps/default.meta

push:
	ssh ${SERVER} '(cd public_html/;rm -r -f *-problems/ *-maps/)'
	scp -p -r *-problems/ ${SERVER}:public_html/
	scp -p -r *-maps/ ${SERVER}:public_html/
	@echo "On lon-capa server, as root do:"
	@echo 'cd public_html/; chown -R www.bvds *-problems *-maps *-problems.sequence*'

