#
#   Create LON-CAPA assignments
#
#  
#
SERVER=andes.eas.asu.edu

create:
	sbcl --load create-sets.cl
	cp default.meta all-problems/

push:
	ssh ${SERVER} '(cd public_html/;rm -r -f *-problems/ *-maps/)'
	scp -p -r *-problems/ ${SERVER}:public_html/
	scp -p -r *-maps/ ${SERVER}:public_html/
	@echo "On lon-capa server, as root do:"
	@echo 'cd public_html/; chown -R www.bvds *-problems *-maps'

