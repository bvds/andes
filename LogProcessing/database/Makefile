#
#  If file exists, read in database login information.
#  See Documentation/server.html for instructions.
#
dbLogin = ../../db_user_password
ifeq ($(shell test -f ${dbLogin} && echo 1),1)
  dbUser := $(shell sed -n -e 1p ${dbLogin})
  dbPass := $(shell sed -n -e 2p  ${dbLogin})
# Use "if"; doing this as an "or" doesn't work in OS X
  dbNameIn := $(shell sed -n -e 3p ${dbLogin})
  dbName := $(if ${dbNameIn},${dbNameIn},andes3)
  dbFrom := "Reading database login from ${dbLogin}"
else
  dbUser := root
  dbPass := 
  dbName := andes3
  dbFrom := "See Documentation/server.html for instructions on creating ${dbLogin}."
endif
versionFile := db_update_version
version := $(shell test -f ${versionFile} && cat ${versionFile} || echo "0")
#  should be incremented as changes are made to database
installVersion := 5

install:
	@echo "${dbFrom}" 
	@echo "You must drop any existing ${dbName} or andes_test db."
	mysqladmin -u ${dbUser} -p'${dbPass}' create ${dbName}
	mysql -u ${dbUser} -p'${dbPass}' ${dbName} < AndesDatabaseCreationSQL.sql
	make open-pass
	echo "${installVersion}" > ${versionFile}

try:
	@echo "Test out access to database via login info."
	@echo "${dbFrom} version=${version} installVersion=${installVersion}"
	mysql -u ${dbUser} -p'${dbPass}' -e 'SHOW TABLES;' ${dbName}

update:
	@echo "${dbFrom}"
# Include version-specific update scripts, in order
ifeq ($(shell test ${version} -lt 5 && echo 1),1)
	mysqldump -u ${dbUser} -p'${dbPass}' ${dbName} > \
	   ${dbName}-${version}-dump.sql
        # This is where you would call an update script for this version.
	echo 5 > ${versionFile}
endif

open-pass:
	@echo "Input password for user 'open':"
	@read pass; mysql -u ${dbUser} -p'${dbPass}' -e "SET PASSWORD FOR 'open'@'localhost'=PASSWORD('$$pass')"


