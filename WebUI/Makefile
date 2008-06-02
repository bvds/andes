# 
# minimal makefile to compile and package the Andes drawing applet
# to be invoked from parent of andes package directory
#

SRCS= andes/Drawing.java andes/DrawObj.java

Drawing.jar: classfiles # !!! also images
	jar cmf Drawing.mf Drawing.jar andes/*.class andes/images

classfiles: $(SRCS)
	javac $?
	date > classfiles

clean:
	$(RM) andes/*.class

