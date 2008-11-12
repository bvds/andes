# 
# minimal makefile to compile and package the Andes drawing applet
# to be invoked from parent of andes package directory
#

SRCS= andes/Drawing.java andes/DrawObj.java
DEMO= Drawing.jar Drawing.jnlp DrawingDemo.html

Drawing.jar: classfiles # !!! also images
	jar cmf Drawing.mf Drawing.jar andes/*.class andes/images

classfiles: $(SRCS)
	javac $?
	date > classfiles

post: $(DEMO)
	scp $? andes@andes3.lrdc.pitt.edu:~/public_html/demo
	date > post

clean:
	$(RM) andes/*.class

