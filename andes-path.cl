;;;;
;;;;  Set the default path for finding Andes2 files
;;;;  (This should eventually not be needed)

(defparameter *andes-path*
    (make-pathname :host (pathname-host *load-pathname*)
		   :device (pathname-device *load-pathname*)
		   :directory (pathname-directory *load-pathname*)
		   :name nil
		   :type nil))
