;;;;
;;;;  Reset problem and rule tables.
;;;;  This must be run before other problems in the module
;;;;

  ;; loading this file resets the problem database:
  (clear-problem-registry)
  ;; clear out the old operators on load so that the new ones can be defined.
  (clear-ops)
  ;; Reset ontology database on each load of this file.
  (clear-ontology)
  ;; reset NewtonsNogoods list
  (clear-nogoods)		

