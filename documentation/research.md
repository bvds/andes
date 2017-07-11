# Research

## Informed Consent

The status of informed consent is tracked in the `STUDENT_STATE` table in the database, with `model='client'`.  The properties `consent-dialog` & `informed-consent` are used to track status for each user and section.

The property `consent-dialog` sets the consent form for a section or user; consent forms are stored as `*.html` files in the `review` directory.  The value for this property normally contains the name of the `*.html` file.  If no consent form has been specified, then the student is shown the default form `consent.html`.  If `consent-dialog` is set to value `none` then the student is never shown a consent form.

The property `informed-consent` specifies the status for a particular student or section.  If the student clicks on a consent form, then the result is recorded in `informed-consent` as `agree:_name_` or `disagree:_name_`.  If consent has been given externally to Andes, this can be specified as `external:_name_`.

To create a consent form for a new section, create a new file in the `review` directory, using `review/consent.html` as a model.  Start up lisp and do the following to use that form for a given section:

    (rhelp) ;load help system
    (andes-database:create) ;open database connection
    ;; Use form my-consent.html for section my-section:
    (andes-database:set-state-property "consent-dialog" "my-consent.html" 
      :model "client" :section "my-section" :student nil :tid t)
    (andes-database:destroy)

If students have given consent externally for a section, this can be specified in the following manner:

    (rhelp) ;load help system
    (andes-database:create) ;open database connection
    (andes-database:set-state-property "consent-dialog" "none" 
      :model "client" :section "my-section" :student nil :tid t)
    (andes-database:set-state-property "informed-consent" "external:my-form" 
      :model "client" :section "my-section" :student nil :tid t)
    ;; If a particular student has opted-out, it can be specified as:
    (andes-database:set-state-property "informed-consent" "opt-out:my-form" 
      :model "client" :section "my-section" :student "bad-student" :tid t)
    (andes-database:destroy)

## Researcher Access

The Andes database has a user account `open` which gives access only to anonymized data where informed consent has been given.  The user `open` has access the tables `OPEN_STUDENT_STATE` and `OPEN_PROBLEM_ATTEMPT` which have anonymized user names and contain only users who have given informed consent.

Note that user names starting with `x:` or `md5:` are assumed to have already been anonymized.  This allows user names to be encoded externally to Andes.  Generally, this is a preferred method of anonymization since user names are then protected between the client and the Andes server.
