(uiop:define-package #:log4cl-extras/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:log4cl-extras/secrets))
(in-package log4cl-extras/changelog)


(defchangelog (:ignore-words ("SLY"
                              "REPL"
                              "API"
                              "CL-STRINGS"
                              "CL"
                              ":JSON"
                              ":PLAIN"
                              "HTTP"
                              "TRACEBACK-TO-STRING"))
  (0.9.0 2022-12-30
         "Function LOG4CL-EXTRAS/ERROR:PRINT-BACKTRACE now prints conditions with type like:

```
Condition REBLOCKS-WEBSOCKET:NO-ACTIVE-WEBSOCKETS: No active websockets bound to the current page.
```

instead of:

Condition: No active websockets bound to the current page.
")
  (0.8.0 2022-11-04
         "* Macro LOG4CL-EXTRAS/ERROR:WITH-LOG-UNHANDLED now has ERRORS-TO-IGNORE argument.
            You can pass a list of class-names of conditions which should not be logged.")
  (0.7.2 2022-10-03
         "* Backtrace printer was fixed to work on ClozureCL.")
    
  (0.7.1 2022-08-06
         "* Now LOG4CL-EXTRAS/SECRETS:MAKE-SECRETS-REPLACER is able to mask secret values even in strings nested in the lists.
            This fixes issue of leaking Authorization tokens when some HTTP error is logged.

              Previously, backtrace was logged like this:

              ```
              1 File \"/Users/art/projects/lisp/cloud-analyzer/.qlot/dists/ultralisp/software/fukamachi-dexador-20220619102143/src/backend/usocket.lisp\", line 451
                  In DEXADOR.BACKEND.USOCKET:REQUEST
                Args (#<unavailable argument> :METHOD :GET :HEADERS ((\"Authorization\" . \"OAuth AQAEA5qgMKaqAAffdZ0Nw7BqTkCTlp6ii80Gdmo\")))
              ```

              and oauth token leaked to the log storage.

              After this fix, backtrace will be logged like this:

              ```
              1 File \"/Users/art/projects/lisp/cloud-analyzer/.qlot/dists/ultralisp/software/fukamachi-dexador-20220619102143/src/backend/usocket.lisp\", line 451
                  In DEXADOR.BACKEND.USOCKET:REQUEST
                Args (#<unavailable argument> :METHOD :GET :HEADERS ((\"Authorization\" . \"OAuth #<secret value>\")))
              ```

          * A new variable LOG4CL-EXTRAS/ERROR:*ARGS-FILTER-CONSTRUCTORS* was introduced. It should be used together
            with LOG4CL-EXTRAS/SECRETS:MAKE-SECRETS-REPLACER to prevent secrets collection during the program life.

              Previosly, when you created a secrets replaced and stored in in the LOG4CL-EXTRAS/ERROR:*ARGS-FILTERS* variable,
              all secrets from logged backtraces were collected in a closure's state. When
              LOG4CL-EXTRAS/ERROR:*ARGS-FILTER-CONSTRUCTORS* variable is used, a new secrets replacer will be created
              for processing of each backtrace.
")
  (0.7.0 2022-07-03
         "* Macro LOG4CL-EXTRAS/ERROR:WITH-LOG-UNHANDLED now uses internal function and you can change backtrace length on the fly by changing LOG4CL-EXTRAS/ERROR:*MAX-TRACEBACK-DEPTH* variable.
          * Also, LOG4CL-EXTRAS/ERROR:*MAX-CALL-LENGTH* variable was documented.")
  (0.6.0 2021-10-03
         "* Now :PLAIN and :JSON logger will output logger's category, filename and a callable name.")
  (0.5.1 2021-03-02
         "* Fixed fail during logging error with `(setf some-func)` in the backtrace.")
  (0.5.0 2021-01-24
         "* Function TRACEBACK-TO-STRING was removed and
            replaced with LOG4CL-EXTRAS/ERROR:PRINT-BACKTRACE which is now
            a part of public API.
          * Added ability to filter secret and sensitive values.
            Read documentation, to lear more.")
  (0.4.2 2020-11-26
         "Fixed
          -----
          
          * Fixed WITH-LOG-UNHANDLED for cases when some function argument's print-object signaled the error.

            Because of this nasty error, sometimes WITH-LOG-UNHANDLED didn't log \"Unandled error\".


          Added
          -----

          * Now LOG4CL-EXTRAS/ERROR:WITH-LOG-UNHANDLED macro accepts key argument DEPTH which is 10 by default.

            This argument can be overriden by setting LOG4CL-EXTRAS/ERROR:*MAX-TRACEBACK-DEPTH*.

          * Also another variable LOG4CL-EXTRAS/ERROR:*MAX-CALL-LENGTH* can be set to control
            how long function or method name can be. By default it is 100, but methods are logged along
            with their specialized arguments and can be longer.")
  (0.4.1 2019-03-05
         "Fixed
          -----

          * Added missing dependency from CL-STRINGS system.")
  (0.4.0 2019-03-04
         "Improved
          --------

          Now LOG4CL-EXTRAS/CONFIG:SETUP sets appender into a mode when it prints log in a human
          readable way if it its called from the SLY's REPL. All logger fields are
          printed as well, including a traceback.
")
  (0.3.0 2019-01-07
         "Improved
          --------

          * Now condition's description is added to the end of the backtrace.")
  (0.2.2 2018-12-08
         "Fixed
          -----

          * Fixed system's loading in environments with `C` locale.

            This closes issue reported along with pull request #1.")
  (0.2.1 2018-11-24
         "Fixed
          -----

          * Previously, macros LOG4CL-EXTRAS/ERROR:WITH-LOG-UNHANDLED catched every signal,
            not only signals derived from ERROR. Because of that,
            it logged traceback for non error signals like that:

            ```lisp
            (log4cl-json/error:with-log-unhandled ()
                (signal \"foo\"))
            ```

            Now this bad behavior was fixed and only `errors` are logged.")
  (0.2.0 2017-08-29
         "New
          ---

          * Added ability to log tracebacks using LOG4CL-EXTRAS/ERROR:WITH-LOG-UNHANDLED.")
  (0.1.0 2017-01-23
         "* Initial version."))
