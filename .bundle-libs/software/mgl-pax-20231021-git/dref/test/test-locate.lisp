(in-package :dref-test)

(deftest test-locate ()
  (test-locate/xref)
  (test-locate/undefined)
  (test-locate/variable)
  (test-locate/constant)
  (test-locate/macro)
  (test-locate/compiler-macro)
  (test-locate/symbol-macro)
  (test-locate/setf)
  (test-locate/function)
  (test-locate/generic-function)
  (test-locate/method)
  (test-locate/method-combination)
  (test-locate/accessor)
  (test-locate/reader)
  (test-locate/writer)
  (test-locate/structure-accessor)
  (test-locate/type)
  (test-locate/class)
  (test-locate/condition)
  (test-locate/declaration)
  (test-locate/restart)
  (test-locate/asdf-system)
  (test-locate/package)
  (test-locate/readtable)
  (test-locate/locative)
  (test-locate/unknown)
  (test-locate/lambda)
  (test-locate/alias))

(defun check-ref (reference name locative &optional type)
  (is (and (% reference)
           (equal (xref-name reference) name)
           (equal (xref-locative reference) locative)
           (or (null type)
               (typep reference type)))))

(deftest test-locate/xref ()
  (check-ref (locate (xref '*package* 'variable))
             '*package* 'variable 'variable-dref))

(deftest test-locate/undefined ()
  (signals (locate-error)
    (locate :xxx))
  (signals (locate-error)
    (locate (xref '*package* 'undefined))))

(deftest test-locate/variable ()
  (check-ref (dref '*package* 'variable)
             '*package* 'variable 'variable-dref)
  (check-ref (dref '*package* '(variable 7))
             '*package* 'variable 'variable-dref)
  (check-ref (dref-origin (dref '*package* '(variable 7)))
             '*package* '(variable 7) 'xref)
  (signals (locate-error :pred "Bad argument")
    (dref '*package* '(variable 7 8)))
  (with-failure-expected ((and (alexandria:featurep '(:not (:or :ccl :sbcl)))
                               'failure))
    (with-test ("How to detect specials?")
      (signals (locate-error)
        (dref '*non-existent* 'variable)))))

(deftest test-locate/constant ()
  (check-ref (dref 'bar 'constant)
             'bar 'constant 'constant-dref)
  (check-ref (dref 'bar '(constant 7))
             'bar 'constant 'constant-dref)
  (check-ref (dref-origin (dref 'bar '(constant 7)))
             'bar '(constant 7) 'xref)
  (check-ref (dref 'bar 'variable)
             'bar 'constant 'constant-dref)
  (check-ref (dref 'bar '(variable 7))
             'bar 'constant 'constant-dref)
  (check-ref (dref-origin (dref 'bar '(variable 7)))
             'bar '(constant 7) 'xref)
  (signals (locate-error :pred "that's CONSTANTP")
    (dref '*package* 'constant))
  (signals (locate-error :pred "Bad arguments")
    (dref 'bar '(constant 7 8)))
  (signals (locate-error)
    (dref :xxx 'constant))
  (signals (locate-error)
    (dref '*print-length* 'constant)))

(deftest test-locate/macro ()
  (check-ref (dref 'bar 'macro)
             'bar 'macro 'macro-dref)
  (with-failure-expected ((and (alexandria:featurep
                                '(:not (:or :allegro :ccl :clisp :ecl :sbcl)))
                               'failure))
    (with-test ("How to detect macro functions?")
      (signals-not (locate-error :pred "The name")
        (check-ref (locate (macro-function 'bar))
                   'bar 'macro 'macro-dref))))
  (signals (locate-error :pred "does not name")
    (dref 'foo 'macro))
  (signals (locate-error :pred "Bad arguments")
    (dref 'bar '(macro 7)))
  (let ((ref (xref 'setfed-macro 'macro)))
    (is (xref= (locate ref) ref)))
  (signals (resolve-error)
    (resolve (dref:xref 'function 'macro))))

(deftest test-locate/symbol-macro ()
  (check-ref (dref 'my-smac 'symbol-macro)
             'my-smac 'symbol-macro 'symbol-macro-dref)
  (with-failure-expected ((and (alexandria:featurep '(:not (:or :ccl :sbcl)))
                               'failure))
    (with-test ("How to detect symbol macros?")
      (signals (locate-error :pred "does not name")
        (dref 'foo 'symbol-macro))))
  (signals (locate-error)
    (dref "xxx" 'symbol-macro))
  (signals (locate-error :pred "Bad arguments")
    (dref 'smac '(symbol-macro 7))))

(deftest test-locate/compiler-macro ()
  (check-ref (dref 'foo 'compiler-macro)
             'foo 'compiler-macro 'compiler-macro-dref)
  (with-failure-expected ((and (alexandria:featurep
                                '(:not (:or :allegro :ccl :clisp :sbcl)))
                               'failure))
    (with-test ("How to detect compiler macro functions?")
      (signals-not (locate-error)
        (check-ref (locate (compiler-macro-function 'foo))
                   'foo 'compiler-macro 'compiler-macro-dref))))
  (signals (locate-error :pred "does not name")
    (dref 'xxx 'compiler-macro))
  (signals (locate-error)
    (dref "xxx" 'compiler-macro))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo '(compiler-macro 7))))

(deftest test-locate/setf ()
  (signals (locate-error :pred "does not have a SETF expansion")
    (dref 'foo 'setf))
  (signals (locate-error)
    (dref "xxx" 'setf))
  (signals (locate-error :pred "Bad arguments")
    (dref 'has-setf-expander '(setf 7 8)))
  (signals (locate-error :pred "Only the METHOD locative")
    (dref 'has-setf-expander '(setf 7)))
  (test-locate/setf/expander)
  (test-locate/setf/function)
  (test-locate/setf/generic-function)
  (test-locate/setf/method))

(deftest test-locate/setf/expander ()
  (check-ref (dref 'has-setf-expander 'setf)
             'has-setf-expander 'setf 'setf-dref)
  (is (null (resolve (xref 'has-setf-expander 'setf) nil))))

(deftest test-locate/setf/function ()
  (check-ref (dref 'has-setf-function 'setf)
             'has-setf-function 'setf 'setf-dref)
  (check-ref (locate #'(setf has-setf-function))
             'has-setf-function 'setf 'setf-dref)
  (is (eq (resolve (xref 'has-setf-function 'setf) nil)
          #'(setf has-setf-function))))

(deftest test-locate/setf/generic-function ()
  (check-ref (dref 'has-setf-generic-function 'setf)
             'has-setf-generic-function 'setf 'setf-dref)
  (check-ref (locate #'(setf has-setf-generic-function))
             'has-setf-generic-function 'setf 'setf-dref)
  (is (eq (resolve (xref 'has-setf-generic-function 'setf) nil)
          #'(setf has-setf-generic-function))))

(deftest test-locate/setf/method ()
  (check-ref (dref 'has-setf-generic-function
                   '(setf (method () (string))))
             'has-setf-generic-function '(setf (method () (string)))
             'setf-dref)
  (signals (locate-error :pred "METHOD not found.")
    (dref 'has-setf-generic-function '(setf (method () (undefined)))))
  (is (eq (resolve (xref 'has-setf-generic-function
                              '(setf (method () (string)))))
          (dref::find-method* '(setf has-setf-generic-function) () '(string)))))

(deftest test-locate/function ()
  (check-ref (dref 'foo 'function) 'foo 'function 'function-dref)
  (check-ref (locate #'foo) 'foo 'function 'function-dref)
  (signals (locate-error :pred "not a symbol naming a function")
    (dref 'undefined 'function))
  (signals (locate-error :pred "BAR names a macro")
    (dref 'bar 'function))
  (signals (locate-error)
    (dref "xxx" 'function))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo '(function 7)))
  (is (eq (resolve (dref 'foo 'function)) #'foo))
  (with-test ("traced")
    (check-ref (dref 'traced-foo 'function)
               'traced-foo 'function 'function-dref)
    (with-failure-expected ((and (alexandria:featurep '(:or :abcl :clisp))
                                 'failure))
      (check-ref (locate #'traced-foo nil)
                 'traced-foo 'function 'function-dref))))

(deftest test-locate/generic-function ()
  (check-ref (dref 'test-gf 'generic-function)
             'test-gf 'generic-function 'generic-function-dref)
  (check-ref (locate #'test-gf) 'test-gf 'generic-function
             'generic-function-dref)
  (with-test ("actualize function to generic-function")
    (check-ref (dref 'test-gf 'function)
               'test-gf 'generic-function 'generic-function-dref))
  (signals (locate-error :pred "not a symbol naming a generic function")
    (dref 'undefined 'generic-function))
  (signals (locate-error :pred "not a symbol naming a generic function")
    (dref 'bar 'generic-function))
  (signals (locate-error :pred "not a symbol naming a generic function")
    (dref "xxx" 'generic-function))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo '(generic-function 7)))
  (is (eq (resolve (dref 'test-gf 'generic-function)) #'test-gf))
  (with-test ("traced")
    (check-ref (dref 'traced-gf 'function)
               'traced-gf 'generic-function 'generic-function-dref)
    (with-failure-expected ((and (alexandria:featurep '(:or :abcl :clisp))
                                 'failure))
      (check-ref (locate #'traced-gf nil)
                 'traced-gf 'generic-function 'generic-function-dref))))

(deftest test-locate/method ()
  (check-ref (dref 'test-gf '(method () (number)))
             'test-gf '(method () (number)) 'method-dref)
  (check-ref (locate (dref::find-method* 'test-gf () '(number)))
             'test-gf '(method () (number)) 'method-dref)
  (signals (locate-error :pred "Method does not exist")
    (dref 'undefined '(method () (number))))
  (signals (locate-error :pred "Method does not exist")
    (dref 'test-gf '(method () (undefined))))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo 'method))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo '(method ())))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo '(method ())))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo '(method () () ())))
  (signals (locate-error :pred "Method does not exist")
    (dref 'foo '(method 7 8)))
  (is (eq (resolve (dref 'test-gf '(method () (number))))
          (dref::find-method* 'test-gf () '(number)))))

(deftest test-locate/method-combination ()
  (check-ref (dref 'my-comb 'method-combination)
             'my-comb 'method-combination 'method-combination-dref)
  (with-failure-expected ((and (alexandria:featurep '(:not (:or :ccl :sbcl)))
                               'failure))
    (with-test ("How to detect method combinations?")
      (signals (locate-error)
        (dref 'undefined 'method-combination))))
  (signals (locate-error)
    (dref "xxx" 'method-combination)))

(deftest test-locate/accessor ()
  (check-ref (dref 'foo-a '(accessor foo))
             'foo-a '(accessor foo) 'accessor-dref)
  (with-test ("recognize writer method object as part of accessor")
    (check-ref (locate (dref::find-method* '(setf foo-a) () '(t foo)))
               'foo-a '(accessor foo) 'accessor-dref))
  (with-test ("recognize reader method object as part of accessor")
    (check-ref (locate (dref::find-method* 'foo-a () '(foo)))
               'foo-a '(accessor foo) 'accessor-dref))
  (with-test ("actualize reader method to accessor")
    (check-ref (dref 'foo-a '(reader foo))
               'foo-a '(accessor foo) 'accessor-dref))
  (with-test ("recognize the writer method of an accessor")
    (check-ref (dref 'foo-a '(writer foo))
               'foo-a '(accessor foo) 'accessor-dref))
  (signals (locate-error)
    (dref 'foo-r '(accessor foo)))
  (signals (locate-error)
    (dref 'foo-w '(accessor foo)))
  (signals (locate-error)
    (dref 'foo-a '(accessor t)))
  (signals (locate-error)
    (dref "xxx" '(accessor foo)))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo-a 'accessor))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo-a '(accessor 1 2)))
  (with-test ("an accessor dref resolves to its writer method")
    (is (eq (resolve (xref 'foo-a '(accessor foo)))
            (dref::find-method* '(setf foo-a) () '(t foo))))))

(deftest test-locate/reader ()
  (check-ref (dref 'foo-r '(reader foo))
             'foo-r '(reader foo) 'reader-dref)
  (check-ref (locate (dref::find-method* 'foo-r () '(foo)))
             'foo-r '(reader foo) 'reader-dref)
  (signals (locate-error)
    (dref 'foo-w '(reader foo)))
  (signals (locate-error)
    (dref 'foo-r '(reader t)))
  (signals (locate-error)
    (dref "xxx" '(reader foo)))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo-r 'reader))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo-r '(reader 1 2)))
  (is (eq (resolve (xref 'foo-r '(reader foo)))
          (dref::find-method* 'foo-r () '(foo)))))

(deftest test-locate/writer ()
  (check-ref (dref 'foo-w '(writer foo))
             'foo-w '(writer foo) 'writer-dref)
  (check-ref (locate (dref::find-method* 'foo-w () '(t foo)))
             'foo-w '(writer foo) 'writer-dref)
  (signals (locate-error)
    (dref 'foo-r '(writer foo)))
  (signals (locate-error)
    (dref 'foo-w '(writer t)))
  (signals (locate-error)
    (dref "xxx" '(writer foo)))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo-w 'writer))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo-w '(writer 1 2)))
  (is (eq (resolve (xref 'foo-w '(writer foo)))
          (dref::find-method* 'foo-w () '(t foo)))))

(deftest test-locate/structure-accessor ()
  (let ((lacks-name (alexandria:featurep '(:not (:or :ccl :sbcl)))))
    (with-failure-expected ((and lacks-name 'failure))
      (check-ref (dref 'baz-aaa 'structure-accessor)
                 'baz-aaa '(structure-accessor baz) 'structure-accessor-dref))
    (with-test ("How to detect structure accessors?")
      (with-failure-expected ((and (alexandria:featurep
                                    '(:or :abcl :allegro :cmucl :ecl))
                                   'failure))
        (signals-not (locate-error)
          (with-failure-expected ((and lacks-name 'failure))
            (check-ref (locate #'baz-aaa)
                       'baz-aaa '(structure-accessor baz)
                       'structure-accessor-dref)))))
    (check-ref (dref 'baz-aaa '(structure-accessor baz))
               'baz-aaa '(structure-accessor baz) 'structure-accessor-dref)
    (check-ref (dref-origin (dref 'baz-aaa '(structure-accessor baz)))
               'baz-aaa '(structure-accessor baz) 'xref)
    (signals (locate-error :pred "not a symbol that names a function")
      (dref 'undefined 'structure-accessor))
    (with-failure-expected ((and lacks-name 'failure))
      (signals (locate-error :pred "not a structure accessor")
        (dref 'foo 'structure-accessor)))
    (signals (locate-error :pred "not a symbol")
      (dref "xxx" 'structure-accessor))
    (with-failure-expected ((and lacks-name 'failure))
      (signals (locate-error :pred "This accessor is on")
        (dref 'baz-aaa '(structure-accessor 1))))
    (signals (locate-error :pred "Bad arguments")
      (dref 'baz-aaa '(structure-accessor 1 2)))
    (with-failure-expected ((and lacks-name 'failure))
      (is (eq (resolve (xref 'baz-aaa 'structure-accessor) nil)
              #'baz-aaa)))))

(deftest test-locate/type ()
  (check-ref (dref 'bar 'type)
             'bar 'type 'type-dref)
  (check-ref (dref 'foo 'type)
             'foo 'class 'class-dref)
  (check-ref (dref 'my-error 'type)
             'my-error 'condition 'condition-dref)
  (with-failure-expected ((and (alexandria:featurep '(:not (:or :ccl :sbcl)))
                               'failure))
    (with-test ("How to detect types?")
      (signals (locate-error)
        (dref 'undefined 'type))))
  (signals (locate-error)
    (dref "xxx" 'type))
  (signals (locate-error :pred "Bad arguments")
    (dref 'bar '(type 1))))

(deftest test-locate/class ()
  (check-ref (dref 'foo 'class)
             'foo 'class 'class-dref)
  (check-ref (locate (find-class 'foo))
             'foo 'class 'class-dref)
  (check-ref (dref 'my-error 'class)
             'my-error 'condition 'condition-dref)
  (signals (locate-error)
    (dref 'undefined 'class))
  (signals (locate-error)
    (dref "xxx" 'class))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo '(class 1)))
  (is (eq (resolve (xref 'foo 'class)) (find-class 'foo))))

(deftest test-locate/condition ()
  (check-ref (dref 'my-error 'condition)
             'my-error 'condition 'condition-dref)
  (check-ref (locate (find-class 'my-error))
             'my-error 'condition 'condition-dref)
  (check-ref (dref 'my-error 'class)
             'my-error 'condition 'condition-dref)
  (signals (locate-error)
    (dref 'undefined 'class))
  (signals (locate-error)
    (dref "xxx" 'class))
  (signals (locate-error :pred "Bad arguments")
    (dref 'foo '(class 1)))
  (is (eq (resolve (xref 'my-error 'condition))
          (find-class 'my-error))))

(deftest test-locate/declaration ()
  (check-ref (dref 'test-declaration 'declaration)
             'test-declaration 'declaration 'declaration-dref)
  (with-failure-expected ((and (alexandria:featurep
                                '(:not (:or :allegro :ccl :sbcl)))
                               'failure))
    (signals (locate-error)
      (dref 'undefined 'declaration)))
  (signals (locate-error)
    (dref "xxx" 'declaration))
  (signals (locate-error :pred "Bad arguments")
    (dref 'test-declaration '(declaration 1))))

(deftest test-locate/restart ()
  (check-ref (dref 'some-restart 'restart)
             'some-restart 'restart 'restart-dref)
  (signals (locate-error)
    (dref 'undefined 'restart))
  (signals (locate-error)
    (dref "xxx" 'restart))
  (signals (locate-error :pred "Bad arguments")
    (dref 'some-restart '(restart 1))))

(deftest test-locate/asdf-system ()
  (check-ref (dref "dref" 'asdf:system)
             "dref" 'asdf:system 'asdf-system-dref)
  (check-ref (dref "DREF" 'asdf:system)
             "dref" 'asdf:system 'asdf-system-dref)
  (check-ref (dref '#:dref 'asdf:system)
             "dref" 'asdf:system 'asdf-system-dref)
  (check-ref (dref '#:dref 'asdf:system)
             "dref" 'asdf:system 'asdf-system-dref)
  (check-ref (locate (asdf:find-system "dref"))
             "dref" 'asdf:system 'asdf-system-dref)
  (signals (locate-error)
    (dref 'undefined 'asdf:system))
  (signals (locate-error :pred "Bad arguments")
    (dref "dref" '(asdf:system 1)))
  (is (eq (resolve (xref "dref" 'asdf:system))
          (asdf:find-system "dref"))))

(deftest test-locate/package ()
  (check-ref (dref '#:dref 'package)
             (symbol-name '#:dref) 'package 'package-dref)
  (check-ref (locate (find-package '#:dref))
             (symbol-name '#:dref) 'package 'package-dref)
  (signals (locate-error)
    (dref "dREf" 'package))
  (signals (locate-error :pred "Bad arguments")
    (dref '#:dref '(package 1)))
  (is (eq (resolve (xref '#:dref 'package))
          (find-package '#:dref))))

(deftest test-locate/readtable ()
  (check-ref (dref 'xxx-rt 'readtable)
             'xxx-rt 'readtable 'readtable-dref)
  (check-ref (locate (named-readtables:find-readtable 'xxx-rt))
             'xxx-rt 'readtable 'readtable-dref)
  (check-ref (dref nil 'readtable)
             :common-lisp 'readtable 'readtable-dref)
  (signals (locate-error)
    (dref 'undefined 'readtable))
  (signals (locate-error)
    (dref "xxx" 'readtable))
  (signals (locate-error :pred "Bad arguments")
    (dref 'xxx-rt '(readtable 1)))
  (is (eq (resolve (xref 'xxx-rt 'readtable))
          (named-readtables:find-readtable 'xxx-rt))))

(deftest test-locate/locative ()
  (check-ref (dref 'my-loc 'locative)
             'my-loc 'locative 'locative-dref)
  (signals (locate-error)
    (dref 'undefined 'locative))
  (signals (locate-error)
    (dref "xxx" 'locative))
  (signals (locate-error :pred "Bad arguments")
    (dref 'my-loc '(locative 1))))

(deftest test-locate/unknown ()
  (signals (locate-error)
    (dref 'undefined '(unknown and more)))
  (dolist (dref (definitions 'print :locative-types '(unknown)))
    (is (xref= (dref 'print (dref-locative dref)) dref))))

(deftest test-locate/lambda ()
  (check-ref (dref nil '(lambda :file "xxx"))
             nil '(lambda :file "xxx") 'lambda-dref)
  (signals (locate-error)
    (dref 'xxx 'lambda))
  (let ((dref (dref nil '(lambda :arglist (x y z)
                                 :docstring "doc"
                                 :file "xxx"
                                 :file-position 7
                                 :snippet "snip"))))
    (is (equal (arglist dref) '(x y z)))
    (is (equal (docstring dref) "doc"))
    (is (equal (source-location-file (source-location dref)) "xxx"))
    (is (equal (source-location-file-position (source-location dref)) 7))
    (is (equal (source-location-snippet (source-location dref)) "snip"))))

(deftest test-locate/alias ()
  (check-ref (dref 'foo 'object)
             'foo 'class 'class-dref)
  (check-ref (dref '*some-var* '(var 7))
             '*some-var* 'variable 'variable-dref)
  (check-ref (dref-origin (dref '*some-var* '(var 7)))
             '*some-var* '(var 7) 'xref))
