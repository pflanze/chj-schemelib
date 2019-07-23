;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         (cj-path path-string?)
         ;; utf8 ah, no, this one:
         (u8vector0 string.utf8-u8vector0
                    u8vector.utf8-parse)
         (cj-c-util define-constant-from-C)
         (sqlite3-errors sqlite3-error-info))


(export (class foreign-object
               (class sqlite3)
               (class sqlite3-statement))
        sqlite3-open
        #!optional
        list&convert
        bitwise-combine)

;;(c/load "sqlite3" ld-options: "-lsqlite3")


;; Design:

;; - use ##c-code only. Nice since it really allows to tailor the
;;   interface, via C code, instead of having to bind all lower-level
;;   C functions via c-lambda and calling them (and perhaps still have
;;   problem of how to report errors (no union support))

;; - do try to be fully safe in the sense that any unsafe usage of the
;;   API is reported.

;; - but do not currently clone statement objects for concurrent
;;   streams (how large would overhead be (need for
;;   re-optimization)?). This means usage of statements needs to
;;   remain non-concurrent even with streams, which is disappointing
;;   but probably in-line with what other languages will offer.


;; TODO:

;; - GC during ##c-code while using movable objects: FAIL?
;; - thread safety? (Only relevant with SMP Gambit)


(include "cj-standarddeclares.scm")

(c-declare "
#include <sqlite3.h>
#include <stdio.h>
#include <string.h>

/* Gambit should use const char* for string conversion inputs; XX
   patch Gambit instead */
#define HACK_CONST_CHARP(v) ___CAST(char *, v)

static void set_result_errmsg(___SCMOBJ *result, sqlite3* db, int err)
{
        const char *msg= sqlite3_errmsg(db);
        ___SCMOBJ smsg;
        ___SCMOBJ ___err; /* Gambit FFI is under-specified? */
        ___BEGIN_CFUN_CHARSTRING_TO_SCMOBJ(HACK_CONST_CHARP(msg), smsg);
        ___SCMOBJ p= ___EXT(___make_pair) (___FIX(err), smsg, ___STILL);
        *result= p;
        ___EXT(___release_scmobj)(p);
        ___END_CFUN_CHARSTRING_TO_SCMOBJ(NULL, smsg);
}
")


(def (list&convert . vals)
     (map (lambda (val)
            (if (u8vector? val)
                (u8vector.utf8-parse val)
                val))
          vals))

(def (sqlite3:catching proc/1 msg)
     (lambda (s)
       ;; ah, needs to allocate anyway; sigh slow. Ocaml?
       (with-exception-catcher (lambda (e)
                                 (WARN msg e))
                               (& (proc/1 s)))))


(def (bitwise-combine . vals)
     (assert (zero? (apply bitwise-and vals)))
     (apply bitwise-or vals))

(TEST
 > (bitwise-combine 1 4)
 5
 > (bitwise-combine 1 4 2)
 7
 > (bitwise-combine 4 3)
 7
 > (%try (bitwise-combine 1 3))
(exception
 text:
 "assertment failure: (zero? (apply bitwise-and vals)) (zero? (apply bitwise-and '(1 3)))\n"))



(define-constants-from-C
  SQLITE_OPEN_READWRITE
  SQLITE_OPEN_CREATE)

(define-constant-from-C C:SQLITE_OK)
(assert (= C:SQLITE_OK SQLITE_OK))

(define-constants-from-C
  SQLITE_STMTSTATUS_FULLSCAN_STEP
  SQLITE_STMTSTATUS_SORT
  SQLITE_STMTSTATUS_AUTOINDEX
  SQLITE_STMTSTATUS_VM_STEP)


(defclass (foreign-object [boolean? freed?]
                          [fixnum? address]))
(def foreign-object-freed?-slot 1)
(def foreign-object-address-slot 2)


;; data types:

;; sqlite3

;; The database connection object. Created by sqlite3_open() and
;; destroyed by sqlite3_close().

;;(c-define-type "")  or go with wordaddress (fixnum addresses) unsafe.

(defclass (sqlite3)
  extends: foreign-object

  (def (sqlite3-open [path-string? path])
       (let ((res (sqlite3 #f 0))
             (path* (string.utf8-u8vector0 path)))
         (let ((err (##c-code "
    ___SCMOBJ res= ___ARG1;
    ___SCMOBJ path= ___ARG2;
    ___SCMOBJ flags= ___ARG3;
    ___SCMOBJ foreign_object_address_slot= ___ARG4;
    sqlite3 *p= NULL;
    int err= sqlite3_open_v2
                   (___CAST(char*,___BODY(path)),
                    &p,
                    ___INT(flags),
                    NULL);
    if (p) {
        sqlite3_extended_result_codes(p, 1);
    }
    ___VECTORSET(res,
                 foreign_object_address_slot,
                 ___FIX(___CAST(___WORD, p) >> 2));
    ___RESULT= ___FIX(err);
"
                              res
                              path*
                              ;; flags:
                              (bitwise-combine SQLITE_OPEN_READWRITE
                                               SQLITE_OPEN_CREATE)
                              foreign-object-address-slot)))
           (if-sqlite3-error-OK
            err
            (begin
              (make-will res catching-sqlite3.close!)
              res)))))


  (defmethod (close! s)
    (if freed?
        (void)
        (let ((err (##c-code "
    ___SCMOBJ addr= ___ARG1;
    ___RESULT= ___FIX(sqlite3_close(___CAST(sqlite3*, ___INT(addr)<<2)));
"
                             address)))
          (if-sqlite3-error-OK
           err
           (begin
             (##vector-set! s foreign-object-freed?-slot #t)
             (void))))))

  (def catching-sqlite3.close!
       (sqlite3:catching sqlite3.close! "sqlite3.close!"))


  (defmethod (prepare db code)
    (assert (not freed?))
    (let ((res (sqlite3-statement #f -1 db code #f #f 0))
          (code* (string.utf8-u8vector0 code)))
      (let ((err (##c-code "
    ___SCMOBJ res= ___ARG1;
    sqlite3* db= ___CAST(sqlite3*, ___INT(___ARG2)<<2);
    ___SCMOBJ code= ___ARG3;
    ___SCMOBJ foreign_object_address_slot= ___ARG4;

    sqlite3_stmt *p= NULL;
    sqlite3_mutex *m= sqlite3_db_mutex(db);
    sqlite3_mutex_enter(m);
    int err= sqlite3_prepare_v2
                   (db,
                    ___CAST(char*,___BODY(code)),
                    ___INT(___U8VECTORLENGTH(code))-1,
                    &p,
                    NULL);
    ___VECTORSET(res,
                 foreign_object_address_slot,
                 ___FIX(___CAST(___WORD, p) >> 2));
    if (err == SQLITE_OK) {
        ___RESULT= ___FIX(err);
    } else {
        set_result_errmsg(&___RESULT, db, err);
    }
    sqlite3_mutex_leave(m);
"
                           res
                           address
                           code*
                           foreign-object-address-slot)))
        (if-sqlite3-error-OK
         err
         (begin
           (make-will res catching-sqlite3-statement.close!)
           res))))))


;; sqlite3_stmt

;; The prepared statement object. Created by sqlite3_prepare() and
;; destroyed by sqlite3_finalize().


(defclass (sqlite3-statement [sqlite3? db] [string? code]
                             _column-count _column-pad
                             ;; track side effects to detect mis-use:
                             [fixnum? effect-id]
                             )
  ;; ^ must link db to ensure that it is not garbage collected
  extends: foreign-object

  (defmethod (_column-count-set! s v)
    (vector-set! s 5 v))
  (defmethod (_column-pad-set! s v)
    (vector-set! s 6 v))
  ;; hmm can `int sqlite3_stmt_status(sqlite3_stmt*, int op,int
  ;; resetFlg)` be used instead?
  ;; (file:///usr/share/doc/sqlite3-doc/c3ref/stmt_status.html):
  (defmethod (effect-id-inc! s)
    ;; XX hmm concurrency?... spinlock here!
    (let ((id (inc effect-id)))
      (vector-set! s 7 id)
      id))

  (defmethod (close! s)
    ;; copy-paste from sqlite3.close! with changes only in 1 line of C
    (if freed?
        (void)
        (let ((err (##c-code "
    ___SCMOBJ addr= ___ARG1;
    ___RESULT= ___FIX(sqlite3_finalize(___CAST(sqlite3_stmt*,
                                               ___INT(addr)<<2)));
"
                             address)))
          (if-sqlite3-error-OK
           err
           (begin
             (##vector-set! s foreign-object-freed?-slot #t)
             (void))))))

  (def catching-sqlite3-statement.close!
       (sqlite3:catching sqlite3-statement.close! "sqlite3-statement.close!"))


  (defmethod (status s [fixnum? what]) -> exact-integer?
    (##c-code "
    sqlite3_stmt* st= ___CAST(sqlite3_stmt*, ___INT(___ARG1)<<2);
    ___SCMOBJ what= ___ARG2;

    ___RESULT= ___S64BOX(sqlite3_stmt_status(st, what, 0));
"
              address
              what))

  (defmethod (status* s)
    (map (C .status s _)
         (list
          SQLITE_STMTSTATUS_FULLSCAN_STEP
          SQLITE_STMTSTATUS_SORT
          SQLITE_STMTSTATUS_AUTOINDEX
          SQLITE_STMTSTATUS_VM_STEP)))
  

  ;; private, use .step instead!
  (defmethod (step s) -> symbol? ;; one of a few selected SQLITE_* values
    (assert (not freed?))
    (sqlite3-statement.effect-id-inc! s)
    (let ((err (##c-code "
    sqlite3_stmt* st= ___CAST(sqlite3_stmt*, ___INT(___ARG1)<<2);
    sqlite3* db= ___CAST(sqlite3*, ___INT(___ARG2)<<2);

    sqlite3_mutex *m= sqlite3_db_mutex(db);
    sqlite3_mutex_enter(m);
    int err= sqlite3_step(st);
    if ((err == SQLITE_DONE) || (err == SQLITE_ROW)) {
        ___RESULT= ___FIX(err);
    } else {
        set_result_errmsg(&___RESULT, db, err);
    }
    sqlite3_mutex_leave(m);
"
                         address
                         (@sqlite3.address db))))
      (sqlite3-error-return err (SQLITE_DONE SQLITE_ROW))))

  
  (defmethod (execute s) -> void?
    (xcase (.step s)
           ((SQLITE_DONE) (void))
           ((SQLITE_ROW)
            ;; XX typed exceptions?
            (error "sqlite3: executed a statement that returns rows"))))

  
  (defmethod (reset s)
    (assert (not freed?))
    (sqlite3-statement.effect-id-inc! s)
    (let ((err (##c-code "
    sqlite3_stmt* st= ___CAST(sqlite3_stmt*, ___INT(___ARG1)<<2);

    ___RESULT= ___FIX(sqlite3_reset(st));

"
                         address)))
      (if-sqlite3-error-OK err s)))

  
  (defmethod (bind s [fixnum-natural0? i] val)
    (assert (not freed?))
    
    ;; file:///usr/share/doc/sqlite3-doc/c3ref/bind_blob.html

    ;; int sqlite3_bind_double(sqlite3_stmt*, int, double);
    ;; int sqlite3_bind_int(sqlite3_stmt*, int, int);
    ;; int sqlite3_bind_int64(sqlite3_stmt*, int, sqlite3_int64);
    ;; ;; int sqlite3_bind_null(sqlite3_stmt*, int);  use NULL with the others?

    ;; In those routines that have a fourth argument, its value is the
    ;; number of bytes in the parameter. To be clear: the value is the
    ;; number of bytes in the value, not the number of characters. If
    ;; the fourth parameter to sqlite3_bind_text() or
    ;; sqlite3_bind_text16() is negative, then the length of the string
    ;; is the number of bytes up to the first zero terminator. If a
    ;; non-negative fourth parameter is provided to sqlite3_bind_text()
    ;; or sqlite3_bind_text16() or sqlite3_bind_text64() then that
    ;; parameter must be the byte offset where the NUL terminator would
    ;; occur assuming the string were NUL terminated. [i.e. length
    ;; excluding the \0]

    ;; The fifth argument to the BLOB and string binding interfaces is a
    ;; destructor used to dispose of the BLOB or string after SQLite has
    ;; finished with it.
  
    ;; int sqlite3_bind_text(sqlite3_stmt*,int,const char*,int,void(*)(void*));
    ;; int sqlite3_bind_text16(sqlite3_stmt*, int, const void*, int,
    ;;                          void(*)(void*));
    ;; int sqlite3_bind_text64(sqlite3_stmt*, int, const char*, sqlite3_uint64,
    ;;                          void(*)(void*), unsigned char encoding);

    ;; The sixth argument to sqlite3_bind_text64() must be one of
    ;; SQLITE_UTF8, SQLITE_UTF16, SQLITE_UTF16BE, or SQLITE_UTF16LE to
    ;; specify the encoding of the text in the third parameter.
  
    ;; int sqlite3_bind_value(sqlite3_stmt*, int, const sqlite3_value*);

    (let ((err
           (##c-code "
    sqlite3_stmt* st= ___CAST(sqlite3_stmt*, ___INT(___ARG1)<<2);
    int i= ___INT(___ARG2);
    ___SCMOBJ val= ___ARG3;

    int err;
    if (___FIXNUMP(val)) {
        err= sqlite3_bind_int64(st, i, ___INT(val));
    } else if (___FLONUMP(val)) {
        err= sqlite3_bind_double(st, i, ___F64UNBOX(val));
    } else if (___U8VECTORP(val)) {
        err= sqlite3_bind_text(st, i, ___CAST(const char*,___BODY(val)),
                                  ___INT(___U8VECTORLENGTH(val)), NULL);
    } else {
        err= -1;
    } 
    ___RESULT= ___FIX(err);
"
                     address
                     i
                     (cond ((or (fixnum? val)
                                ;; (and (number? val)
                                ;;      (exact? val)
                                ;;      (integer? val))
                                (flonum? val)
                                (u8vector? val))
                            val)
                           ((string? val)
                            (string.utf8-u8vector val))
                           (else
                            (error
                             "sqlite3-statement.bind: no support for value:"
                             val))))))
      (case err
        ((-1) (error "sqlite3-statement.bind: BUG"))
        (else
         (if-sqlite3-error-OK err (void))))))


  (defmethod (get-row s constructor)
    (if (not _column-count)
        (let ((count (or (##c-code "
    sqlite3_stmt* st= ___CAST(sqlite3_stmt*, ___INT(___ARG1)<<2);

    int c= sqlite3_column_count(st);
    if (c < 1000000) {
        ___RESULT= ___FIX(c);
    } else {
        ___RESULT= ___FAL;
    }
"
                                   address)
                         (error "bug"))))
          (sqlite3-statement._column-count-set! s count)
          ;; XX evil since it references memory for longer?
          (sqlite3-statement._column-pad-set! s (make-vector count))
          (sqlite3-statement.get-row s constructor))

        (begin 
          (assert (not freed?))
          (let ((err (##c-code "
    sqlite3_stmt* st= ___CAST(sqlite3_stmt*, ___INT(___ARG1)<<2);
    int count= ___INT(___ARG2);
    ___SCMOBJ pad= ___ARG3;

    ___RESULT= ___FAL;
    for (int i=0; i<count; i++) {
        int t= sqlite3_column_type(st, i);
        ___SCMOBJ val;
        if (t == SQLITE_INTEGER) {
            ___S64 v= sqlite3_column_int64(st, i);
            val= ___S64BOX(v);
        } else if (t == SQLITE_FLOAT) {
            double v= sqlite3_column_double(st, i);
            val= ___F64BOX(v);
        } else if (t == SQLITE_TEXT) {
            const unsigned char* str= sqlite3_column_text(st, i);
            int bytes= sqlite3_column_bytes(st, i);
            /* ___CHARSTRING_to_SCMOBJ won't work since it looks for \0.
               Also want to start using u8vectors anyway, hence: */
            ___BEGIN_ALLOC_U8VECTOR(bytes);
            ___END_ALLOC_U8VECTOR(bytes);
            val= ___GET_U8VECTOR(bytes);
            /* XX safe? No GC triggered after here and before storing it
               in pad? Well.. SMP? AH wow pad loses contact anyway. */
            memcpy(___BODY(val), str, bytes);
        } else if (t == SQLITE_BLOB) {
            // todo
            ___RESULT= ___FIX(i);
            break;
        } else if (t == SQLITE_NULL) {
            val= ___FAL;
        } else {
            ___RESULT= ___FIX(i);
            break;
        }
        ___VECTORSET(pad, ___FIX(i), val);
    }
"
                               address
                               _column-count
                               _column-pad)))
            (if err
                (error "sqlite3-statement.get-row: conversion failure for column:" err)
                (apply-vector constructor _column-pad))))))


  ;; stream of rows
  (defmethod (get-rows s #!optional (constructor list&convert) (tail '()))
    (let rec ((expected-effect-id effect-id))
      (delay
        (let ((current-effect-id (sqlite3-statement.effect-id s)))
          (if (= current-effect-id expected-effect-id)
              (xcase (sqlite3-statement.step s)
                     ((SQLITE_ROW) (cons (sqlite3-statement.get-row s constructor)
                                         (rec (inc expected-effect-id))))
                     ((SQLITE_DONE) tail))
              (error "sqlite3-statement.get-rows: no. concurrent side-effects on this statement:"
                     (- current-effect-id expected-effect-id)))))))

  
  (defmethod (send-rows s rows)
    (.for-each rows
               (lambda (row)
                 (.reset s)
                 (.for-each/iota row
                                 (lambda (val i)
                                   (.bind s (inc i) val)))
                 (.execute s)))))

