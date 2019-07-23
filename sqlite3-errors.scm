;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         sqlite3-error)

(export sqlite3-error-from-code-or-pair
        ;;(macro when-sqlite3-error-OK)
        (macro if-sqlite3-error-OK)
        (macro sqlite3-error-return))

(include "cj-standarddeclares.scm")


;; file:///usr/share/doc/sqlite3-doc/rescode.html

(both-times
 (def sqlite3-errors
      (map (applying (lambda (code name desc)
                       (sqlite3-error-static code (string.symbol name) desc)))
           (list
            (list 0 "SQLITE_OK" "The SQLITE_OK result code means that the operation was successful and that there were no errors. Most other result codes indicate an error.")
            (list 1 "SQLITE_ERROR" "The SQLITE_ERROR result code is a generic error code that is used when no other more specific error code is available.")
            (list 2 "SQLITE_INTERNAL" "The SQLITE_INTERNAL result code indicates an internal malfunction. In a working version of SQLite, an application should never see this result code. If application does encounter this result code, it shows that there is a bug in the database engine.\n\nSQLite does not currently generate this result code. However, application-defined SQL functions or virtual tables, or VFSes, or other extensions might cause this result code to be returned.")
            (list 3 "SQLITE_PERM" "The SQLITE_PERM result code indicates that the requested access mode for a newly created database could not be provided.")
            (list 4 "SQLITE_ABORT" "The SQLITE_ABORT result code indicates that an operation was aborted prior to completion, usually be application request. See also: SQLITE_INTERRUPT.\n\nIf the callback function to sqlite3_exec() returns non-zero, then sqlite3_exec() will return SQLITE_ABORT.\n\nIf a ROLLBACK operation occurs on the same database connection as a pending read or write, then the pending read or write may fail with an SQLITE_ABORT or SQLITE_ABORT_ROLLBACK error.\n\nIn addition to being a result code, the SQLITE_ABORT value is also used as a conflict resolution mode returned from the sqlite3_vtab_on_conflict() interface.")
            (list 5 "SQLITE_BUSY" "The SQLITE_BUSY result code indicates that the database file could not be written (or in some cases read) because of concurrent activity by some other database connection, usually a database connection in a separate process.\n\nFor example, if process A is in the middle of a large write transaction and at the same time process B attempts to start a new write transaction, process B will get back an SQLITE_BUSY result because SQLite only supports one writer at a time. Process B will need to wait for process A to finish its transaction before starting a new transaction. The sqlite3_busy_timeout() and sqlite3_busy_handler() interfaces and the busy_timeout pragma are available to process B to help it deal with SQLITE_BUSY errors.\n\nAn SQLITE_BUSY error can occur at any point in a transaction: when the transaction is first started, during any write or update operations, or when the transaction commits. To avoid encountering SQLITE_BUSY errors in the middle of a transaction, the application can use BEGIN IMMEDIATE instead of just BEGIN to start a transaction. The BEGIN IMMEDIATE command might itself return SQLITE_BUSY, but if it succeeds, then SQLite guarantees that no subsequent operations on the same database through the next COMMIT will return SQLITE_BUSY.\n\nSee also: SQLITE_BUSY_RECOVERY and SQLITE_BUSY_SNAPSHOT.\n\nThe SQLITE_BUSY result code differs from SQLITE_LOCKED in that SQLITE_BUSY indicates a conflict with a separate database connection, probably in a separate process, whereas SQLITE_LOCKED indicates a conflict within the same database connection (or sometimes a database connection with a shared cache).")
            (list 6 "SQLITE_LOCKED" "The SQLITE_LOCKED result code indicates that a write operation could not continue because of a conflict within the same database connection or a conflict with a different database connection that uses a shared cache.\n\nFor example, a DROP TABLE statement cannot be run while another thread is reading from that table on the same database connection because dropping the table would delete the table out from under the concurrent reader.\n\nThe SQLITE_LOCKED result code differs from SQLITE_BUSY in that SQLITE_LOCKED indicates a conflict on the same database connection (or on a connection with a shared cache) whereas SQLITE_BUSY indicates a conflict with a different database connection, probably in a different process.")
            (list 7 "SQLITE_NOMEM" "The SQLITE_NOMEM result code indicates that SQLite was unable to allocate all the memory it needed to complete the operation. In other words, an internal call to sqlite3_malloc() or sqlite3_realloc() has failed in a case where the memory being allocated was required in order to continue the operation.")
            (list 8 "SQLITE_READONLY" "The SQLITE_READONLY result code is returned when an attempt is made to alter some data for which the current database connection does not have write permission.")
            (list 9 "SQLITE_INTERRUPT" "The SQLITE_INTERRUPT result code indicates that an operation was interrupted by the sqlite3_interrupt() interface. See also: SQLITE_ABORT")
            (list 10 "SQLITE_IOERR" "The SQLITE_IOERR result code says that the operation could not finish because the operating system reported an I/O error.\n\nA full disk drive will normally give an SQLITE_FULL error rather than an SQLITE_IOERR error.\n\nThere are many different extended result codes for I/O errors that identify the specific I/O operation that failed.")
            (list 11 "SQLITE_CORRUPT" "The SQLITE_CORRUPT result code indicates that the database file has been corrupted. See the How To Corrupt Your Database Files for further discussion on how corruption can occur.")
            (list 12 "SQLITE_NOTFOUND" "The SQLITE_NOTFOUND result code is used in two contexts. SQLITE_NOTFOUND can be returned by the sqlite3_file_control() interface to indicate that the file control opcode passed as the third argument was not recognized by the underlying VFS. SQLITE_NOTFOUND can also be returned by the xSetSystemCall() method of an sqlite3_vfs object.\n\nThe SQLITE_NOTFOUND result code is also used internally by the SQLite implementation, but those internal uses are not exposed to the application.")
            (list 13 "SQLITE_FULL" "The SQLITE_FULL result code indicates that a write could not complete because the disk is full. Note that this error can occur when trying to write information into the main database file, or it can also occur when writing into temporary disk files.\n\nSometimes applications encounter this error even though there is an abundance of primary disk space because the error occurs when writing into temporary disk files on a system where temporary files are stored on a separate partition with much less space that the primary disk.")
            (list 14 "SQLITE_CANTOPEN" "The SQLITE_CANTOPEN result code indicates that SQLite was unable to open a file. The file in question might be a primary database file or on of several temporary disk files.")
            (list 15 "SQLITE_PROTOCOL" "The SQLITE_PROTOCOL result code indicates a problem with the file locking protocol used by SQLite. The SQLITE_PROTOCOL error is currently only returned when using WAL mode and attempting to start a new transaction. There is a race condition that can occur when two separate database connections both try to start a transaction at the same time in WAL mode. The loser of the race backs off and tries again, after a brief delay. If the same connection loses the locking race dozens of times over a span of multiple seconds, it will eventually give up and return SQLITE_PROTOCOL. The SQLITE_PROTOCOL error should appear in practice very, very rarely, and only when there are many separate processes all competing intensely to write to the same database.")
            (list 16 "SQLITE_EMPTY" "The SQLITE_EMPTY result code is not currently used.")
            (list 17 "SQLITE_SCHEMA" "The SQLITE_SCHEMA result code indicates that the database schema has changed. This result code can be returned from sqlite3_step() for a prepared statement that was generated using sqlite3_prepare() or sqlite3_prepare16(). If the database schema was changed by some other process in between the time that the statement was prepared and the time the statement was run, this error can result.\n\nIf a prepared statement is generated from sqlite3_prepare_v2() then the statement is automatically re-prepared if the schema changes, up to SQLITE_MAX_SCHEMA_RETRY times (default: 50). The sqlite3_step() interface will only return SQLITE_SCHEMA back to the application if the failure persists after these many retries.")
            (list 18 "SQLITE_TOOBIG" "The SQLITE_TOOBIG error code indicates that a string or BLOB was too large. The default maximum length of a string or BLOB in SQLite is 1,000,000,000 bytes. This maximum length can be changed at compile-time using the SQLITE_MAX_LENGTH compile-time option, or at run-time using the sqlite3_limit(db,SQLITE_LIMIT_LENGTH,...) interface. The SQLITE_TOOBIG error results when SQLite encounters a string or BLOB that exceeds the compile-time or run-time limit.\n\nThe SQLITE_TOOBIG error code can also result when an oversized SQL statement is passed into one of the sqlite3_prepare_v2() interfaces. The maximum length of an SQL statement defaults to a much smaller value of 1,000,000 bytes. The maximum SQL statement length can be set at compile-time using SQLITE_MAX_SQL_LENGTH or at run-time using sqlite3_limit(db,SQLITE_LIMIT_SQL_LENGTH,...).")
            (list 19 "SQLITE_CONSTRAINT" "The SQLITE_CONSTRAINT error code means that an SQL constraint violation occurred while trying to process an SQL statement. Additional information about the failed constraint can be found by consulting the accompanying error message (returned via sqlite3_errmsg() or sqlite3_errmsg16()) or by looking at the extended error code.")
            (list 20 "SQLITE_MISMATCH" "The SQLITE_MISMATCH error code indicates a datatype mismatch.\n\nSQLite is normally very forgiving about mismatches between the type of a value and the declared type of the container in which that value is to be stored. For example, SQLite allows the application to store a large BLOB in a column with a declared type of BOOLEAN. But in a few cases, SQLite is strict about types. The SQLITE_MISMATCH error is returned in those few cases when the types do not match.\n\nThe rowid of a table must be an integer. Attempt to set the rowid to anything other than an integer (or a NULL which will be automatically converted into the next available integer rowid) results in an SQLITE_MISMATCH error.")
            (list 21 "SQLITE_MISUSE" "The SQLITE_MISUSE return code might be returned if the application uses any SQLite interface in a way that is undefined or unsupported. For example, using a prepared statement after that prepared statement has been finalized might result in an SQLITE_MISUSE error.\n\nSQLite tries to detect misuse and report the misuse using this result code. However, there is no guarantee that the detection of misuse will be successful. Misuse detection is probabilistic. Applications should never depend on an SQLITE_MISUSE return value.\n\nIf SQLite ever returns SQLITE_MISUSE from any interface, that means that the application is incorrectly coded and needs to be fixed. Do not ship an application that sometimes returns SQLITE_MISUSE from a standard SQLite interface because that application contains potentially serious bugs.")
            (list 22 "SQLITE_NOLFS" "The SQLITE_NOLFS error can be returned on systems that do not support large files when the database grows to be larger than what the filesystem can handle. \"NOLFS\" stands for \"NO Large File Support\".")
            (list 23 "SQLITE_AUTH" "The SQLITE_AUTH error is returned when the authorizer callback indicates that an SQL statement being prepared is not authorized.")
            (list 24 "SQLITE_FORMAT" "The SQLITE_FORMAT error code is not currently used by SQLite.")
            (list 25 "SQLITE_RANGE" "The SQLITE_RANGE error indices that the parameter number argument to one of the sqlite3_bind routines or the column number in one of the sqlite3_column routines is out of range.")
            (list 26 "SQLITE_NOTADB" "When attempting to open a file, the SQLITE_NOTADB error indicates that the file being opened does not appear to be an SQLite database file.")
            (list 27 "SQLITE_NOTICE" "The SQLITE_NOTICE result code is not returned by any C/C++ interface. However, SQLITE_NOTICE (or rather one of its extended error codes) is sometimes used as the first argument in an sqlite3_log() callback to indicate that an unusual operation is taking place.")
            (list 28 "SQLITE_WARNING" "The SQLITE_WARNING result code is not returned by any C/C++ interface. However, SQLITE_WARNING (or rather one of its extended error codes) is sometimes used as the first argument in an sqlite3_log() callback to indicate that an unusual and possibly ill-advised operation is taking place.")
            (list 100 "SQLITE_ROW" "The SQLITE_ROW result code returned by sqlite3_step() indicates that another row of output is available.")
            (list 101 "SQLITE_DONE" "The SQLITE_DONE result code indicates that an operation has completed. The SQLITE_DONE result code is most commonly seen as a return value from sqlite3_step() indicating that the SQL statement has run to completion. But SQLITE_DONE can also be returned by other multi-step interfaces such as sqlite3_backup_step().")
            (list 256 "SQLITE_OK_LOAD_PERMANENTLY" "The sqlite3_load_extension() interface loads an extension into a single database connection. The default behavior is for that extension to be automatically unloaded when the database connection closes. However, if the extension entry point returns SQLITE_OK_LOAD_PERMANENTLY instead of SQLITE_OK, then the extension remains loaded into the process address space after the database connection closes. In other words, the xDlClose methods of the sqlite3_vfs object is not called for the extension when the database connection closes.\n\nThe SQLITE_OK_LOAD_PERMANENTLY return code is useful to loadable extensions that register new VFSes, for example.")
            (list 261 "SQLITE_BUSY_RECOVERY" "The SQLITE_BUSY_RECOVERY error code is an extended error code for SQLITE_BUSY that indicates that an operation could not continue because another process is busy recovering a WAL mode database file following a crash. The SQLITE_BUSY_RECOVERY error code only occurs on WAL mode databases.")
            (list 262 "SQLITE_LOCKED_SHAREDCACHE" "The SQLITE_LOCKED_SHAREDCACHE error code is an extended error code for SQLITE_LOCKED indicating that the locking conflict has occurred due to contention with a different database connection that happens to hold a shared cache with the database connection to which the error was returned. For example, if the other database connection is holding an exclusive lock on the database, then the database connection that receives this error will be unable to read or write any part of the database file unless it has the read_uncommitted pragma enabled.\n\nThe SQLITE_LOCKED_SHARECACHE error code works very much like the SQLITE_BUSY error code except that SQLITE_LOCKED_SHARECACHE is for separate database connections that share a cache whereas SQLITE_BUSY is for the much more common case of separate database connections that do not share the same cache. Also, the sqlite3_busy_handler() and sqlite3_busy_timeout() interfaces do not help in resolving SQLITE_LOCKED_SHAREDCACHE conflicts.")
            (list 264 "SQLITE_READONLY_RECOVERY" "The SQLITE_READONLY_RECOVERY error code is an extended error code for SQLITE_READONLY. The SQLITE_READONLY_RECOVERY error code indicates that a WAL mode database cannot be opened because the database file needs to be recovered and recovery requires write access but only read access is available.")
            (list 266 "SQLITE_IOERR_READ" "The SQLITE_IOERR_READ error code is an extended error code for SQLITE_IOERR indicating an I/O error in the VFS layer while trying to read from a file on disk. This error might result from a hardware malfunction or because a filesystem came unmounted while the file was open.")
            (list 267 "SQLITE_CORRUPT_VTAB" "The SQLITE_CORRUPT_VTAB error code is an extended error code for SQLITE_CORRUPT used by virtual tables. A virtual table might return SQLITE_CORRUPT_VTAB to indicate that content in the virtual table is corrupt.")
            (list 270 "SQLITE_CANTOPEN_NOTEMPDIR" "The SQLITE_CANTOPEN_NOTEMPDIR error code is no longer used.")
            (list 275 "SQLITE_CONSTRAINT_CHECK" "The SQLITE_CONSTRAINT_CHECK error code is an extended error code for SQLITE_CONSTRAINT indicating that a CHECK constraint failed.")
            (list 283 "SQLITE_NOTICE_RECOVER_WAL" "The SQLITE_NOTICE_RECOVER_WAL result code is passed to the callback of sqlite3_log() when a WAL mode database file is recovered.")
            (list 284 "SQLITE_WARNING_AUTOINDEX" "The SQLITE_WARNING_AUTOINDEX result code is passed to the callback of sqlite3_log() whenever automatic indexing is used. This can serve as a warning to application designers that the database might benefit from additional indexes.")
            (list 516 "SQLITE_ABORT_ROLLBACK" "The SQLITE_ABORT_ROLLBACK error code is an extended error code for SQLITE_ABORT indicating that an SQL statement aborted because the transaction that was active when the SQL statement first started was rolled back. Pending write operations always fail with this error when a rollback occurs. A ROLLBACK will cause a pending read operation to fail only if the schema was changed within the transaction being rolled back.")
            (list 517 "SQLITE_BUSY_SNAPSHOT" "The SQLITE_BUSY_SNAPSHOT error code is an extended error code for SQLITE_BUSY that occurs on WAL mode databases when a database connection tries to promote a read transaction into a write transaction but finds that another database connection has already written to the database and thus invalidated prior reads.\n\nThe following scenario illustrates how an SQLITE_BUSY_SNAPSHOT error might arise:\n\n    Process A starts a read transaction on the database and does one or more SELECT statement. Process A keeps the transaction open.\n    Process B updates the database, changing values previous read by process A.\n    Process A now tries to write to the database. But process A's view of the database content is now obsolete because process B has modified the database file after process A read from it. Hence process A gets an SQLITE_BUSY_SNAPSHOT error. \n")
            (list 520 "SQLITE_READONLY_CANTLOCK" "The SQLITE_READONLY_CANTLOCK error code is an extended error code for SQLITE_READONLY. The SQLITE_READONLY_CANTLOCK error code indicates that SQLite is unable to obtain a read lock on a WAL mode database because the shared-memory file associated with that database is read-only.")
            (list 522 "SQLITE_IOERR_SHORT_READ" "The SQLITE_IOERR_SHORT_READ error code is an extended error code for SQLITE_IOERR indicating that a read attempt in the VFS layer was unable to obtain as many bytes as was requested. This might be due to a truncated file.")
            (list 526 "SQLITE_CANTOPEN_ISDIR" "The SQLITE_CANTOPEN_ISDIR error code is an extended error code for SQLITE_CANTOPEN indicating that a file open operation failed because the file is really a directory.")
            (list 531 "SQLITE_CONSTRAINT_COMMITHOOK" "The SQLITE_CONSTRAINT_COMMITHOOK error code is an extended error code for SQLITE_CONSTRAINT indicating that a commit hook callback returned non-zero that thus caused the SQL statement to be rolled back.")
            (list 539 "SQLITE_NOTICE_RECOVER_ROLLBACK" "The SQLITE_NOTICE_RECOVER_ROLLBACK result code is passed to the callback of sqlite3_log() when a hot journal is rolled back.")
            (list 776 "SQLITE_READONLY_ROLLBACK" "The SQLITE_READONLY_ROLLBACK error code is an extended error code for SQLITE_READONLY. The SQLITE_READONLY_ROLLBACK error code indicates that a database cannot be opened because it has a hot journal that needs to be rolled back but cannot because the database is readonly.")
            (list 778 "SQLITE_IOERR_WRITE" "The SQLITE_IOERR_WRITE error code is an extended error code for SQLITE_IOERR indicating an I/O error in the VFS layer while trying to write into a file on disk. This error might result from a hardware malfunction or because a filesystem came unmounted while the file was open. This error should not occur if the filesystem is full as there is a separate error code (SQLITE_FULL) for that purpose.")
            (list 782 "SQLITE_CANTOPEN_FULLPATH" "The SQLITE_CANTOPEN_FULLPATH error code is an extended error code for SQLITE_CANTOPEN indicating that a file open operation failed because the operating system was unable to convert the filename into a full pathname.")
            (list 787 "SQLITE_CONSTRAINT_FOREIGNKEY" "The SQLITE_CONSTRAINT_FOREIGNKEY error code is an extended error code for SQLITE_CONSTRAINT indicating that a foreign key constraint failed.")
            (list 1032 "SQLITE_READONLY_DBMOVED" "The SQLITE_READONLY_DBMOVED error code is an extended error code for SQLITE_READONLY. The SQLITE_READONLY_DBMOVED error code indicates that a database cannot be modified because the database file has been moved since it was opened, and so any attempt to modify the database might result in database corruption if the processes crashes because the rollback journal would not be correctly named.")
            (list 1034 "SQLITE_IOERR_FSYNC" "The SQLITE_IOERR_FSYNC error code is an extended error code for SQLITE_IOERR indicating an I/O error in the VFS layer while trying to flush previously written content out of OS and/or disk-control buffers and into persistent storage. In other words, this code indicates a problem with the fsync() system call in unix or the FlushFileBuffers() system call in windows.")
            (list 1038 "SQLITE_CANTOPEN_CONVPATH" "The SQLITE_CANTOPEN_CONVPATH error code is an extended error code for SQLITE_CANTOPEN used only by Cygwin VFS and indicating that the cygwin_conv_path() system call failed while trying to open a file. See also: SQLITE_IOERR_CONVPATH")
            (list 1043 "SQLITE_CONSTRAINT_FUNCTION" "The SQLITE_CONSTRAINT_FUNCTION error code is not currently used by the SQLite core. However, this error code is available for use by extension functions.")
            (list 1290 "SQLITE_IOERR_DIR_FSYNC" "The SQLITE_IOERR_DIR_FSYNC error code is an extended error code for SQLITE_IOERR indicating an I/O error in the VFS layer while trying to invoke fsync() on a directory. The unix VFS attempts to fsync() directories after creating or deleting certain files to ensure that those files will still appear in the filesystem following a power loss or system crash. This error code indicates a problem attempting to perform that fsync().")
            (list 1299 "SQLITE_CONSTRAINT_NOTNULL" "The SQLITE_CONSTRAINT_NOTNULL error code is an extended error code for SQLITE_CONSTRAINT indicating that a NOT NULL constraint failed.")
            (list 1546 "SQLITE_IOERR_TRUNCATE" "The SQLITE_IOERR_TRUNCATE error code is an extended error code for SQLITE_IOERR indicating an I/O error in the VFS layer while trying to truncate a file to a smaller size.")
            (list 1555 "SQLITE_CONSTRAINT_PRIMARYKEY" "The SQLITE_CONSTRAINT_PRIMARYKEY error code is an extended error code for SQLITE_CONSTRAINT indicating that a PRIMARY KEY constraint failed.")
            (list 1802 "SQLITE_IOERR_FSTAT" "The SQLITE_IOERR_FSTAT error code is an extended error code for SQLITE_IOERR indicating an I/O error in the VFS layer while trying to invoke fstat() (or the equivalent) on a file in order to determine information such as the file size or access permissions.")
            (list 1811 "SQLITE_CONSTRAINT_TRIGGER" "The SQLITE_CONSTRAINT_TRIGGER error code is an extended error code for SQLITE_CONSTRAINT indicating that a RAISE function within a trigger fired, causing the SQL statement to abort.")
            (list 2058 "SQLITE_IOERR_UNLOCK" "The SQLITE_IOERR_UNLOCK error code is an extended error code for SQLITE_IOERR indicating an I/O error within xUnlock method on the sqlite3_io_methods object.")
            (list 2067 "SQLITE_CONSTRAINT_UNIQUE" "The SQLITE_CONSTRAINT_UNIQUE error code is an extended error code for SQLITE_CONSTRAINT indicating that a UNIQUE constraint failed.")
            (list 2314 "SQLITE_IOERR_RDLOCK" "The SQLITE_IOERR_UNLOCK error code is an extended error code for SQLITE_IOERR indicating an I/O error within xLock method on the sqlite3_io_methods object while trying to obtain a read lock.")
            (list 2323 "SQLITE_CONSTRAINT_VTAB" "The SQLITE_CONSTRAINT_VTAB error code is not currently used by the SQLite core. However, this error code is available for use by application-defined virtual tables.")
            (list 2570 "SQLITE_IOERR_DELETE" "The SQLITE_IOERR_UNLOCK error code is an extended error code for SQLITE_IOERR indicating an I/O error within xDelete method on the sqlite3_vfs object.")
            (list 2579 "SQLITE_CONSTRAINT_ROWID" "The SQLITE_CONSTRAINT_ROWID error code is an extended error code for SQLITE_CONSTRAINT indicating that a rowid is not unique.")
            (list 2826 "SQLITE_IOERR_BLOCKED" "The SQLITE_IOERR_BLOCKED error code is no longer used.")
            (list 3082 "SQLITE_IOERR_NOMEM" "The SQLITE_IOERR_NOMEM error code is sometimes returned by the VFS layer to indicate that an operation could not be completed due to the inability to allocate sufficient memory. This error code is normally converted into SQLITE_NOMEM by the higher layers of SQLite before being returned to the application.")
            (list 3338 "SQLITE_IOERR_ACCESS" "The SQLITE_IOERR_ACCESS error code is an extended error code for SQLITE_IOERR indicating an I/O error within the xAccess method on the sqlite3_vfs object.")
            (list 3594 "SQLITE_IOERR_CHECKRESERVEDLOCK" "The SQLITE_IOERR_CHECKRESERVEDLOCK error code is an extended error code for SQLITE_IOERR indicating an I/O error within the xCheckReservedLock method on the sqlite3_io_methods object.")
            (list 3850 "SQLITE_IOERR_LOCK" "The SQLITE_IOERR_LOCK error code is an extended error code for SQLITE_IOERR indicating an I/O error in the advisory file locking logic. Usually an SQLITE_IOERR_LOCK error indicates a problem obtaining a PENDING lock. However it can also indicate miscellaneous locking errors on some of the specialized VFSes used on Macs.")
            (list 4106 "SQLITE_IOERR_CLOSE" "The SQLITE_IOERR_ACCESS error code is an extended error code for SQLITE_IOERR indicating an I/O error within the xClose method on the sqlite3_io_methods object.")
            (list 4362 "SQLITE_IOERR_DIR_CLOSE" "The SQLITE_IOERR_DIR_CLOSE error code is no longer used.")
            (list 4618 "SQLITE_IOERR_SHMOPEN" "The SQLITE_IOERR_SHMOPEN error code is an extended error code for SQLITE_IOERR indicating an I/O error within the xShmMap method on the sqlite3_io_methods object while trying to open a new shared memory segment.")
            (list 4874 "SQLITE_IOERR_SHMSIZE" "The SQLITE_IOERR_SHMSIZE error code is an extended error code for SQLITE_IOERR indicating an I/O error within the xShmMap method on the sqlite3_io_methods object while trying to resize an existing shared memory segment.")
            (list 5130 "SQLITE_IOERR_SHMLOCK" "The SQLITE_IOERR_SHMLOCK error code is no longer used.")
            (list 5386 "SQLITE_IOERR_SHMMAP" "The SQLITE_IOERR_SHMMAP error code is an extended error code for SQLITE_IOERR indicating an I/O error within the xShmMap method on the sqlite3_io_methods object while trying to map a shared memory segment into the process address space.")
            (list 5642 "SQLITE_IOERR_SEEK" "The SQLITE_IOERR_SEEK error code is an extended error code for SQLITE_IOERR indicating an I/O error within the xRead or xWrite methods on the sqlite3_io_methods object while trying to seek a file descriptor to the beginning point of the file where the read or write is to occur.")
            (list 5898 "SQLITE_IOERR_DELETE_NOENT" "The SQLITE_IOERR_DELETE_NOENT error code is an extended error code for SQLITE_IOERR indicating that the xDelete method on the sqlite3_vfs object failed because the file being deleted does not exist.")
            (list 6154 "SQLITE_IOERR_MMAP" "The SQLITE_IOERR_MMAP error code is an extended error code for SQLITE_IOERR indicating an I/O error within the xFetch or xUnfetch methods on the sqlite3_io_methods object while trying to map or unmap part of the database file into the process address space.")
            (list 6410 "SQLITE_IOERR_GETTEMPPATH" "The SQLITE_IOERR_GETTEMPPATH error code is an extended error code for SQLITE_IOERR indicating that the VFS is unable to determine a suitable directory in which to place temporary files.")
            (list 6666 "SQLITE_IOERR_CONVPATH" "The SQLITE_IOERR_CONVPATH error code is an extended error code for SQLITE_IOERR used only by Cygwin VFS and indicating that the cygwin_conv_path() system call failed. See also: SQLITE_CANTOPEN_CONVPATH\n")))))

(enable-unquoting
 (begin
   ,@(map (lambda (e)
            (let.-static (sqlite3-error-static. (code name) e)
                         `(def ,name ,code)))
          sqlite3-errors)))

(def sqlite3-error<-code (list->table (map (lambda (e)
                                             (cons (.code e) e))
                                           sqlite3-errors)))

(def (code->sqlite3-error [fixnum? code])
     (table-ref sqlite3-error<-code code))


(def (raise-sqlite3-error-from-code-or-pair err-or-pair)
     (let ((cont (lambda (code #!optional maybe-msg)
                   (let ((err (code->sqlite3-error code)))
                     (raise (if maybe-msg
                                (sqlite3-error/message err maybe-msg)
                                err))))))
       (cond ((fixnum? err-or-pair)
              (cont err-or-pair))
             ((pair? err-or-pair)
              (cont (car err-or-pair) (cdr err-or-pair)))
             (else
              (error "BUG" err-or-pair)))))

;; bad name since it throws an exception otherwise.  how is this ?

;; (defmacro (when-sqlite3-error-OK err . body)
;;   `(let ((,E ,err))
;;      (if (= ,E SQLITE_OK)
;;          (begin ,@body)
;;          (raise-sqlite3-error-from-code-or-pair ,E))))

;; no big like for when forms really, since it is even looking
;; confusing isn't it (test isn't separate from body forms). Thus:

;; ditto
(defmacro (if-sqlite3-error-OK err then)
  (with-gensym E
               `(let ((,E ,err))
                  (if (eq? ,E SQLITE_OK)
                      ,then
                      (raise-sqlite3-error-from-code-or-pair ,E)))))

;; Convert fixnum error code into symbol representing the error if one
;; of the given accepted names; otherwise or if err is a pair, it is
;; passed to |raise-sqlite3-error-from-code-or-pair| (throwing an
;; exception). This is a macro since that's the only way to have fast
;; code -> symbol lookup (for the few needed values).
(defmacro (sqlite3-error-return err accepted-constant-names)
  (with-gensym
   E
   `(let ((,E ,err))
      (cond ,@(assert* (list-of (source-of symbol?)) accepted-constant-names
                       (lambda (accepted-constant-names)
                         (map (lambda (accepted-constant-name)
                                `((eq? ,E ,accepted-constant-name)
                                  ',accepted-constant-name))
                              accepted-constant-names)))
            (else
             (raise-sqlite3-error-from-code-or-pair ,E))))))

