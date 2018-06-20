;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Automatically run GDB when getting a segmentation violation (or
;; other system/C level unhandled exception).

(require easy
	 test)

(export)

(include "cj-standarddeclares.scm")

;; Inspired by https://github.com/kou/segv-handler-gdb

;; Relies on procfs and hence only works on Linux.

(c-declare "
#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>

static struct sigaction original_segv_action;


static void
auto_gdb_handler(int signum) {
    pid_t errpid= getpid();
    pid_t pid= fork();
    if (pid < 0) {
        perror(\"fork\");
    }
    if (pid) {
        int wstatus;
        waitpid(pid, &wstatus, 0);
        // XX instead call original_segv_action.sa_handler (in
        // the original context? How?)
        sigaction(SIGSEGV, &original_segv_action, NULL);
        kill(errpid, SIGSEGV);
    } else {
        char pidstr[100];
        snprintf(pidstr, 100, \"%i\", errpid);
        pidstr[99]=0;
        // don't have access to argv, hence have to retrieve /proc/$$/exe
#define EXEPATH_SIZE 500
        char exepath[EXEPATH_SIZE];
        {
            char procpath[200];
            snprintf(procpath, 200, \"/proc/%s/exe\", pidstr);
            ssize_t i= readlink(procpath, exepath, EXEPATH_SIZE-1);
            if (i < 0) {
                perror(\"readlink\");
            }
            if (i >= (EXEPATH_SIZE-1)) {
                fprintf(stderr, \"readlink: buffer too short, truncated\\n\");
                fclose(stderr);
                _exit(77);
            }
            exepath[i]=0;
        }
        char* gdbpath= \"gdb\";
        char* cmd[]= { gdbpath, exepath, pidstr, NULL };
        {
            char** p= cmd;
            fprintf(stderr, \"auto-gdb: handling SEGV: running\");
            while(*p) {
                fprintf(stderr, \" '%s'\", *p);
                p++;
            }
            fprintf(stderr, \"\\n\");
        }
        execvp(gdbpath, cmd);
        perror(\"execlp\");
        fclose(stderr);
        _exit(77);
    }
}

")

(def (auto-gdb:init)
     (or (##c-code "
/* not left to avoid breaking emacs' indenter */  {
    struct sigaction segv_action;
    segv_action.sa_handler = auto_gdb_handler;
    sigemptyset(&(segv_action.sa_mask));
    segv_action.sa_flags = 0;
    if (sigaction(SIGSEGV, &segv_action, &original_segv_action) == -1) {
        ___RESULT= ___FAL;
    } else {
        ___RESULT= ___TRU;
    }
}
")
	 (error "could not set SEGV handler")))

(auto-gdb:init)
