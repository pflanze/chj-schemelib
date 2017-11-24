// cj Fri, 27 Jan 2006 19:18:46 +0100

// A fixed-size queue which doesn't allocate memory.
// an interrupt safe queue (well, no thread locking is done yet - but it doesn't allocate memory or change global data in an unsafe way, I hope.)


struct sigqueue_entry {
    int signo;
};


#define bool int
#define true 1
#define false 0

/* the following constant actually has to be one greater than the
   maximally possible load, to make it possible to differentiate
   between fullness and emptyness */

#define SIGQUEUE_LENGTH 10000  /* big enough so as to not overflow on signal storms */
/* 1000 has not been enough when being tested with this:
   perl -we '$pid= 23892; for(1..1e6) {kill "USR1",$pid; kill "USR2",$pid; kill "USR1",$pid; kill "USR2",$pid; kill "USR2",$pid; }'
   10'000 has been ok (the end size of the output file has been 4812800, so about 3.8% of the signals have been lost; but unix doesn't guarantee delivery of the full number of URS1/2 signals.)
*/
   

struct sigqueue {
    int startpos;
    int endpos;
    bool overflow;
    struct sigqueue_entry data[SIGQUEUE_LENGTH];
};

//static struct sigqueue *q;

//static int
//init_queue () {

#include <stdlib.h>
#include <stdio.h>


#define SIGQUEUE_ERROR -1
#define SIGQUEUE_SUCCESS 0
typedef int int_or_error;
typedef int status;


static struct sigqueue *
make_sigqueue () {
    struct sigqueue* q= (struct sigqueue*)malloc(sizeof(struct sigqueue));
    if (q) {
	q->startpos=0;
	q->endpos=0;
	q->overflow=false;
    }
    return q;
}

static int
sigqueue_usage (struct sigqueue *q) {
    if (q->startpos <= q->endpos) {
	return q->endpos - q->startpos;
    } else {
	return q->endpos + SIGQUEUE_LENGTH - q->startpos;
    }
}

static bool
sigqueue_isfull (struct sigqueue *q) {
    return (sigqueue_usage(q) == (SIGQUEUE_LENGTH-1));
}
static bool
sigqueue_isempty (struct sigqueue *q) {
    return (q->startpos == q->endpos);
}

static status
sigqueue_add (struct sigqueue *q, int signo) {
    if (sigqueue_isfull(q)) {
	q->overflow=true;
	return SIGQUEUE_ERROR;
    } else {
	q->data[q->endpos].signo=signo;
	q->endpos++;
	if (q->endpos == SIGQUEUE_LENGTH) {
	    q->endpos=0;
	}
	return SIGQUEUE_SUCCESS;
    }
}

static int_or_error
sigqueue_ref_signo (struct sigqueue * q) {
    if (sigqueue_isempty(q)) {
	return SIGQUEUE_ERROR;
    } else {
	return q->data[q->startpos].signo;
    }
}

static status
sigqueue_remove (struct sigqueue * q) {
    if (sigqueue_isempty(q)) {
	return SIGQUEUE_ERROR;
    } else {
	q->startpos++;
	if (q->startpos == SIGQUEUE_LENGTH) {
	    q->startpos=0;
	}
	return SIGQUEUE_SUCCESS;
    }
}


//static void
//sigqueue_release (struct sigqueue*q) {
static ___SCMOBJ
sigqueue_release (void*q) {
    //fprintf(stderr,"releasing %p\n",q);
    free(q);
    return ___FIX(___NO_ERR);
}


/* --- interfacing scheme with handler setup functions: -------
 */

#include <signal.h>

static struct sigqueue * global_queue;
static sigset_t set_of_all_handlers;
#include <errno.h>
static int sig_errno=0;

static void
handler (int sig)
{
    sigset_t saved_set;
    // since I'm too lazy to change all the signal() calls to be sure to block all others, I do this:
    // [btw: do we have to add SIGINT to set_of_all_handlers? no , not until we install_signal on it and then it's been taken care of. ok?]
    if (0== sigprocmask(SIG_BLOCK, &set_of_all_handlers, &saved_set)) {
	sigqueue_add(global_queue, sig);
	if (0== sigprocmask(SIG_SETMASK, &saved_set, 0)) {
	    /* and, regardless of result of sigqueue_add: (since
	       scheme will still have to look at it) */
	    ___EXT(___raise_interrupt) (___INTR_7);
	} else {
	    fprintf(stderr,"WARNING from signal handler: got error2\n");
	}
    } else {
	fprintf(stderr,"WARNING from signal handler: got error1\n");
    }
}

static status
install_signal (int sig)
{
    if (signal (sig, handler) != SIG_ERR) {
	if (sigaddset(&set_of_all_handlers, sig)>=0) {
	    return SIGQUEUE_SUCCESS;
	} else {
	    sig_errno=errno;
	    return SIGQUEUE_ERROR;
	}
    } else {
	sig_errno=errno;
	return SIGQUEUE_ERROR;
    }
}

static status
uninstall_signal (int sig)
{
    if (signal (sig, SIG_DFL) != SIG_ERR) {
	if (sigdelset(&set_of_all_handlers, sig)>=0) {
	    return SIGQUEUE_SUCCESS;
	} else {
	    sig_errno=errno;
	    return SIGQUEUE_ERROR;
	}
    } else {
	sig_errno=errno;
	return SIGQUEUE_ERROR;
    }
}


/* --- process-wide locking by masking interrupts: -------

 Masking signals is the only safe way to prevent signals from changing
 the queue while scheme is reading it. (I think semaphores would risk
 deadlocks)
 
 (And, since we're not using multiple scheme pthreads, no semapthores
 for locking out other threads is necessary.)

 int sigprocmask(int how, const sigset_t *set, sigset_t *oldset);
*/

#include <signal.h>

static sigset_t saved_set;
static bool is_locked=false;

#define SIGQUEUE_ERROR2 -2
    
static status
sig_lock() {
    if (is_locked) {
	return SIGQUEUE_ERROR;
    } else {
	//sigset_t newset;
	// mask which signals? all? no, segv and such must still be delivered. well:
	// all the signal handlers which have been set by us.
	if (0== sigprocmask(SIG_BLOCK, &set_of_all_handlers, &saved_set)) {
	    is_locked=true;
	    return SIGQUEUE_SUCCESS;
	} else {
	    sig_errno=errno;
	    return SIGQUEUE_ERROR2;
	}
    }
}

static status
sig_unlock() {
    if (is_locked) {
	if (0== sigprocmask(SIG_SETMASK, &saved_set, 0)) {
	    is_locked=false;
	    return SIGQUEUE_SUCCESS;
	} else {
	    sig_errno=errno;
	    return SIGQUEUE_ERROR2;
	}
    } else {
	return SIGQUEUE_ERROR;
    }
}

#include <string.h>

static char*
sig_errstr() {
    return strerror(sig_errno);
}

/* ---- init: ------------ */

static status
init() {
    sigemptyset(&set_of_all_handlers);
    //global_queue= make_sigqueue() || return SIGQUEUE_ERROR; we are not in perl :/
    if (!(global_queue= make_sigqueue()))
	return SIGQUEUE_ERROR;
    return SIGQUEUE_SUCCESS;
}
