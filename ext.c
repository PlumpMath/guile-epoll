#include <libguile.h>

#include <sys/epoll.h>
#include <errno.h>
#include <assert.h>
#include <stdbool.h>

// this should be global since signals always affect the entire process
sigset_t g_blocked_signals;

bool g_edge_triggering = true;

static SCM scm_epoll_cripple(SCM crippled) {
  if(crippled == SCM_BOOL_F) {
    g_edge_triggering = true;
  } else {
    g_edge_triggering = false;
  }
}

static SCM scm_epoll_signals(SCM in_signals) {
  assert(scm_list_p(in_signals));
  sigemptyset(&g_blocked_signals);
  while(!scm_null_p(in_signals)) {
    int signal = scm_to_int(scm_car(in_signals));
    sigaddset(&g_blocked_signals,signal);
    in_signals = scm_cdr(in_signals);
  }
}


static SCM scm_epoll_wait(SCM in_epfd, SCM in_timeout, SCM in_maxevents) {
  int epfd = scm_to_int(in_epfd);
  int maxevents;
  int timeout;
  if(in_timeout == SCM_UNDEFINED) {
    timeout = 0;
  } else {
    timeout = scm_to_int(in_timeout);
  }
  if(in_maxevents == SCM_UNDEFINED) {
    maxevents = 0x20;
  } else {
    maxevents = scm_to_int(in_maxevents);
  }
  static struct epoll_event* events = NULL;
  events = realloc(events,maxevents);
  int res = epoll_pwait(epfd,events,maxevents,timeout,&g_blocked_signals);
  if(res < 0) {
    SCM args = scm_list_1(scm_from_int(errno));
    scm_throw(scm_from_utf8_symbol("epoll"),args);
    perror("Shouldn't get here!");
    exit(23);
  }
  SCM result = SCM_EOL;
  int i;
  for(i=0;i<res;++i) {
    SCM entry = scm_cons(scm_from_uint32(events[i].data.fd),
                         scm_from_uint32(events[i].events));
    result = scm_cons(entry,result);
  }
  return result;
}

static SCM scm_epoll_ctl(SCM epfd, SCM in_op, SCM fd, SCM in_events) {
  int op = scm_to_int(in_op);

  assert(scm_number_p(in_events));
  static struct epoll_event event = {};
  if (in_events == SCM_UNDEFINED) {
      if(op != EPOLL_CTL_DEL) {
        scm_throw(scm_from_utf8_symbol("epoll"),
                  scm_from_utf8_string("Either specify EPOLLIN, EPOLLOUT, or both!"));
        exit(23);
      }
  } else {
    if(op == EPOLL_CTL_DEL) {
      scm_write_line
        (
         scm_from_utf8_string
         (
          "WARNING: deletion ignores any event specifications. use EPOLL_CTL_MOD to remove some events."),
         SCM_UNDEFINED);
    } else {
      event.data.fd = scm_to_int(fd);
      event.events = scm_to_uint32(in_events);
      if(g_edge_triggering) {
        // may as well just use select() if you don't have this :p
        event.events |= EPOLLET;
      }
    }
  } // by default, clears all events (zero)

  int res = epoll_ctl(scm_to_int(epfd),
                      op,
                      scm_to_int(fd),
                      &event);
  if(res < 0) {
    scm_throw(scm_from_utf8_symbol("epoll"),scm_list_1(scm_from_int(errno)));
    exit(23);
  }
  return scm_from_int(res);
}

static SCM scm_epoll_create(SCM cloexec_p) {
  int flags;
  if(cloexec_p == SCM_UNDEFINED) {
    flags = 0;
  } else if(cloexec_p == SCM_BOOL_T) {
    flags = EPOLL_CLOEXEC;
  }
  int epfd = epoll_create1(flags);
  if(epfd < 0) {
    scm_throw(scm_from_utf8_symbol("epoll"),scm_list_1(scm_from_int(errno)));
    exit(23);
  }
  return scm_from_int(epfd);
}

void scm_epoll_init() {
  puts("DERP");
  SCM sym;
#define D(name) { sym = scm_from_utf8_symbol(#name); \
      puts("Define " #name);                         \
      scm_define(sym,scm_from_int(name));            \
    }
  D(EPOLLONESHOT);
  D(EPOLLWAKEUP); // for autosleep
  D(EPOLL_CLOEXEC);

  D(EPOLLIN);
  D(EPOLLOUT);
  D(EPOLLERR);
  D(EPOLLET);

  D(EPOLL_CTL_ADD);
  D(EPOLL_CTL_MOD);
  D(EPOLL_CTL_DEL);

  fflush(stdout);

  scm_c_define_gsubr("epoll-signals",0,0,1,scm_epoll_signals);
  scm_c_define_gsubr("epoll-wait",1,2,0,scm_epoll_wait);
  scm_c_define_gsubr("epoll-ctl",3,1,0,scm_epoll_ctl);
  scm_c_define_gsubr("epoll-create",0,1,0,scm_epoll_create);
  scm_c_define_gsubr("epoll-cripple",0,1,0,scm_epoll_cripple);

}
