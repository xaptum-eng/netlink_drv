/*-------------------------------------------------------------------------------------------
//
// XAPTUM CONFIDENTIAL
// __________________
//
//  2018(C) Xaptum, Inc.
//  All Rights Reserved.Patents Pending.
//
// NOTICE:  All information contained herein is, and remains
// the property of Xaptum, Inc.  The intellectual and technical concepts contained
// herein are proprietary to Xaptum, Inc and may be covered by U.S. and Foreign Patents,
// patents in process, and are protected by trade secret or copyright law.
// Dissemination of this information or reproduction of this material
// is strictly forbidden unless prior written permission is obtained
// from Xaptum, Inc.
//
// @author Venkatakumar Srinivasan
//
//-------------------------------------------------------------------------------------------*/
#include <erl_driver.h>

#include <ei.h>
#include <errno.h>
#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <net/if.h>
#include <linux/if.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <linux/rtnetlink.h>

#define BUFFSIZE 8192

#define ERR_READ 10
#define ERR_READ_HEADER 11
#define ERR_PACKET_SIZE 12

#define ERROR "error"

#define NETLINK_CREATE_ERROR "netlink_create_error"
#define NETLINK_BIND_ERROR "netlink_bind_error"
#define NETLINK_READ_ERROR "netlink_read_error"
#define NETLINK_NO_DATA_ERROR "netlink_no_data_error"
#define NETLINK_SENDER_ERROR "netlink_sender_address_error"
#define NETLINK_MSG_LEN_ERROR "netlink_message_length_error"

typedef struct {
  char eibuf[BUFFSIZE];
  int sockfd;
  struct sockaddr_nl *local; // local addr struct
  ErlDrvPort port;
} nldrv_data_t;


static ErlDrvData drv_start(ErlDrvPort port, char *command);

static void drv_stop(ErlDrvData handle);

static void drv_output(ErlDrvData handle, char *buff, ErlDrvSizeT bufflen);


static ErlDrvEntry nldrv_entry = {
    NULL,			/* F_PTR init, called when driver is loaded */
    drv_start,   		/* L_PTR start, called when port is opened */
    drv_stop,	        	/* F_PTR stop, called when port is closed */
    drv_output,		        /* F_PTR output, called when erlang has sent */
    NULL,			/* F_PTR ready_input, called when input descriptor ready */
    NULL,			/* F_PTR ready_output, called when output descriptor ready */
    "nldrv",		        /* char *driver_name, the argument to open_port */
    NULL,			/* F_PTR finish, called when unloaded */
    NULL,                       /* void *handle, Reserved by VM */
    NULL,			/* F_PTR control, port_command callback */
    NULL,			/* F_PTR timeout, reserved */
    NULL,			/* F_PTR outputv, reserved */
    NULL,                       /* F_PTR ready_async, only for async drivers */
    NULL,                       /* F_PTR flush, called when port is about to be closed, but there is data in driver queue */
    NULL,                       /* F_PTR call, much like control, sync call to driver */
    NULL,                       /* F_PTR event, called when an event selected by driver_event() occurs. */
    ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be set to this value */
    ERL_DRV_FLAG_USE_PORT_LOCKING,  /* int driver_flags, see documentation */
    NULL,                           /* void *handle2, reserved for VM use */
    NULL,                           /* F_PTR process_exit, called when a monitored process dies */
    NULL                            /* F_PTR stop_select, called to close an event object */
};

DRIVER_INIT(nldrv) /* must match name in driver_entry */
{
    return &nldrv_entry;
}

static int encode_error_tuple(char *rbuf, const char *error) {
  int index = 0;
  ei_encode_version(rbuf, &index);
  ei_encode_tuple_header(rbuf, &index, 2);
  ei_encode_atom(rbuf, &index, ERROR);
  ei_encode_atom(rbuf, &index, error);
  return index;
}

static int encode_rtm_route(char *rbuf) {
  int index = 0;
  ei_encode_version(rbuf, &index);
  ei_encode_atom(rbuf, &index, "rtchange");
  return index;
}

static int encode_rtm_deladdr(char *rbuf, char *ifName) {
  int index = 0;
  ei_encode_version(rbuf, &index);
  ei_encode_tuple_header(rbuf, &index, 2);
  ei_encode_atom(rbuf, &index, "deladdr");
  ei_encode_string(rbuf, &index, ifName);
  return index;
}

static int encode_rtm_dellink(char *rbuf, char *ifName) {
  int index = 0;
  ei_encode_version(rbuf, &index);
  ei_encode_tuple_header(rbuf, &index, 2);
  ei_encode_atom(rbuf, &index, "dellink");
  ei_encode_string(rbuf, &index, ifName);
  return index;
}

static int encode_rtm_newlink(char *rbuf, char *ifName, const char *ifUpp, const char *ifRunn) {
  int index = 0;
  ei_encode_version(rbuf, &index);
  ei_encode_tuple_header(rbuf, &index, 4);
  ei_encode_atom(rbuf, &index, "newlink");
  ei_encode_string(rbuf, &index, ifName);
  ei_encode_atom(rbuf, &index, ifUpp);
  ei_encode_atom(rbuf, &index, ifRunn);
  
  return index;
}

static int encode_rtm_newaddr(char *rbuf, char *ifName, char *ifAddress) {
  int index = 0;
  ei_encode_version(rbuf, &index);
  ei_encode_tuple_header(rbuf, &index, 3);
  ei_encode_atom(rbuf, &index, "newaddr");
  ei_encode_string(rbuf, &index, ifName);
  ei_encode_string(rbuf, &index, ifAddress);
  return index;
}

// little helper to parsing message using netlink macroses
static void parseRtattr(struct rtattr *tb[], int max, struct rtattr *rta, int len) {
  memset(tb, 0, sizeof(struct rtattr *) * (max + 1));

  while (RTA_OK(rta, len)) {  // while not end of the message
    if (rta->rta_type <= max) {
      tb[rta->rta_type] = rta; // read attr
    }
    rta = RTA_NEXT(rta,len);    // get next attr
  }
}

static ErlDrvData drv_start(ErlDrvPort port, char *command) {
  /* allocated memory for driver data */
  nldrv_data_t* d = (nldrv_data_t *)driver_alloc(sizeof(nldrv_data_t));
  memset(d, 0, sizeof(nldrv_data_t));
  
  /* create netlink socket */
  int fd = socket(AF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
  if (fd < 0) {
    driver_failure_atom(port, NETLINK_CREATE_ERROR);
  }


  /* bind netlink socket */
  struct sockaddr_nl *local = (struct sockaddr_nl *)driver_alloc(sizeof(struct sockaddr_nl));  
  memset(local, 0, sizeof(struct sockaddr_nl));
  local->nl_family = AF_NETLINK;       // set protocol family
  local->nl_groups =   RTMGRP_LINK | RTMGRP_IPV4_IFADDR | RTMGRP_IPV4_ROUTE;   // set groups we interested in
  local->nl_pid = getpid();    // set out id using current process id

  // bind socket
  if (bind(fd, (struct sockaddr*)local, sizeof(struct sockaddr_nl)) < 0) { 
    close(fd);
    driver_free((char *)local);
    driver_failure_atom(port, NETLINK_BIND_ERROR);
  }
 

  /* initalize driver data struct */
  d->port = port;
  d->sockfd = fd;
  d->local = local;
  
  return (ErlDrvData)d;
}

static void drv_stop(ErlDrvData handle) {
  nldrv_data_t* d = (nldrv_data_t *)handle;

  // free struct sockaddr_nl
  if( d->local ) {
    driver_free((char *)d->local);
  }
  
  // close netlink socket
  if( d->sockfd > 0 ) {
    close( d->sockfd );
  }

  // free driver data
  driver_free((char *)handle);
}

static void drv_output(ErlDrvData handle, char *pbuff, ErlDrvSizeT bufflen) {
  nldrv_data_t* d = (nldrv_data_t *)handle;
  int eiSz = 0;

  char buf[BUFFSIZE];             // message buffer
  struct iovec iov;           // message structure
  iov.iov_base = buf;         // set message buffer as io
  iov.iov_len = sizeof(buf);  // set size

  // initialize protocol message header
  struct msghdr msg;
  {
    msg.msg_name = d->local;                  // local address
    msg.msg_namelen = sizeof(struct sockaddr_nl);        // address size
    msg.msg_iov = &iov;                     // io vector
    msg.msg_iovlen = 1;                     // io size
  }

  // read netlink message
  ssize_t status = recvmsg(d->sockfd, &msg, MSG_DONTWAIT);
  if (status < 0) {
    if (errno == EINTR || errno == EAGAIN) {
      eiSz = encode_error_tuple(d->eibuf, NETLINK_NO_DATA_ERROR);
      driver_output(d->port, d->eibuf, eiSz);
    } else {
      eiSz = encode_error_tuple(d->eibuf, NETLINK_READ_ERROR);
      driver_output(d->port, d->eibuf, eiSz);
    }
    return;
  }

  // check message length, just in case
  if (msg.msg_namelen != sizeof(struct sockaddr_nl)) { 
    eiSz = encode_error_tuple(d->eibuf, NETLINK_SENDER_ERROR);
    driver_output(d->port, d->eibuf, eiSz);
    return;
  }

  // message parser
  struct nlmsghdr *h;
  for (h = (struct nlmsghdr*)buf; status >= (ssize_t)sizeof(*h); ) {
    // read all messagess headers
      int len = h->nlmsg_len;
      int l = len - sizeof(*h);
      char *ifName = 0;

      if ((l < 0) || (len > status)) {
	eiSz = encode_error_tuple(d->eibuf, NETLINK_MSG_LEN_ERROR);
	driver_output(d->port, d->eibuf, eiSz);
	continue;
      }

      // now we can check message type
      if ((h->nlmsg_type == RTM_NEWROUTE) || (h->nlmsg_type == RTM_DELROUTE)) { // some changes in routing table
	eiSz = encode_rtm_route(d->eibuf);
	driver_output(d->port, d->eibuf, eiSz);
      } else {    // in other case we need to go deeper
	char *ifUpp;
	char *ifRunn;
	struct ifinfomsg *ifi;  // structure for network interface info
	struct rtattr *tb[IFLA_MAX + 1];

	ifi = (struct ifinfomsg*) NLMSG_DATA(h);    // get information about changed network interface

	parseRtattr(tb, IFLA_MAX, IFLA_RTA(ifi), h->nlmsg_len);  // get attributes
                
	if (tb[IFLA_IFNAME]) {  // validation
	  ifName = (char*)RTA_DATA(tb[IFLA_IFNAME]); // get network interface name
	}

	if (ifi->ifi_flags & IFF_UP) { // get UP flag of the network interface
	  ifUpp = (char*)"up";
	} else {
	  ifUpp = (char*)"down";
	}

	if (ifi->ifi_flags & IFF_RUNNING) { // get RUNNING flag of the network interface
	  ifRunn = (char*)"running";
	} else {
	  ifRunn = (char*)"not_running";
	}

	char ifAddress[256];    // network addr
	struct ifaddrmsg *ifa; // structure for network interface data
	struct rtattr *tba[IFA_MAX+1];

	ifa = (struct ifaddrmsg*)NLMSG_DATA(h); // get data from the network interface

	parseRtattr(tba, IFA_MAX, IFA_RTA(ifa), h->nlmsg_len);

	if (tba[IFA_LOCAL]) {
	  inet_ntop(AF_INET, RTA_DATA(tba[IFA_LOCAL]), ifAddress, sizeof(ifAddress)); // get IP addr
	}

	switch (h->nlmsg_type) { // what is actually happenned?
	case RTM_DELADDR:
	  eiSz = encode_rtm_deladdr(d->eibuf, ifName);
	  driver_output(d->port, d->eibuf, eiSz);
	  break;

	case RTM_DELLINK:
	  eiSz = encode_rtm_dellink(d->eibuf, ifName);
	  driver_output(d->port, d->eibuf, eiSz);
	  break;

	case RTM_NEWLINK:
	  eiSz = encode_rtm_newlink(d->eibuf, ifName, ifUpp, ifRunn);
	  driver_output(d->port, d->eibuf, eiSz);
	  break;

	case RTM_NEWADDR:
	  eiSz = encode_rtm_newaddr(d->eibuf, ifName, ifAddress);
	  driver_output(d->port, d->eibuf, eiSz);	  
	  break;
	}
      }

      status -= NLMSG_ALIGN(len); // align offsets by the message length, this is important

      h = (struct nlmsghdr*)((char*)h + NLMSG_ALIGN(len));    // get next message
    }
}
