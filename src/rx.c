#include "netif.h"

#include <assert.h>
#include <string.h>
#include <stdio.h> // XXX

#include "misc.h"
#include "yield.h"

uint16_t netdev_alloc_rxpkts(void *argp, struct uk_netbuf *nb[], uint16_t count)
{
  struct netif *netif = (struct netif*)argp;
  uint16_t i;

  for (i = 0; i < count; ++i) {
    nb[i] = netdev_alloc_rx_netbuf(netif);
    if (!nb[i]) {
      /* we run out of memory */
      break;
    }
  }

  uk_pr_debug("Allocated %d buffers\n", i);
  return i;
}


/* Returns
* -1 on error
*  0 on success
*  1 on success and more to process
*/
static int netdev_rx(struct netif* netif, uint8_t *buf, unsigned *size,
        const char **err)
{
  struct uk_netdev *dev = netif->dev;
  struct uk_netbuf *nb;
  unsigned bufsize = *size;
  int rc;

  uk_pr_debug("netdev_rx: buffer=%p, size=%lu)\n", buf, size); // XXX
  *size = 0;

  rc = uk_netdev_rx_one(dev, 0, &nb);
  uk_pr_debug("uk_netdev_rx_one: input status %d (%c%c%c)\n",
      rc,
      uk_netdev_status_test_set(rc, UK_NETDEV_STATUS_SUCCESS) ? 'S' : '-',
      uk_netdev_status_test_set(rc, UK_NETDEV_STATUS_MORE) ? 'M' : '-',
      uk_netdev_status_test_set(rc, UK_NETDEV_STATUS_UNDERRUN) ? 'U' : '-');

  if (rc < 0) {
    uk_pr_err("netdev_rx: failed to receive a packet\n");
    *err = "Failed to receive a packet";
    return -1;
  }

  if (!uk_netdev_status_successful(rc)) {
    uk_pr_info("netdev_rx: no packet received\n");
    *err = "No packet received";
    return 0;
  }

  bool more = uk_netdev_status_more(rc);
  if (!more) {
      uk_pr_debug("netdev_rx: no more to read\n"); // XXX
  }

  uk_pr_debug("netdev_rx: nb = %p, nb->data = %p, nb->len = %d, more = %d\n",
          nb, nb->data, nb->len, more);
 
#ifdef MNU_DEBUG
  printf("---- RX ----\n");
  debug_bytes(nb->data, nb->len);
  printf("------------\n");
#endif

  if (bufsize <= nb->len) {
    *err = "Not enough room in buffer to write packet";
    uk_netbuf_free_single(nb);
    return -1;
  }

  memcpy(buf, nb->data, nb->len);
  *size = nb->len;
  uk_netbuf_free_single(nb);

  return (more ? 1 : 0);
}


#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>


CAMLprim value uk_netdev_rx(value v_netif, value v_buf, value v_size)
{
  CAMLparam3(v_netif, v_buf, v_size);
  CAMLlocal1(v_result);

  struct netif *netif = (struct netif*)Ptr_val(v_netif);
  uint8_t *buf = (uint8_t *)Caml_ba_data_val(v_buf);
  unsigned size = Int_val(v_size);
  const char *err = NULL;

  const int rc = netdev_rx(netif, buf, &size, &err);
  if (rc < 0) {
    uk_pr_err("netdev_rx: failed with %s\n", err);
    v_result = alloc_result_error(err);
    CAMLreturn(v_result);
  }

  if (rc > 0) {
    set_netdev_queue_ready(netif->id);
  }
  else {
    set_netdev_queue_empty(netif->id);
  }

  uk_pr_debug("uk_netdev_rx: read %d bytes\n", size);

  v_result = alloc_result_ok();
  Store_field(v_result, 0, Val_int(size));
  CAMLreturn(v_result);
}
