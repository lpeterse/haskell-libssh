#include <stdlib.h>
#include <stdio.h>
#include <libssh/libssh.h>
#include <libssh/server.h>
#include <libssh/callbacks.h>

ssh_bind_callbacks ssh_bind_new_callbacks(ssh_bind_incoming_connection_callback callback) {

  // This was useful for debugging:
  //   printf("callback = %p \n", callback);
  //   callback(NULL, NULL);

  // Reminder:
  //   typedef struct ssh_bind_callbacks_struct *ssh_bind_callbacks;

  // It is not safe for the struct to reside on the stack
  // as libssh is not making a copy.
  ssh_bind_callbacks ptr = malloc(sizeof (struct ssh_bind_callbacks_struct));

  // Set the actual pointer to the callback
  (*ptr).incoming_connection = callback;

  // Initialize the struct according to libssh docs
  ssh_callbacks_init(ptr);

  return ptr;
}

void ssh_bind_free_callbacks(ssh_bind_callbacks callbacks) {
  free(callbacks);
};
