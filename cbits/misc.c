#include <stdlib.h>
#include <stdio.h>
#include <libssh/libssh.h>
#include <libssh/server.h>
#include <libssh/callbacks.h>

ssh_server_callbacks
  ssh_new_server_callbacks( void                                 *userdata
                          , ssh_auth_password_callback                *cb1
                          , ssh_auth_none_callback                    *cb2
                          , ssh_auth_pubkey_callback                  *cb3
                          , ssh_service_request_callback              *cb4
                          , ssh_channel_open_request_session_callback *cb5
                          ) {

  ssh_server_callbacks ptr = malloc(sizeof (struct ssh_server_callbacks_struct));
  memset(ptr, 0, sizeof (struct ssh_server_callbacks_struct));
  ssh_callbacks_init(ptr);

  (*ptr).auth_password_function                = cb1;
  (*ptr).auth_none_function                    = cb2;
  (*ptr).auth_pubkey_function                  = cb3;
  (*ptr).service_request_function              = cb4;
  (*ptr).channel_open_request_session_function = cb5;

  return ptr;

}

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
