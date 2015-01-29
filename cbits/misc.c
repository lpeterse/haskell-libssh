#include <stdlib.h>
#include <stdio.h>
#include <libssh/libssh.h>
#include <libssh/server.h>
#include <libssh/callbacks.h>

ssh_server_callbacks
  ssh_new_server_callbacks( void                                *userdata
                          , ssh_auth_password_callback                cb1
                          , ssh_auth_none_callback                    cb2
                          , ssh_auth_pubkey_callback                  cb3
                          , ssh_service_request_callback              cb4
                          , ssh_channel_open_request_session_callback cb5
                          ) {

  ssh_server_callbacks ptr = malloc(sizeof (struct ssh_server_callbacks_struct));
  memset(ptr, 0, sizeof (struct ssh_server_callbacks_struct));
  ssh_callbacks_init(ptr);

  (*ptr).userdata                              = userdata;
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
void ssh_free_server_callbacks(ssh_server_callbacks callbacks) {
  free(callbacks);
};


ssh_channel_callbacks
  ssh_new_channel_callbacks
    ( void                                   *userdata
    , ssh_channel_data_callback              c1
    , ssh_channel_eof_callback               c2
    , ssh_channel_close_callback             c3
    , ssh_channel_signal_callback            c4
    , ssh_channel_exit_status_callback       c5
    , ssh_channel_exit_signal_callback       c6
    , ssh_channel_pty_request_callback       c7
    , ssh_channel_shell_request_callback     c8
    , ssh_channel_auth_agent_req_callback    c9
    , ssh_channel_x11_req_callback           c10
    , ssh_channel_pty_window_change_callback c11
    , ssh_channel_exec_request_callback      c12
    , ssh_channel_env_request_callback       c13
    , ssh_channel_subsystem_request_callback c14
    ) {

  ssh_channel_callbacks ptr = malloc(sizeof (struct ssh_channel_callbacks_struct));
  memset(ptr, 0, sizeof (struct ssh_channel_callbacks_struct));
  ssh_callbacks_init(ptr);

  (*ptr).userdata                           = userdata;
  (*ptr).channel_data_function              = c1;
  (*ptr).channel_eof_function               = c2;
  (*ptr).channel_close_function             = c3;
  (*ptr).channel_signal_function            = c4;
  (*ptr).channel_exit_status_function       = c5;
  (*ptr).channel_exit_signal_function       = c6;
  (*ptr).channel_pty_request_function       = c7;
  (*ptr).channel_shell_request_function     = c8;
  (*ptr).channel_auth_agent_req_function    = c9;
  (*ptr).channel_x11_req_function           = c10;
  (*ptr).channel_pty_window_change_function = c11;
  (*ptr).channel_exec_request_function      = c12;
  (*ptr).channel_env_request_function       = c13;
  (*ptr).channel_subsystem_request_function = c14;

  return ptr;
};

void
  ssh_free_channel_callbacks(ssh_channel_callbacks callbacks) {
    free(callbacks);
};
