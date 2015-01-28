#include <libssh/libssh.h>
#include <libssh/server.h>
#include <libssh/callbacks.h>

ssh_bind_callbacks ssh_bind_new_callbacks(ssh_bind_incoming_connection_callback *callback);
void               ssh_bind_free_callbacks(ssh_bind_callbacks callbacks);

ssh_server_callbacks
  ssh_new_server_callbacks( void                                 *userdata
                          , ssh_auth_password_callback                *cb1
                          , ssh_auth_none_callback                    *cb2
                          , ssh_auth_pubkey_callback                  *cb3
                          , ssh_service_request_callback              *cb4
                          , ssh_channel_open_request_session_callback *cb5
                          );
void
  ssh_free_server_callbacks(ssh_server_callbacks callbacks);
