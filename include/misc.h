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

ssh_channel_callbacks
  ssh_new_channel_callbacks(
      void                                   *userdata
    , ssh_channel_data_callback              *c1
    , ssh_channel_eof_callback               *c2
    , ssh_channel_close_callback             *c3
    , ssh_channel_signal_callback            *c4
    , ssh_channel_exit_status_callback       *c5
    , ssh_channel_exit_signal_callback       *c6
    , ssh_channel_pty_request_callback       *c7
    , ssh_channel_shell_request_callback     *c8
    , ssh_channel_auth_agent_req_callback    *c9
    , ssh_channel_x11_req_callback           *c10
    , ssh_channel_pty_window_change_callback *c11
    , ssh_channel_exec_request_callback      *c12
    , ssh_channel_env_request_callback       *c13
    , ssh_channel_subsystem_request_callback *c14
    );

void
  ssh_free_channel_callbacks(ssh_channel_callbacks callbacks);
