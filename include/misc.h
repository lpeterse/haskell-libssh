#include <libssh/libssh.h>
#include <libssh/server.h>
#include <libssh/callbacks.h>

ssh_bind_callbacks ssh_bind_new_callbacks(ssh_bind_incoming_connection_callback *callback);
void               ssh_bind_free_callbacks(ssh_bind_callbacks callbacks);
