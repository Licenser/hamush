%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, mushdb,
 [{description, "RPC server for Erlang and OTP in action"},
  {vsn, "0.1.0"},
  {modules, [mdb_app,
             mdb_sup,
	     mdb_store,
	     mdb_element]},
  {registered, [mdb_sup]},
  {applications, [kernel, stdlib]},
  {mod, {mdb_app, []}}
 ]}.
