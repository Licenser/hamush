%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, hamush,
 [{description, "HA Mush main application"},
  {vsn, "0.1.0"},
  {modules, [ham_app]},
  {registered, []},
  {applications, [kernel, stdlib, mushdb, mushcmd]},
  {mod, {ham_app, []}}
 ]}.
