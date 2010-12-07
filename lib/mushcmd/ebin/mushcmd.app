%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, mushcmd,
 [{description, "HA Mush command execotor"},
  {vsn, "0.1.0"},
  {modules, [mcmd_app,
             mcmd_sup,
	     mcmd_worker]},
  {registered, [mcmd_sup]},
  {applications, [kernel, stdlib, mushdb]},
  {mod, {mcmd_app, []}}
 ]}.
