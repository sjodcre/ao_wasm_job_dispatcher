-module(dispatcher_tests).
-include_lib("eunit/include/eunit.hrl").

dispatcher_usage_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
         {"prints usage on invalid args",
          fun() ->
              Result = (catch dispatcher:main(["invalid", "args"])),
              ?assertMatch({'EXIT', _}, Result)
          end}
     ]}.
