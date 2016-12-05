-module(mstore_inspector).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(["create", File]) ->
    io:format("Not implemented"),
    erlang:halt(1);
main(["metrics", File]) ->
    io:format("Not implemented"),
    erlang:halt(1);
main(["compare", Metric, A, B]) ->
    io:format("Not implemented"),
    erlang:halt(1);
main(["--help"]) ->
    io:format(
      " MStore inspectr help\n"
      "create  <mstore file>              - creates an index file\n"
      "metrics <idx_fille>                - Lists metrics in an index\n"
      "compare <metric> <idx_file A>      - compares two iundex files\n"
      "        <idx_file B>                 and prints a delta\n"
     ),
    erlang:halt(0);
main(Args) ->
    io:format("Unknown command: ~p\n", [Args]),
    main(["--help"]).


%%====================================================================
%% Internal functions
%%====================================================================
