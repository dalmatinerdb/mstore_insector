-module(mstore_inspector).

%% API exports
-export([display/1, get/2, main/1, compare/3]).

-record(acc, {
          offset,
          file,
          metric,
          bitmap,
          size,
          io
         }).

%%====================================================================
%% API functions
%%====================================================================

display(File) ->
    {ok, F} = mfile:open(File),
    mfile:close(F),
    Size = mfile:size(F),
    {ok, Dummy} = bitmap:new([{size, Size}]),
    ChunkSize = byte_size(Dummy),
    M0 = mfile:metrics(F),
    M1 = lists:keysort(2, btrie:to_list(M0)),
    M3 = [M || {M, _} <- M1],
    {ok, IO} = file:open(File ++ ".bitmap", [read, binary, raw]),
    read_metrics(M3, ChunkSize, IO).

get(File, Metric) ->
    {ok, F} = mfile:open(File),
    mfile:close(F),
    M0 = mfile:metrics(F),
    case btrie:find(Metric, M0) of
        error ->
            {error, not_found};
        {ok, Idx} ->
            Size = mfile:size(F),
            {ok, Dummy} = bitmap:new([{size, Size}]),
            ChunkSize = byte_size(Dummy),
            Offset = ChunkSize * Idx,
            {ok, IO} = file:open(File ++ ".bitmap", [read, binary, raw]),
            R = file:pread(IO, Offset, ChunkSize),
            file:close(IO),
            R
    end.

compare(Metric, FileL, FileR) ->
    {ok, L} = get(FileL, Metric),
    {ok, R} = get(FileR, Metric),
    W = application:get_env(mstore_inspector, width, 100),
    bitmap:display_diff(L, R, W).

%% escript Entry point
main(["create", File]) ->
    mfile:fold(File, fun fold_fun/4, 4096, ok),
    erlang:halt(1);

main(["metrics", File]) ->
    io:format("Inspecting file: ~s~n", [File]),
    {ok, F} = mfile:open(File),
    Metrics = btrie:fetch_keys(mfile:metrics(F)),
    [io:format("* ~p~n", [M]) || M <- Metrics],
    erlang:halt(0);

main(["compare", _Metric, _A, _B]) ->
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

set_bitmap(<<>>, _I, B) ->
    B;
set_bitmap(<<0, _:56, R/binary>>, I, B) ->
    set_bitmap(R, I + 1, B);
set_bitmap(<<_:64, R/binary>>, I, B) ->
    {ok, B1} = bitmap:set(I, B),
    set_bitmap(R, I+1, B1).

fold_fun(Metric, Idx, Data, Acc = #acc{size = Size,
                                       metric = undefined}) ->
    {ok, B} = bitmap:new([{size, Size}]),
    B1 = set_bitmap(Data, Idx rem Size, B),
    Acc#acc{metric = Metric, bitmap = B1};

fold_fun(Metric, Idx, Data, Acc = #acc{metric = Metric,
                                       size = Size,
                                       bitmap = B}) ->
    B1 = set_bitmap(Data, Idx rem Size, B),
    Acc#acc{bitmap = B1};
fold_fun(Metric, Idx, Data, Acc = #acc{size = Size,
                                       bitmap = BOld,
                                       io = IO}) ->
    {ok, B} = bitmap:new([{size, Size}]),
    B1 = set_bitmap(Data, Idx rem Size, B),
    ok = file:write(IO, BOld),
    Acc#acc{metric = Metric, bitmap = B1}.

read_metrics([], _, IO) ->
    file:close(IO);
read_metrics([M | R], C, IO) ->
    case file:read(IO, C) of
        {ok, Bitmap} ->
            io:format("~n=== ~p~n", [M]),
            W = application:get_env(mstore_inspector, width, 100),
            bitmap:display(Bitmap, W),
            read_metrics(R, C, IO);
        E ->
            file:close(IO),
            E
    end.
