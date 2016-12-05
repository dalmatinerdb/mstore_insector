# mstore_inspector

In inspector tool for mstore-files (not mstores!) that allows comparing, and gathering some information on them.


### Build

    $ rebar3 escriptize

## Run

    $ _build/default/bin/mstore_inspector --help

## File Format


The index file generated looks like:

```erlang
<<
  SizePerSection:64,          %% size of each bitmap (this includes the prefix!)
  SizeOfIndex:64,             %% size of the index section
  Index:SizeOfIndex/binary,   %% index Section
  Bitmaps/binary              %% bitmaps
>>.
```

Bitmaps are stored using the [bitmap library](https://github.com/dalmatinerdb/bitmap)

### Index section

```erlang
<<
  Size:32,       %% Size of this entry
  Element:Size,  %% Entry
  Rest/binary    %% The rest
>>.
```

## Comparison logic

We sort the indexes then iterate through them, if we see one on the left and not the right (or visa versa) we known the entire metric is missing. If both er present we use `bitmap:diff/2` to diff the two bitmaps.

We fail early if the `SizePerSection` differs.

Diffs are visualised using the `bitmap:visualize/4` function.

If there is no difference we don't need to visualise.

For per metric comparison it's a lot easier.


Left always refers to the 1st file and right to the 2nd file.