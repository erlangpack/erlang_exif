# Erlang EXIF Library

## Building and Testing

### Prerequisites

* [Erlang/OTP](http://www.erlang.org) 18 or higher.

To build the application and run the test suite, use the Makefile like so:

```
$ make
$ make test
```

## Usage

To add `erlang_exif` as a dependency to your rebar-based project, simply add the following to your `rebar.config` file, then run the `./rebar3 upgrade` command to retrieve it.

```
{deps, [
    {erlang_exif, "3.0.0"}
]}.
```

## Example

The `erlang_exif:read(Path)` function actually calls `erlang_exif:read(Path, dict)` (for backward compatibility).

The `erlang_exif:read(Path, ReturnType)` function returns `{ok, Exif}` where `Exif` is a `dict:dict()` or a map
of the values read from the JPEG image. `ReturnType` has two valid values: `dict` and `maps`.
If no such values are present, the structure will be empty.
However, if there was an error, an `{error, Reason}` tuple will be returned, where `Reason` is nearly always `invalid_exif`.

```
case erlang_exif:read(Path, dict) of
    {error, Reason} ->
        error_logger:error_msg("Unable to read EXIF data from ~s, ~p~n", [Path, Reason]);
    {ok, ExifData} ->
        case dict:find(date_time_original, ExifData) of
            {ok, Original} ->
                % do something with the date...
                ok;
            error ->
                error_logger:info_msg("No original date available")
        end
end.
```

Two more functions are present: `erlang_exif:read_binary/1` and `erlang_exif:read_binary/2` which are
equivalents of `erlang_exif:read/1,2`.
They accept actual file in binary format as a first argument, instead of a path.
