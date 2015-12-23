# Erlang EXIF Library

## Building

Requires [Erlang/OTP](http://www.erlang.org) R17|R18 for building and running, and [rebar](https://github.com/rebar/rebar) for building.

```
$ make compile
```

## Testing

```
$ make ct
```

## Usage

To add `erlang-exif` as a dependency to your rebar-based project, simply add the following to your `rebar.config` file, then run the `rebar get-deps` command to retrieve it.

```
{deps, [
    {exif, ".*", {git, "https://github.com/nlfiedler/erlang-exif", {tag, "2.0.0"}}}
]}.
```

## Example

The `exif:read/1` function returns `{ok, Exif}` where `Exif` is a `dict:dict()` of the values read from the JPEG image. If no such values are present, the `dict` will be empty. However, if there was an error, an `{error, Reason}` tuple will be returned, where `Reason` is nearly always `invalid_exif`.

```
case exif:read(Path) of
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
