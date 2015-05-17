# Erlang EXIF Library

## Building

Requires [Erlang/OTP](http://www.erlang.org) R17 for building and running, and [rebar](https://github.com/rebar/rebar) for building.

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
    {exif, ".*", {git, "https://github.com/nlfiedler/erlang-exif", {tag, "v1.1"}}}
]}.
```
