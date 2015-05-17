%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Nathan Fiedler
%%
%% -------------------------------------------------------------------

-module(exif_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) ->
    ok = application:load(exif),
    Config.

all() ->
    [
        test_read_exif,
        test_read_jfif,
        test_read_jfif_exif
    ].

test_read_exif(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "with_exif.jpg"]),
    case exif:read(ImagePath) of
        {error, _Reason} ->
            ?assert(false);
        Exif ->
            % TODO: extend this to verify additional values
            case dict:find(date_time_original, Exif) of
                {ok, Original} ->
                    ?assertEqual(<<"2014:04:23 13:33:08">>, Original);
                error ->
                    ?assert(false)
            end
    end,
    ok.

test_read_jfif(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "with_jfif.jpg"]),
    case exif:read(ImagePath) of
        {error, _Reason} ->
            ?assert(false);
        Exif ->
            ?assert(dict:is_empty(Exif))
    end,
    ok.

test_read_jfif_exif(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "with_jfif_exif.jpg"]),
    case exif:read(ImagePath) of
        {error, _Reason} ->
            ?assert(false);
        Exif ->
            case dict:find(date_time_original, Exif) of
                {ok, Original} ->
                    ?assertEqual(<<"2014:04:23 13:33:08">>, Original);
                error ->
                    ?assert(false)
            end
    end,
    ok.
