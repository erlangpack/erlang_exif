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
    {ok, Exif} = exif:read(ImagePath),
    {ok, Version} = dict:find(exif_version, Exif),
    ?assertEqual(<<"Exif Version 2.21">>, Version),
    {ok, Flash} = dict:find(flash, Exif),
    ?assertEqual(16, Flash),
    {ok, Original} = dict:find(date_time_original, Exif),
    ?assertEqual(<<"2014:04:23 13:33:08">>, Original),
    {ok, WhiteBalance} = dict:find(white_balance, Exif),
    ?assertEqual(<<"Auto white balance">>, WhiteBalance),
    {ok, Aperture} = dict:find(aperture_value, Exif),
    ?assertEqual({ratio, 7801, 3429}, Aperture),
    {ok, PixelXDim} = dict:find(pixel_x_dimension, Exif),
    ?assertEqual(512, PixelXDim),
    {ok, PixelYDim} = dict:find(pixel_y_dimension, Exif),
    ?assertEqual(384, PixelYDim),
    ok.

test_read_jfif(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "with_jfif.jpg"]),
    {ok, Exif} = exif:read(ImagePath),
    ?assert(dict:is_empty(Exif)),
    ok.

test_read_jfif_exif(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "with_jfif_exif.jpg"]),
    {ok, Exif} = exif:read(ImagePath),
    {ok, Original} = dict:find(date_time_original, Exif),
    ?assertEqual(<<"2014:04:23 13:33:08">>, Original),
    ok.
