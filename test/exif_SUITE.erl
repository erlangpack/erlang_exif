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

%% ------------------------------------------------------------
%% Tests list
%% ------------------------------------------------------------

all() ->
    [
     {group, dict}
     | maybe_maps_tests()
    ].

groups() ->
    [
     {dict, [], all_tests()},
     {maps, [], all_tests()}
    ].

all_tests() ->
    [
        test_read_exif,
        test_read_jfif,
        test_read_jfif_exif,
        test_ifd_end,
        test_empty_tag
    ].

-ifdef(otp17_or_higher).

maybe_maps_tests() ->  [{group, maps}].

-else.

maybe_maps_tests() -> [].

-endif.

%% ------------------------------------------------------------
%% Init & clean
%% ------------------------------------------------------------

init_per_suite(Config) ->
    ok = application:load(exif),
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(dict, Config) ->
    [{return_type, dict}, {data_mod, exif_dict} | Config];
init_per_group(maps, Config) ->
    [{return_type, maps}, {data_mod, exif_maps} | Config].

end_per_group(_Group, Config) ->
    Config.

%% ------------------------------------------------------------
%% Test cases
%% ------------------------------------------------------------

test_read_exif(Config) ->
    DataDir = ?config(data_dir, Config),
    ReturnType = ?config(return_type, Config),
    DataMod = ?config(data_mod, Config),
    ImagePath = filename:join([DataDir, "with_exif.jpg"]),
    {ok, Exif} = exif:read(ImagePath, ReturnType),
    {ok, Version} = DataMod:find(exif_version, Exif),
    ?assertEqual(<<"Exif Version 2.21">>, Version),
    {ok, Flash} = DataMod:find(flash, Exif),
    ?assertEqual(16, Flash),
    {ok, Original} = DataMod:find(date_time_original, Exif),
    ?assertEqual(<<"2014:04:23 13:33:08">>, Original),
    {ok, WhiteBalance} = DataMod:find(white_balance, Exif),
    ?assertEqual(<<"Auto white balance">>, WhiteBalance),
    {ok, Aperture} = DataMod:find(aperture_value, Exif),
    ?assertEqual({ratio, 7801, 3429}, Aperture),
    {ok, PixelXDim} = DataMod:find(pixel_x_dimension, Exif),
    ?assertEqual(512, PixelXDim),
    {ok, PixelYDim} = DataMod:find(pixel_y_dimension, Exif),
    ?assertEqual(384, PixelYDim),
    ok.

test_read_jfif(Config) ->
    DataDir = ?config(data_dir, Config),
    ReturnType = ?config(return_type, Config),
    DataMod = ?config(data_mod, Config),
    ImagePath = filename:join([DataDir, "with_jfif.jpg"]),
    {ok, Exif} = exif:read(ImagePath, ReturnType),
    ?assertEqual(0, DataMod:size(Exif)),
    ok.

test_read_jfif_exif(Config) ->
    DataDir = ?config(data_dir, Config),
    ReturnType = ?config(return_type, Config),
    DataMod = ?config(data_mod, Config),
    ImagePath = filename:join([DataDir, "with_jfif_exif.jpg"]),
    {ok, Exif} = exif:read(ImagePath, ReturnType),
    {ok, Original} = DataMod:find(date_time_original, Exif),
    ?assertEqual(<<"2014:04:23 13:33:08">>, Original),
    ok.

test_ifd_end(Config) ->
    DataDir = ?config(data_dir, Config),
    ReturnType = ?config(return_type, Config),
    ImagePath = filename:join([DataDir, "ifd_end.jpg"]),
    {ok, _Exif} = exif:read(ImagePath, ReturnType),
    ok.

test_empty_tag(Config) ->
    DataDir = ?config(data_dir, Config),
    ReturnType = ?config(return_type, Config),
    DataMod = ?config(data_mod, Config),
    ImagePath = filename:join([DataDir, "empty_tag.jpg"]),
    {ok, Exif} = exif:read(ImagePath, ReturnType),
    ?assertEqual(error, DataMod:find(iso_speed_ratings, Exif)),
    {ok, ShutterSpeed} = DataMod:find(shutter_speed_value, Exif),
    ?assertEqual({ratio,0,0}, ShutterSpeed),
    {ok, ExposureProgram} = DataMod:find(exposure_program, Exif),
    ?assertEqual(0, ExposureProgram),
    ok.
