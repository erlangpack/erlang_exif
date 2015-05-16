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

end_per_suite(_Config) ->
    % TODO: probably don't need this function
    ok.

all() ->
    [
        test_read_exif
        % TODO: test reading image with JFIF and Exif data
        % TODO: test reading image with JFIF and no Exif data
    ].

test_read_exif(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "with_exif.jpg"]),
    case exif:read(ImagePath) of
        {error, _Reason} ->
            ?assert(false);
        ExifDate ->
            % TODO: extend this to verify additional values
            case dict:find(date_time_original, ExifDate) of
                {ok, Original} ->
                    ?assertEqual(<<"2014:04:23 13:33:08">>, Original);
                error ->
                    ?assert(false)
            end
    end,
    ok.
