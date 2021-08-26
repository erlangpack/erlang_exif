%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011 Andre Nathan
%% Copyright (c) 2015 Nathan Fiedler
%%
%% @author Andre Nathan, Nathan Fiedler
%%
%% @doc Reads the Exif data from JPEG images.
%% @end
%% -------------------------------------------------------------------

-module(erlang_exif).
-export([read/1, read/2]).
-export([read_binary/1, read_binary/2]).

-ifdef(debug).
-define(DEBUG(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(DEBUG(_Fmt, _Args), ok).
-endif.

-type exif() :: dict:dict() | map().

-type data_module() :: erlang_exif_dict | erlang_exif_maps.
-type return_type() :: dict | maps.

-define(MAX_EXIF_LEN,        65536).
-define(JPEG_MARKER,       16#ffd8).
-define(EXIF_MARKER,       16#ffe1).
-define(JFIF_MARKER,       16#ffe0).
-define(FIRST_EXIF_OFFSET,       8).
-define(FIELD_LEN,              12).
-define(START_POSITION,         22).
-define(BYTE_SIZE,               1).
-define(SHORT_SIZE,              2).
-define(LONG_SIZE,               4).
-define(RATIO_SIZE,              8).
-define(MAX_INT32,      2147483647).
-define(MAX_UINT32,     4294967295).

-define(TYPE_NONE,          0).
-define(TYPE_BYTE,          1).
-define(TYPE_ASCII,         2).
-define(TYPE_SHORT,         3).
-define(TYPE_LONG,          4).
-define(TYPE_RATIO,         5).
-define(TYPE_SIGNED_BYTE,   6).
-define(TYPE_UNDEFINED,     7).
-define(TYPE_SIGNED_SHORT,  8).
-define(TYPE_SIGNED_LONG,   9).
-define(TYPE_SIGNED_RATIO, 10).

%%
%% @doc Read the Exif data from a binary.
%%
-spec read_binary(Data) -> {ok, Exif} | {error, Reason}
    when Data   :: binary(),
         Exif   :: exif(),
         Reason :: term().
read_binary(Data) ->
    read_binary(Data, dict).

%%
%% @doc Read the Exif data from a binary and specify
%%      a data module.
%%
-spec read_binary(Data, ReturnType) -> {ok, Exif} | {error, Reason}
    when Data       :: binary(),
         ReturnType :: return_type(),
         Exif       :: exif(),
         Reason     :: term().
read_binary(Data, ReturnType) when is_binary(Data) ->
    ImageData = if byte_size(Data) > ?MAX_EXIF_LEN ->
                        <<Img:?MAX_EXIF_LEN/binary, _/binary>> = Data,
                        Img;
                   true ->
                        Data
                end,
    find_and_parse_exif(ImageData, data_mod(ReturnType)).

%%
%% @doc Read the Exif data from a named file (or binary data).
%%
-spec read(File) -> {ok, Exif} | {error, Reason}
    when File   :: file:filename_all(),
         Exif   :: exif(),
         Reason :: term()
        ; (Data) -> {ok, Exif} | {error, Reason}
    when Data   :: binary(),
         Exif   :: exif(),
         Reason :: term().
read(File) when is_list(File); is_binary(File) ->
    read(File, dict).

%%
%% @doc Read the Exif data from a named file (or binary data)
%%      with data module specified.
%%
-spec read(File, ReturnType) -> {ok, Exif} | {error, Reason}
    when File       :: file:filename_all(),
         ReturnType :: return_type(),
         Exif       :: exif(),
         Reason     :: term()
        ; (Data, ReturnType) -> {ok, Exif} | {error, Reason}
    when Data       :: binary(),
         ReturnType :: return_type(),
         Exif       :: exif(),
         Reason     :: term().
read(File, ReturnType) when is_list(File); is_binary(File) ->
    {ok, Fd} = file:open(File, [read, binary, raw]),
    {ok, Img} = file:read(Fd, ?MAX_EXIF_LEN),
    ok = file:close(Fd),
    find_and_parse_exif(Img, data_mod(ReturnType)).


find_and_parse_exif(<< ?JPEG_MARKER:16, ?JFIF_MARKER:16, Len:16, Rest/binary >>, DataMod) ->
    % skip the JFIF segment and attempt to match the Exif segment
    case skip_segment(Len, Rest) of
        <<?EXIF_MARKER:16, _Len:16, Exif/binary>> ->
            read_exif(Exif, DataMod);
        _ ->
            % apparently just JFIF and no Exif
            {ok, DataMod:new()}
    end;
find_and_parse_exif(<< ?JPEG_MARKER:16, ?EXIF_MARKER:16, _Len:16, Rest/binary >>, DataMod) ->
    read_exif(Rest, DataMod);
find_and_parse_exif(<< ?JPEG_MARKER:16, _Rest/binary >>, DataMod) ->
    {ok, DataMod:new()};
find_and_parse_exif(_, DataMod) ->
    {ok, DataMod:new()}.

-spec data_mod(ReturnType) -> DataMod
    when ReturnType :: return_type(),
         DataMod :: data_module().
data_mod(dict) -> erlang_exif_dict;
data_mod(maps) -> erlang_exif_maps.

skip_segment(Len, Data) ->
    Skip = Len - 2,
    <<_Segment:Skip/binary, Rest/binary>> = Data,
    Rest.

% Exif header
read_exif(<<
            "Exif",
            0 : 16,
            TiffMarker/binary
        >>, DataMod) ->
    read_tiff_marker(TiffMarker, DataMod);
read_exif(_, _) ->
    {error, invalid_exif}.

read_tiff_marker(<<
             16#4949   : 16,
             16#2a00   : 16,
             Offset    : 4/binary,
             _/binary
          >> = TiffMarker, DataMod) ->
    read_exif_header(little, Offset, TiffMarker, DataMod);
read_tiff_marker(<< 16#4d4d   : 16,
             16#002a   : 16,
             Offset    : 4/binary,
             _/binary
          >> = TiffMarker, DataMod) ->
    read_exif_header(big, Offset, TiffMarker, DataMod);
read_tiff_marker(_, _DataMod) ->
    {error, invalid_exif}.

read_exif_header(End, Offset, TiffMarker, DataMod) ->
    IfdOffset = uread(Offset, End),
    ?DEBUG("IFD0 at ~p~n", [IfdOffset]),
    <<_:IfdOffset/binary, IFD/binary>> = TiffMarker,
    {ok, read_tags(IFD, TiffMarker, End, fun image_tag/1, DataMod)}.

read_tags(<< NumEntries:2/binary, Rest/binary >>, TiffMarker, End, TagFun, DataMod) ->
    N = uread(NumEntries, End),
    read_tags(Rest, N, TiffMarker, End, TagFun, DataMod).

read_tags(Bin, NumEntries, TiffMarker, End, TagFun, DataMod) ->
    read_tags(Bin, NumEntries, TiffMarker, End, TagFun, DataMod:new(), DataMod).

read_tags(_Bin, 0, _TiffMarker, _End, _TagFun, Tags, _DataMod) ->
    Tags;
read_tags(Bin, NumEntries, TiffMarker, End, TagFun, Tags, DataMod) ->
    {NewTags, Rest} = add_tag(Bin, TiffMarker, End, Tags, TagFun, DataMod),
    read_tags(Rest, NumEntries - 1, TiffMarker, End, TagFun, NewTags, DataMod).

add_tag(<< Tag:2/binary, Rest/binary >>, TiffMarker, End, Tags, TagFun, DataMod) ->
    TagNum = uread(Tag, End),
    Name = TagFun(TagNum),
    Value = tag_value(Name, read_tag_value(Rest, TiffMarker, End)),
    ?DEBUG("Tag(0x~.16b): ~p: ~p~n", [TagNum, Name, Value]),
    NewTags = if
        Value =:= undefined ->
            Tags;
        true ->
            case Name of
                unknown ->
                    Tags;
                exif ->
                    ExifTags = read_subtags(Value, TiffMarker, End, fun exif_tag/1, DataMod),
                    DataMod:merge(Tags, ExifTags);
                gps ->
                    GpsTags = read_subtags(Value, TiffMarker, End, fun gps_tag/1, DataMod),
                    DataMod:merge(Tags, GpsTags);
                _ ->
                    DataMod:store(Name, Value, Tags)
            end
    end,
    Len = ?FIELD_LEN - 2, % 2 bytes for the tag above.
    << _:Len/binary, NewRest/binary >> = Rest,
    {NewTags, NewRest}.

read_subtags(Offset, TiffMarker, End, TagFun, DataMod) ->
    ?DEBUG("Sub-IFD at ~p~n", [Offset]),
    << _:Offset/binary, Rest/binary >> = TiffMarker,
    read_tags(Rest, TiffMarker, End, TagFun, DataMod).

read_tag_value(<<
                 ValueType   : 2/binary,
                 ValueNum    : 4/binary,
                 Rest/binary
               >>, TiffMarker, End) ->
    Type = uread(ValueType, End),
    NumValues = uread(ValueNum, End),
    case decode_tag(Type, Rest, NumValues, TiffMarker, End) of
        [] -> ok;
        [Value] -> Value;
        Values  -> Values
    end.

%% Tag decoding by type.

% Byte
decode_tag(?TYPE_BYTE, Bin, NumValues, TiffMarker, End) ->
    ?DEBUG("> Byte(~p)~n", [NumValues]),
    decode_numeric(Bin, NumValues, TiffMarker, ?BYTE_SIZE, End);

% ASCII
decode_tag(?TYPE_ASCII, Bin, NumBytes, TiffMarker, End) ->
    ?DEBUG("> ASCII(~p)~n", [NumBytes]),
    << ValueOffset:4/binary, _/binary >> = Bin,
    case NumBytes > 4 of
        true ->
            Offset = uread(ValueOffset, End),
            Len = NumBytes - 1,  % ignore null-byte termination
            << _:Offset/binary, Value:Len/binary, _/binary >> = TiffMarker,
            Value;
        false ->
            ValueOffset
    end;

% Short
decode_tag(?TYPE_SHORT, Bin, NumValues, TiffMarker, End) ->
    ?DEBUG("> Short(~p)~n", [NumValues]),
    decode_numeric(Bin, NumValues, TiffMarker, ?SHORT_SIZE, End);

% Signed short
decode_tag(?TYPE_SIGNED_SHORT, Bin, NumValues, TiffMarker, End) ->
    ?DEBUG("> Signed Short(~p)~n", [NumValues]),
    Ushorts = decode_numeric(Bin, NumValues, TiffMarker, ?SHORT_SIZE, End),
    lists:map(fun as_signed/1, Ushorts);

% Long
decode_tag(?TYPE_LONG, Bin, NumValues, TiffMarker, End) ->
    ?DEBUG("> Long(~p)~n", [NumValues]),
    decode_numeric(Bin, NumValues, TiffMarker, ?LONG_SIZE, End);

% Signed long
decode_tag(?TYPE_SIGNED_LONG, Bin, NumValues, TiffMarker, End) ->
    ?DEBUG("> Signed Long(~p)~n", [NumValues]),
    Ulongs = decode_numeric(Bin, NumValues, TiffMarker, ?LONG_SIZE, End),
    lists:map(fun as_signed/1, Ulongs);

% Rational
decode_tag(?TYPE_RATIO, Bin, NumValues, TiffMarker, End) ->
    ?DEBUG("> Rational(~p)~n", [NumValues]),
    decode_ratio(Bin, NumValues, TiffMarker, End);

% Signed rational
decode_tag(?TYPE_SIGNED_RATIO, Bin, NumValues, TiffMarker, End) ->
    ?DEBUG("> Signed Rational(~p)~n", [NumValues]),
    Uratios = decode_ratio(Bin, NumValues, TiffMarker, End),
    lists:map(fun({ratio, Num, Den}) ->
        {ratio, as_signed(Num), as_signed(Den)}
    end, Uratios);

% Undefined
decode_tag(?TYPE_UNDEFINED, Bin, NumValues, TiffMarker, End) ->
    ?DEBUG("> Undefined(~p)~n", [NumValues]),
    decode_numeric(Bin, NumValues, TiffMarker, ?BYTE_SIZE, End);

decode_tag(?TYPE_NONE, _Bin, 0, _TiffMarker, _End) ->
    ?DEBUG("> None~n",[]),
    undefined. 


decode_numeric(Bin, NumValues, TiffMarker, Size, End) ->
    Len = NumValues * Size,
    Values = case Len > 4 of
        true ->
            << ValueOffset:4/binary, _/binary >> = Bin,
            Offset = uread(ValueOffset, End),
            << _:Offset/binary, Data:Len/binary, _/binary >> = TiffMarker,
            Data;
        false ->
            << Value:Len/binary, _/binary >> = Bin,
            Value
    end,
    uread_many(Values, Size, End).

decode_ratio(Bin, NumValues, TiffMarker, End) ->
    << ValueOffset:4/binary, _/binary >> = Bin,
    Offset = uread(ValueOffset, End),
    <<_:Offset/binary, RatioData/binary>> = TiffMarker,
    decode_ratios(RatioData, NumValues, End).

decode_ratios(_RatioData, 0, _End) ->
    [];
decode_ratios(RatioData, NumValues, End) ->
    << N : ?LONG_SIZE/binary,
       D : ?LONG_SIZE/binary,
       Rest/binary >> = RatioData,

    Num = uread(N, End),
    Den = uread(D, End),

    %% <<Get:500/binary, _/binary>> = Rest,
    %% io:format(user, "BIN Offset ~p~n~p~n~n~p~n~p~n~n", [{Offset, NewOffset, binary:encode_unsigned(338323, End)}, {N, D}, {Num, Den}, Get]),

    [{ratio, Num, Den} | decode_ratios(Rest, NumValues - 1, End)].

as_signed(X) when X > ?MAX_INT32 -> X - ?MAX_UINT32 - 1;
as_signed(X)                     -> X.

image_tag(16#00fe) -> new_subfile_type;
image_tag(16#00ff) -> subfile_type;
image_tag(16#0100) -> image_width;
image_tag(16#0101) -> image_length;
image_tag(16#0102) -> bits_per_sample;
image_tag(16#0103) -> compression;
image_tag(16#0106) -> photometric_interpretation;
image_tag(16#0107) -> threshholding;
image_tag(16#0108) -> cell_width;
image_tag(16#0109) -> cell_length;
image_tag(16#010a) -> fill_order;
image_tag(16#010d) -> document_name;
image_tag(16#010e) -> image_description;
image_tag(16#010f) -> make;
image_tag(16#0110) -> model;
image_tag(16#0111) -> strip_offsets;
image_tag(16#0112) -> orientation;
image_tag(16#0115) -> samples_per_pixel;
image_tag(16#0116) -> rows_per_strip;
image_tag(16#0117) -> strip_byte_counts;
image_tag(16#0118) -> min_sample_value;
image_tag(16#0119) -> max_sample_value;
image_tag(16#011a) -> x_resolution;
image_tag(16#011b) -> y_resolution;
image_tag(16#011c) -> planar_configuration;
image_tag(16#011d) -> page_name;
image_tag(16#011e) -> x_position;
image_tag(16#011f) -> y_position;
image_tag(16#0120) -> free_offsets;
image_tag(16#0121) -> free_byte_counts;
image_tag(16#0122) -> gray_response_unit;
image_tag(16#0123) -> gray_response_curve;
image_tag(16#0124) -> t4_options;
image_tag(16#0125) -> t6_options;
image_tag(16#0128) -> resolution_unit;
image_tag(16#012d) -> transfer_function;
image_tag(16#0131) -> software;
image_tag(16#0132) -> date_time;
image_tag(16#013b) -> artist;
image_tag(16#013c) -> host_computer;
image_tag(16#013a) -> predictor;
image_tag(16#013e) -> white_point;
image_tag(16#013f) -> primary_chromaticities;
image_tag(16#0140) -> color_map;
image_tag(16#0141) -> halftone_hints;
image_tag(16#0142) -> tile_width;
image_tag(16#0143) -> tile_length;
image_tag(16#0144) -> tile_offsets;
image_tag(16#0145) -> tile_byte_counts;
image_tag(16#0146) -> bad_fax_lines;
image_tag(16#0147) -> clean_fax_data;
image_tag(16#0148) -> consecutive_bad_fax_lines;
image_tag(16#014a) -> sub_ifds;
image_tag(16#014c) -> ink_set;
image_tag(16#014d) -> ink_names;
image_tag(16#014e) -> number_of_inks;
image_tag(16#0150) -> dot_range;
image_tag(16#0151) -> target_printer;
image_tag(16#0152) -> extra_samples;
image_tag(16#0156) -> transfer_range;
image_tag(16#0157) -> clip_path;
image_tag(16#0158) -> x_clip_path_units;
image_tag(16#0159) -> y_clip_path_units;
image_tag(16#015a) -> indexed;
image_tag(16#015b) -> jpeg_tables;
image_tag(16#015f) -> opi_proxy;
image_tag(16#0190) -> global_parameters_ifd;
image_tag(16#0191) -> profile_type;
image_tag(16#0192) -> fax_profile;
image_tag(16#0193) -> coding_methods;
image_tag(16#0194) -> version_year;
image_tag(16#0195) -> mode_number;
image_tag(16#01B1) -> decode;
image_tag(16#01B2) -> default_image_color;
image_tag(16#0200) -> jpegproc;
image_tag(16#0201) -> jpeg_interchange_format;
image_tag(16#0202) -> jpeg_interchange_format_length;
image_tag(16#0203) -> jpeg_restart_interval;
image_tag(16#0205) -> jpeg_lossless_predictors;
image_tag(16#0206) -> jpeg_point_transforms;
image_tag(16#0207) -> jpeg_q_tables;
image_tag(16#0208) -> jpeg_dc_tables;
image_tag(16#0209) -> jpeg_ac_tables;
image_tag(16#0211) -> ycb_cr_coefficients;
image_tag(16#0212) -> ycb_cr_sub_sampling;
image_tag(16#0213) -> ycb_cr_positioning;
image_tag(16#0214) -> reference_black_white;
image_tag(16#022f) -> strip_row_counts;
image_tag(16#02bc) -> xmp;
image_tag(16#800d) -> image_id;
image_tag(16#87ac) -> image_layer;
image_tag(16#8298) -> copyright;
image_tag(16#83bb) -> iptc;
image_tag(16#8769) -> exif;
image_tag(16#8825) -> gps;
image_tag(_)       -> unknown.

exif_tag(16#829a) -> exposure_time;
exif_tag(16#829d) -> f_number;
exif_tag(16#8822) -> exposure_program;
exif_tag(16#8824) -> spectral_sensitivity;
exif_tag(16#8827) -> iso_speed_ratings;
exif_tag(16#8828) -> oecf;
exif_tag(16#9000) -> exif_version;
exif_tag(16#9003) -> date_time_original;
exif_tag(16#9004) -> date_time_digitized;
exif_tag(16#9101) -> components_configuration;
exif_tag(16#9102) -> compressed_bits_per_pixel;
exif_tag(16#9201) -> shutter_speed_value;
exif_tag(16#9202) -> aperture_value;
exif_tag(16#9203) -> brightness_value;
exif_tag(16#9204) -> exposure_bias_value;
exif_tag(16#9205) -> max_aperture_value;
exif_tag(16#9206) -> subject_distance;
exif_tag(16#9207) -> metering_mode;
exif_tag(16#9208) -> light_source;
exif_tag(16#9209) -> flash;
exif_tag(16#920a) -> focal_length;
exif_tag(16#9214) -> subject_area;
exif_tag(16#927c) -> maker_note;
exif_tag(16#9286) -> user_comment;
exif_tag(16#9290) -> subsec_time;
exif_tag(16#9291) -> subsec_time_orginal;
exif_tag(16#9292) -> subsec_time_digitized;
exif_tag(16#a000) -> flashpix_version;
exif_tag(16#a001) -> color_space;
exif_tag(16#a002) -> pixel_x_dimension;
exif_tag(16#a003) -> pixel_y_dimension;
exif_tag(16#a004) -> related_sound_file;
exif_tag(16#a20b) -> flash_energy;
exif_tag(16#a20c) -> spatial_frequency_response;
exif_tag(16#a20e) -> focal_plane_x_resolution;
exif_tag(16#a20f) -> focal_plane_y_resolution;
exif_tag(16#a210) -> focal_plane_resolution_unit;
exif_tag(16#a214) -> subject_location;
exif_tag(16#a215) -> exposure_index;
exif_tag(16#a217) -> sensing_method;
exif_tag(16#a300) -> file_source;
exif_tag(16#a301) -> scene_type;
exif_tag(16#a302) -> cfa_pattern;
exif_tag(16#a401) -> custom_rendered;
exif_tag(16#a402) -> exposure_mode;
exif_tag(16#a403) -> white_balance;
exif_tag(16#a404) -> digital_zoom_ratio;
exif_tag(16#a405) -> focal_length_in_35mm_film;
exif_tag(16#a406) -> scene_capture_type;
exif_tag(16#a407) -> gain_control;
exif_tag(16#a408) -> contrast;
exif_tag(16#a409) -> saturation;
exif_tag(16#a40a) -> sharpness;
exif_tag(16#a40b) -> device_setting_description;
exif_tag(16#a40c) -> subject_distance_range;
exif_tag(16#a420) -> image_unique_id;
exif_tag(_)       -> unknown.

gps_tag(16#0000) -> gps_version_id;
gps_tag(16#0001) -> gps_latitude_ref;
gps_tag(16#0002) -> gps_latitude;
gps_tag(16#0003) -> gps_longitude_ref;
gps_tag(16#0004) -> gps_longitude;
gps_tag(16#0005) -> gps_altitude_ref;
gps_tag(16#0006) -> gps_altitude  ;
gps_tag(16#0007) -> gps_time_stamp;
gps_tag(16#0008) -> gps_satellites;
gps_tag(16#0009) -> gps_status;
gps_tag(16#000a) -> gps_measure_mode;
gps_tag(16#000b) -> gps_dop;
gps_tag(16#000c) -> gps_speed_ref;
gps_tag(16#000d) -> gps_speed;
gps_tag(16#000e) -> gps_track_ref;
gps_tag(16#000f) -> gps_track;
gps_tag(16#0010) -> gps_img_direction_ref;
gps_tag(16#0011) -> gps_img_direction;
gps_tag(16#0012) -> gps_map_datum;
gps_tag(16#0013) -> gps_dest_latitude_ref;
gps_tag(16#0014) -> gps_dest_latitude;
gps_tag(16#0015) -> gps_dest_longitude_ref;
gps_tag(16#0016) -> gps_dest_longitude;
gps_tag(16#0017) -> gps_dest_bearing_ref;
gps_tag(16#0018) -> gps_dest_bearing;
gps_tag(16#0019) -> gps_dest_distance_ref;
gps_tag(16#001a) -> gps_dest_distance;
gps_tag(16#001b) -> gps_processing_method;
gps_tag(16#001c) -> gps_area_information;
gps_tag(16#001d) -> gps_date_stamp;
gps_tag(16#001e) -> gps_differential;
gps_tag(_)       -> unknown.

tag_value(planar_configuration, 0) -> <<"Chunky format">>;
tag_value(planar_configuration, 1) -> <<"Planar format">>;

tag_value(sensing_method, 1) -> <<"Not defined">>;
tag_value(sensing_method, 2) -> <<"One-chip color area sensor">>;
tag_value(sensing_method, 3) -> <<"Two-chip color area sensor">>;
tag_value(sensing_method, 4) -> <<"Three-chip color area sensor">>;
tag_value(sensing_method, 5) -> <<"Color sequential area sensor">>;
tag_value(sensing_method, 6) -> <<"Trilinear sensor">>;
tag_value(sensing_method, 7) -> <<"Color sequential linear sensor">>;

tag_value(orientation, 1) -> <<"Top-left">>;
tag_value(orientation, 2) -> <<"Top-right">>;
tag_value(orientation, 3) -> <<"Bottom-right">>;
tag_value(orientation, 4) -> <<"Bottom-left">>;
tag_value(orientation, 5) -> <<"Left-top">>;
tag_value(orientation, 6) -> <<"Right-top">>;
tag_value(orientation, 7) -> <<"Right-bottom">>;
tag_value(orientation, 8) -> <<"Left-bottom">>;

tag_value(ycb_cr_positioning, 1) -> <<"Centered">>;
tag_value(ycb_cr_positioning, 2) -> <<"Co-sited">>;

tag_value(photometric_interpretation, 0) -> <<"Reversed mono">>;
tag_value(photometric_interpretation, 1) -> <<"Normal mono">>;
tag_value(photometric_interpretation, 2) -> <<"RGB">>;
tag_value(photometric_interpretation, 3) -> <<"Palette">>;
tag_value(photometric_interpretation, 5) -> <<"CMYK">>;
tag_value(photometric_interpretation, 6) -> <<"YCbCr">>;
tag_value(photometric_interpretation, 8) -> <<"CieLAB">>;

tag_value(custom_rendered, 0) -> <<"Normal process">>;
tag_value(custom_rendered, 1) -> <<"Custom process">>;

tag_value(exposure_mode, 0) -> <<"Auto exposure">>;
tag_value(exposure_mode, 1) -> <<"Manual exposure">>;
tag_value(exposure_mode, 2) -> <<"Auto bracket">>;

tag_value(white_balance, 0) -> <<"Auto white balance">>;
tag_value(white_balance, 1) -> <<"Manual white balance">>;

tag_value(scene_capture_type, 0) -> <<"Standard">>;
tag_value(scene_capture_type, 1) -> <<"Landscape">>;
tag_value(scene_capture_type, 2) -> <<"Portrait">>;
tag_value(scene_capture_type, 3) -> <<"Night scene">>;

tag_value(gain_control, 0) -> <<"Normal">>;
tag_value(gain_control, 1) -> <<"Low gain up">>;
tag_value(gain_control, 2) -> <<"High gain up">>;
tag_value(gain_control, 3) -> <<"Low gain down">>;
tag_value(gain_control, 4) -> <<"High gain down">>;

tag_value(saturation, 0) -> <<"Normal">>;
tag_value(saturation, 1) -> <<"Low saturation">>;
tag_value(saturation, 2) -> <<"High saturation">>;

tag_value(contrast, 0) -> <<"Normal">>;
tag_value(contrast, 1) -> <<"Soft">>;
tag_value(contrast, 2) -> <<"Hard">>;

tag_value(sharpness, 0) -> <<"Normal">>;
tag_value(sharpness, 1) -> <<"Soft">>;
tag_value(sharpness, 2) -> <<"Hard">>;

tag_value(exif_version, "0110") -> <<"Exif Version 1.1">>;
tag_value(exif_version, "0120") -> <<"Exif Version 1.2">>;
tag_value(exif_version, "0200") -> <<"Exif Version 2.0">>;
tag_value(exif_version, "0210") -> <<"Exif Version 2.1">>;
tag_value(exif_version, "0220") -> <<"Exif Version 2.1">>;
tag_value(exif_version, "0221") -> <<"Exif Version 2.21">>;
tag_value(exif_version, _)      -> <<"Unknown Exif Version">>;

tag_value(flashpix_version, "0100") -> <<"FlashPix Version 1.0">>;
tag_value(flashpix_version, "0101") -> <<"FlashPix Version 1.01">>;
tag_value(flashpix_version, _)      -> <<"Unknown FlashPix version">>;

tag_value(file_source, 3) -> <<"DSC">>;

% TODO: copyright, components_configuration, scene_type, ycb_cr_sub_sampling,
%       subject_area, gps_altitude_ref

tag_value(_Name, Value) -> Value.

uread(Bin, End) ->
    binary:decode_unsigned(Bin, End).

uread_many(<<>>, _Size, _End) ->
    [];
uread_many(Bin, Size, End) ->
     << Num:Size/binary, Rest/binary >> = Bin,
    [uread(Num, End) | uread_many(Rest, Size, End)].
