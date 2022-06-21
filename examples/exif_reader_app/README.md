exif_reader_app
=====

An OTP application

This app just read selected file data and returns a result.

## Build

`$ rebar3 compile`

## Run

`rebar3 shell`
	
## Build and run

```
$ rebar3 compile && rebar3 shell
===> Verifying dependencies...
===> Upgrading erlang_exif (from {path,"../..",{mtime,<<"2022-06-03T18:51:13Z">>}})
===> Analyzing applications...
===> Compiling erlang_exif
===> Analyzing applications...
===> Compiling exif_reader_app
===> Verifying dependencies...
===> Upgrading erlang_exif (from {path,"../..",{mtime,<<"2022-06-03T18:51:29Z">>}})
===> Analyzing applications...
===> Compiling erlang_exif
===> Analyzing applications...
===> Compiling exif_reader_app
===> Booted erlang_exif
===> Booted exif_reader_app
Eshell V12.2  (abort with ^G)
1> gen_server:call(reader, "priv/img/empty_tag.jpg").
{ok,{ok,#{contrast => <<"Normal">>,gain_control => <<"Normal">>,
          orientation => <<"Top-left">>,
          image_description => <<"HELLOMOTO">>,
          model => <<"EZX Camera">>,
          date_time_digitized => <<"2009:01:11 14:00:52">>,
          flashpix_version => <<"FlashPix Version 1.0">>,
          metering_mode => 1,saturation => <<"Normal">>,
          white_balance => <<"Manual white balance">>,
          pixel_x_dimension => 768,
          shutter_speed_value => {ratio,0,0},
          exposure_mode => <<"Auto exposure">>,resolution_unit => 2,
          file_source => <<"DSC">>,light_source => 0,
          components_configuration => "1230",
          user_comment => "EZX phone",flash => 0,
          scene_capture_type => <<"Standard">>,
          pixel_y_dimension => 1024,software => <<"EZX V1.0.0">>,
          date_time_original => <<"2009:01:11 14:00:52">>,
          make => <<"Motorola">>,
          brightness_value => {ratio,0,0},...}}}
2> gen_server:call(reader, "priv/img/ifd_end.jpg").
{ok,{ok,#{contrast => <<"Normal">>,orientation => <<"Top-left">>,
          image_description => <<"                               ">>,
          compressed_bits_per_pixel => {ratio,2,1},
          model => <<"COOLPIX S7   ">>,
          host_computer => <<"Mac OS X 10.3.9">>,
          custom_rendered => <<"Normal process">>,
          date_time_digitized => <<"2006:11:08 13:29:14">>,
          flashpix_version => <<"FlashPix Version 1.0">>,
          metering_mode => 5,saturation => <<"Normal">>,
          exposure_bias_value => {ratio,0,10},
          white_balance => <<"Manual white balance">>,
          pixel_x_dimension => 3072,
          exposure_mode => <<"Auto exposure">>,resolution_unit => 2,
          file_source => <<"DSC">>,light_source => 4,
          components_configuration => [1,2,3,0],
          digital_zoom_ratio => {ratio,0,100},
          flash => 16,scene_capture_type => <<"Standard">>,
          pixel_y_dimension => 2304,
          software => <<"Adobe Photoshop CS Macintosh">>,
          date_time_original => <<"2006:11:08 13:29:14">>,...}}}
3> gen_server:call(reader, "priv/img/with_exif.jpg").
{ok,{ok,#{orientation => <<"Top-left">>,model => <<"iPhone 5s">>,
          subject_area => [1631,1223,1795,1077],
          date_time_digitized => <<"2014:04:23 13:33:08">>,
          flashpix_version => <<"FlashPix Version 1.0">>,
          subsec_time_orginal => <<51,52,53,0>>,
          metering_mode => 5,
          subsec_time_digitized => <<51,52,53,0>>,
          white_balance => <<"Auto white balance">>,
          pixel_x_dimension => 512,
          shutter_speed_value => {ratio,11180,979},
          exposure_mode => <<"Auto exposure">>,resolution_unit => 2,
          components_configuration => [1,2,3,0],
          flash => 16,scene_capture_type => <<"Standard">>,
          pixel_y_dimension => 384,
          software => <<55,46,49,0>>,
          date_time_original => <<"2014:04:23 13:33:08">>,
          make => <<"Apple">>,
          brightness_value => {ratio,9671,876},
          focal_length => {ratio,103,25},
          exposure_time => {ratio,1,2740},
          sensing_method => <<"One-chip color area sensor">>,
          exif_version => <<"Exif Version 2.21">>,...}}}
4> gen_server:call(reader, "priv/img/with_jfif.jpg").
{ok,{ok,#{}}}
5> gen_server:call(reader, "priv/img/with_jfif_exif.jpg").
{ok,{ok,#{orientation => <<"Top-left">>,model => <<"iPhone 5s">>,
          subject_area => [1631,1223,1795,1077],
          date_time_digitized => <<"2014:04:23 13:33:08">>,
          flashpix_version => <<"FlashPix Version 1.0">>,
          subsec_time_orginal => <<51,52,53,0>>,
          metering_mode => 5,
          subsec_time_digitized => <<51,52,53,0>>,
          white_balance => <<"Auto white balance">>,
          pixel_x_dimension => 512,
          shutter_speed_value => {ratio,11180,979},
          exposure_mode => <<"Auto exposure">>,resolution_unit => 2,
          components_configuration => [1,2,3,0],
          flash => 16,scene_capture_type => <<"Standard">>,
          pixel_y_dimension => 384,
          software => <<55,46,49,0>>,
          date_time_original => <<"2014:04:23 13:33:08">>,
          make => <<"Apple">>,
          brightness_value => {ratio,9671,876},
          focal_length => {ratio,103,25},
          exposure_time => {ratio,1,2740},
          sensing_method => <<"One-chip color area sensor">>,
          exif_version => <<"Exif Version 2.21">>,...}}}
```		  