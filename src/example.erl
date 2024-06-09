%%This is an example file to show the usage of all functions.


-module(example).
-behaviour(melody).
-compile(export_all).


beats_per_minute() ->
    60.


pitch_based_track() ->
[
    {
    "melody1",
	 2,
	 3.25,
	 4.67,
    "sine", 
    [
    {60.24, '___'},
    {rest, '___'},
    {60.5, '___'},
    {rest, '___'}
    ],
    no_effect,
    [ {amplitude, 0.1}]
    }
 ].

chord_play() ->
[
   {
    "chord1", 
    1,
    no_delay,
	"beep",
    e3, minor,
    ['___', '____', '_____'],
	no_effect,
	[{amplitude, 1.0}]
    },
    {
    "chord2", 
    1,
    4.00,
	"beep",
    c4, major,
    ['___', '____', '_____'],
	no_effect,
	[{amplitude, 1.0}]
     }
].


sample_pattern_track() ->
[
   {"sample1", 1, 2.1, 4.25, "drum_heavy_kick", [x, o, x, o], no_effect,  [{amplitude, 0.25}] },
   {"sample2", loop, 4.1, no_delay, "drum_snare_hard", [o, x, o, x], no_effect, [{amplitude, 0.25}] }
].


rubato_pattern_track() ->
[
 {"rubato1", 1, '_____', '_', 2.35, "drum_heavy_kick", [x, x, x, x, x, x, x, x, x, x, x, x ,x ], no_effect,  [{amplitude, 1.0}] }
].



choose_play() ->
[
   {
    "choose1", 
    10,
    4.00,
	"piano",
    [60.25, 65.89, 78.59],
    ['__', '___', '_____']
    }	 
].




