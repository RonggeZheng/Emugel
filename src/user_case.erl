%%RUN COMAND:
%%To run you melody, please type command in the console): 

%%	erlc user_case.erl
%%  erl -noshell -s main start -s init stop
%%Then press Enter to send command to run.

%%To stop you melody, please type command in the console: 
%%	erl -noshell -s main stop -s init stop
%%Then press Enter to send command to run.

-module(user_case).
-behaviour(melody).
-compile(export_all).


beats_per_minute() ->
    90.


pitch_based_track() ->
[
    {
    "melody",
     loop,
     5.0,
     10.00,
    "piano", 
    [
    {b1, '_'},
    {b2, '__'},
    {e1, '__'},
	{e2, '___'},
	{b3, '__'},
	{60.5, '__'}
    ],
    no_effect,
    [{attack, 1.0}, {decay, 1.0}, {sustain, 1.0},
    {release, 1.0}, {amplitude, 0.1}]
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
    {"sample1", 2, 3.254, 3.067,  "drum_heavy_kick", [x, o, x, o, x, x], no_effect,  [{amplitude, 0.25}] },
    {"sample2", 1, 2.36, no_delay, "drum_snare_hard", [o, x, o, x, o, o], no_effect, [{amplitude, 0.25}] }
    ].


rubato_pattern_track() ->
[
  {"rubato1", 1, '_____', '_', 2.35, "drum_heavy_kick", [x, x, x, x, x, x, x, x, x, x, x, x ,x ], no_effect,  [{amplitude, 1.0}] }
].

choose_play() ->
[
   {
    "choose1", 
    12, 
    2.00,
	"piano",
    [65.89, d2, e2],
    ['__', '___', '_____']
    }
].





