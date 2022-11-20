%%RUN COMAND:
%%To run you melody, please type command in the console(remember to include the period): 
%%	sound_generate:main(user_case1).
%%Then press Enter to send command to run.

%%To stop you melody, please type command in the console(remember to include the period): 
%%	sound_generate:stop().
%%Then press Enter to send command to run.

-module(user_case1).
-behaviour(melody).
-compile(export_all).


beats_per_minute() ->
    90.


pitch_based_track() ->
[
    {
    "melody",
    "pretty_bell", 
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


sample_pattern_track() ->
    [
     {"drum_heavy_kick", [x, o, x, o, x, x], no_effect,  [{amplitude, 0.25}] },
    {"drum_snare_hard", [o, x, o, x, o, o], no_effect, [{amplitude, 0.25}] }
    ].



choose_play() ->
[
   {
    "choose1", 
	"piano",
    [65.89, d2, e2],
    ['__', '___', '_____']
    }
].


