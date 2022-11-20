%%RUN COMAND:
%%To run the melody file, please type command in the console(remember to include the period): 
%%	sound_generate:main(your_file_name, 1).
%%Then press Enter to send command to run.

%%To stop the melody, please type command in the console(remember to include the period): 
%%	sound_generate:stop().
%%Then press Enter to send command to run.

%% This part is used to compile, users can ignore it. 
-module(api).
-behaviour(melody).
-compile(export_all).


%% FUNCTION ONE: beats_per_minute()

%%Here users can change the bpm/tempo of the melody, they can define any number between 60 and 180. 
%%Here are "typical" tempo ranges for a number of common genres:
%%	Dub: 60-90 bpm
%%	Hip-hop: 60-100 bpm
%%	House: 115-130 bpm
%%	Techno/trance: 120-140 bpm
%%	Dubstep: 135-145 bpm
%%	Drum and bass: 160-180 bpm

%% Example 1:
beats_per_minute() ->
    60.

%% Example 2:
beats_per_minute() ->
    100.

%% Example 3:
beats_per_minute() ->
    128.

%% Example 4:
beats_per_minute() ->
    145.

%% Example 5:
beats_per_minute() ->
    160.

%% Example 6:
beats_per_minute() ->
    170.
    
%%FUNCTION TWO: pitch_based_track()

%%Use this function to define some pitch based tracks. Each tuple is one track. Each track consists of 
%% the following items: name, mode, duration, delay, instrument, sound, effect, envelope
%% name: the name of this track. name needs to be surrounded by double quotes.
%% mode: choose from loop mode or non-loop mode for this track. Use keyword 'loop' to open loop mode. Or an positive integer(like '2') to open non-loop mode. The integer will represents the iteration time of this track. 
%% duration: a float number with at most three decimal places which represents the duration of this track. Eg, '3.25'.
%% delay: how long you want this track to be delayed when playing. A float number with at most three decimal places should be given. Eg, '2.89'
%% instrument: the instrument of this track. Options: 
    %%
%% sound: 
%% effect: 
%% envelope: 


%% Example 1:


%% Example 2:



%% FUNCTION FOUR: sample_pattern_track()

%%This function is used to define the pattern tracks.
%%Every tuple inside a '{}' is an independent track. For example, 
%%			{"drum_heavy_kick", [x, o, x, o], echo, [{amplitude, 0.5}] }

%%Each tuple, you should define 
%%			1. the sample name 
%%				(like: 
%%				"bass_dnb_f",
%%				"drum_heavy_kick", "drum_snare_hard",
%%				"guit_harmonics","guit_e_fifths",  
%%				"ambi_piano",
%%				"perc_till"
%%				"loop_safari","loop_mehackit1","loop_electric",
%%				"table_na_o" )
%%			2. the pattern for playing this sample 
%%				(like: 
%%					[x, o, x, o],
%%					[x,x,x,x,x,x],
%%					[x,o,o]
%%				),
%% 				x means that play for 1 beat 
%% 				o means that rest for 1 beat
%%			3. effect of this sample. 
%%				(like: echo, ping_pong, record, reverb)
%%			   If you don't want to have any effect, you can input (no_effect). 
%%			4. envelope of this sample. You can use it to modify the volume/amplitude of the smaple.
%%			   Its value can vary from 0.0 to 1.0  
%%				(like: [{amplitude, 0.5}], [{amplitude, 1.0}]; [{amplitude, 0.0}]; [{amplitude, 0.6}])
%%			   If you don't want to have any sample, you can input (no_envelope). 

%%Example 1:
sample_pattern_track() ->
    [
     {"drum_heavy_kick", [x, o, x, o], echo, [{amplitude, 0.5}] }, 
     {"drum_snare_hard", [o, x, o, x], no_effect, no_envelope}, 
     {"guit_e_fifths", [x, x, x, x], ping_pong, no_envelope},
     {"perc_till", [x, o, o, o], record,  [{amplitude, 0.6}] }
    ].

%%Example 2:
sample_pattern_track() ->
    [
     {"guit_harmonics", [x, o, x, o], echo, [{amplitude, 0.5}] }, 
     {"drum_snare_hard", [o,x,x], no_effect, no_envelope}, 
     {"loop_compus", [x], ping_pong, no_envelope},
    ].


%%Example 3:
sample_pattern_track() ->
    [
     {"ambi_glass_hum", [x, o, x, o], echo, [{amplitude, 0.5}] }, 
     {"drum_snare_hard", [o, o, x], reverb, no_envelope}, 
     {"guit_harmonics", [x], no_effect, no_envelope},
     {"drum_heavy_kick", [x, o, o, o], record, [{amplitude, 0.8}] },
	 {"guit_e_fifths", [x, o, o, o,x], record, [{amplitude, 0.8}] }
    ].

%% FUNCTION FIVE: rubato_pattern_track()

%% FUNCTION SIX: choose_play() 
choose_play() ->
    [].

