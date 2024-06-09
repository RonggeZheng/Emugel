
# Emugel

## Run Command
To run your melody, please open a command line, change your current directory to where the `Emugel` folder is (e.g., `E:\code\Emugel`), then type the commands in the console:

```sh
erlc user_case.erl
erl -noshell -s main start -s init stop
```

Then press Enter to run the command.

To stop your melody, please type the following command in a console:

```sh
erl -noshell -s main stop -s init stop
```

Then press Enter to run the command.

## Functions

This part is used to compile; users can ignore it.

```erlang
-module(api).
-behaviour(melody).
-compile(export_all).
```

### Function One: `beats_per_minute()`

Here users can change the BPM (tempo) of the melody. BPM value can be any number between 60 and 180. Here are "typical" tempo ranges for a number of common genres:

- Dub: 60-90 bpm
- Hip-hop: 60-100 bpm
- House: 115-130 bpm
- Techno/trance: 120-140 bpm
- Dubstep: 135-145 bpm
- Drum and bass: 160-180 bpm

Example 1:
```erlang
beats_per_minute() ->
    60.
```

Example 2:
```erlang
beats_per_minute() ->
    100.
```

Example 3:
```erlang
beats_per_minute() ->
    128.
```

### Function Two: `pitch_based_track()`

Use this function to define some pitch-based tracks. Each tuple is one track. Each track consists of the following items: name, mode, duration, delay, instrument, sound, effect, envelope.

#### Example 1:
```erlang
pitch_based_track() ->
[
    {
    "melody1",
     loop,
     3.25,
     4.00,
    "sine", 
    [
    {60.24, '___'},
    {rest, '_'},
    {60.5, '_____'},
    {rest, '___'}
    ],
    wobble,
    [{amplitude, 0.5}]
    }
].
```

#### Example 2:
```erlang
pitch_based_track() ->
[
    {
    "melody2",
     2,
     3.00,
     7.67,
    "piano", 
    [
    {c4, '___'},
    {rest, '_'},
    {d4, '___'},
    {rest, '_}
    ],
    echo,
    [{amplitude, 0.5}]
    },
    {
    "melody3",
     3,
     3.00,
     8.00,
    "beep", 
    [
    {64, '___'},
    {rest, '_'},
    {d4, '___'},
    {rest, '_}
    ],
    no_effect,
    [{amplitude, 0.5}]
    }
].
```

### Function Three: `chord_play()`

Use this function to define some chord-based tracks. In each track, users could define a chord to play all the notes in this chord together.

#### Example 1:
```erlang
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
    }
].
```

#### Example 2:
```erlang
chord_play() ->
[
   {
    "chord2", 
    1,
    no_delay,
    "piano",
    e3, minor7,
    ['___', '____', '_____', '__'],
    no_effect,
    [{amplitude, 1.0}]
    },
    {
    "chord3", 
    1,
    4.00,
    "piano",
    c4, major,
    ['___', '____', '_____'],
    no_effect,
    [{amplitude, 1.0}]
     }
].
```

### Function Four: `sample_pattern_track()`

This function is used to define the pattern tracks in which you could play some samples according to a specific pattern.

#### Example 1:
```erlang
sample_pattern_track() ->
    [
     {"sample1", 1, 2.1, no_delay, "drum_heavy_kick", [x, o, o], echo, [{amplitude, 0.5}]}, 
     {"sample2", 1, 4.1, 4.25, "drum_snare_hard", [o, x, o, x], no_effect, no_envelope}, 
     {"sample3", loop, 5.1, 4.25, "guit_e_fifths", [x, x, x, x, x], ping_pong, no_envelope},
     {"sample4", 1, 2.0, 4.25, "perc_till", [x, o, o, o], wobble, [{amplitude, 0.6}]}
    ].
```

#### Example 2:
```erlang
sample_pattern_track() ->
    [
     {"sample1", 1, 3.1, no_delay, "guit_harmonics", [x, o, x, o], echo, [{amplitude, 0.5}]}, 
     {"sample2", 1, 3.6, no_delay, "drum_snare_hard", [o, x, x], no_effect, no_envelope}, 
     {"sample3", 1, 0.8, no_delay, "loop_compus", [x], ping_pong, no_envelope}
    ].
```

### Function Five: `rubato_pattern_track()`

This function is used to define the rubato pattern tracks. This kind of track provides transitions from a given initial duration to a given end duration according to a sequence of `xo` events.

#### Example 1:
```erlang
rubato_pattern_track() ->
[
 {"rubato1", 1, '____', '_', no_delay, "drum_heavy_kick", [x, x, x, x, x, x], no_effect, [{amplitude, 1.0}]}
].
```

#### Example 2:
```erlang
rubato_pattern_track() ->
[
 {"rubato1", loop, '_', '_____', 6.00, "drum_heavy_kick", [x, x, x, x, x, x, x, x, x, x, x, x, x], no_effect, [{amplitude, 1.0}]},
 {"rubato2", 1, '_____', '_', no_delay, "drum_heavy_kick", [x, o, x, x, o, x, x, x], no_effect, [{amplitude, 1.0}]}
].
```

### Function Six: `choose_play()`

This function is used to define the choose-based tracks. In this kind of track, the program could play some random music by randomly choosing items from the note and beat lists that users give.

#### Example 1:
```erlang
choose_play() ->
[
   {
    "choose1", 
    5, 
    4.00,
    "piano",
    [eb4, 65.89, gb4],
    ['_', '_____', '_____', '__']
    }    
].
```

#### Example 2:
```erlang
choose_play() ->
[
   {
    "choose1", 
    loop, 
    no_delay,
    "prophet",
    [d5, e5, 78.59],
    ['__', '___']
    },
    {
    "choose2", 
    6, 
    10.00,
    "piano",
    [60.25, 65.89, 78.59],
    ['__', '___', '_____', '_', '___']
    }
].
```

## Comments
If you don't want to use any of the functions, simply comment or empty the main body of that function by adding `%` ahead of each line. Notepad++ provides the operation of 'Block comment' if you select the block then right click your mouse. REMEMBER: you should ONLY comment the code between the square brackets `[]`! Or else your music file will not run properly.

Example of commenting the main body of unused function:
```erlang
sample_pattern_track() ->
[
   % {"ambi_glass_hum" [x o x o] echo [{amplitude 0.5}]}, 
   % {"drum_snare_hard" [o o x] reverb, no_envelope}
]
```

Example of emptying the main body of unused function:
```erlang
sample_pattern_track() -> 
[]
```
