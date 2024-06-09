%% This module is used to is transfer the song in music file 
%%to Sonic Pi code and plays song with Sonic Pi server using the Sonic Pi Tool

-module(sound_generate).

-export([playMelody/1,stop/0]).


%% main function is used to play music using Erlang files
%%'Arg' is file name, e.g. main(user_case1)
playMelody(Arg) ->
	use_eval_bpm(Arg),
	
    use_eval_play_pitch(Arg),
	
	%%transfer the pattern track 
    use_eval_play_pattern_track(Arg),
    
    %%transfer the choose_play track  
    use_eval_play_choose(Arg),
	
	%%transfer the rubato_pattern track  
    use_eval_play_rubato_pattern(Arg),
	
	%%transfer the chord play track  
    use_eval_play_chord(Arg),
	   
    %%write tail command of sonic-pi-tool
    writeTail(), 
    
    %%run the eval command of Sonic PI tool to play the music 
    play().


stop() ->
	 %% Get file handler and write
    {ok, Fh} = file:open("out.rb", [write]),
    
    %% Write the text
    N = "python sonic-pi-tool.py stop",
    file:write(Fh, N),
	{Status,Value} = file:open("out.rb",read),
    if Status =:= ok ->
            io:format("==== The Server Is Stopping Your Melody  ====~n"),
            %%read file, read one line from file 
            OneLine = io:get_line(Value,""),			
            os:cmd (OneLine);
	        
        Status =/= ok ->
            io:format("open file error:cannot open out.rb!")
    end.

%write synth (instrument)
write_synth(Instrument) ->
	Synth = Instrument,
    write("use_synth :" ++ Synth ++ "; ").

write_effect(Effect) ->
	if 
        Effect == no_effect ->
            ok;
        true ->
             write("with_fx :"++ atom_to_list(Effect) ++" do; ")
    end.

write_envelope(Envelope) ->
    if
        Envelope == no_envelope ->
            ok;
        true ->
            write("use_synth_defaults "),
            
            %% divide envelope list into two parts: the last one and the others 
            Last_envelope = lists:last(Envelope),
            Dropped_envelope =  lists:droplast(Envelope),

            %deal with each envelope element in dropped envelope list 
            lists:map(
                fun({EnvelopeName, EnvelopeValue}) ->
                    envelope(EnvelopeName, EnvelopeValue)
                end, Dropped_envelope),
            
            %deal with the last envelope element
            last_envelope(Last_envelope)
    end.

write_effect_end(Effect) ->
	 if 
        Effect == no_effect ->
            ok;
        true ->
            write("end;")
     end.

write_end(Mode) ->
	 if   		
        is_integer(Mode)-> 
           write("end; end; ");
		%write live_loop
        true ->  
          write("end; ")
    end.

use_eval_play_chord(Arg) ->
    RawChords = Arg:chord_play(),
    
    %%extract each sound tuple in sound track and transfer it 
    lists:map(
        fun({Name, Mode, Delay, Instrument, Note, ChordType, Beats, Effect, Envelope}) ->
            make_chord(Name, Mode, Delay, Instrument, Note, ChordType, Beats, Effect, Envelope)
        end, RawChords).

make_chord(Name, Mode, Delay, Instrument, Note, ChordType, Beats, Effect, Envelope) ->	
	if   
		%write iteration times
        is_integer(Mode) andalso Mode > 0 -> 
           write("in_thread name: :" ++ Name),
		   if 
			   is_number(Delay) ->
				   	write(", delay:" ++ float_to_list(Delay, [{decimals, 3}, compact]));
			   true ->
				    ok
		   end,
		   write(" do; "),
		   write(integer_to_list(Mode) ++ ".times" ++ " do; ");
		%write live_loop
     true ->  
           write("live_loop :" ++ Name),
		   if 
			   is_number(Delay) ->
				   	write(", delay:" ++ float_to_list(Delay, [{decimals, 3}, compact])); 
			   true ->
				    ok
		   end,
		   write(" do; ")
    end,

    %write FX (effect)
    write_effect(Effect),
    
    %write synth (instrument)
    write_synth(Instrument),
    
    %write envelope
	write_envelope(Envelope),

    %%get beats list
	TransferredBeats = lists:map(
       fun(Beat) ->
           transform(Beat)
       end, Beats),
	
	%% write the notes in the chord
	if 
		ChordType == major7 orelse ChordType == minor7 ->
			if 
				length(TransferredBeats) == 4 ->
		    		write_chord(4, Note, ChordType,TransferredBeats, 0);
				true ->
					io:format("ERROR: The length of Beats list in this method should be 4 regards to your Chord type. Please rewrite the method."),
			   		erlang:halt(0)
			end;
	    true ->
			if 
				length(TransferredBeats) == 3->
					write_chord(3, Note, ChordType,TransferredBeats, 0);
				true ->
					io:format("ERROR: The length of Beats list in this method should be 3 regards to your Chord type. Please rewrite the method."),
			   		erlang:halt(0)
			end
	end,

    %end of FX
	write_effect_end(Effect),
	 
	%%write end
	write_end(Mode).

write_chord(ChordLength, Note, ChordType, TransferredBeats, Offset) ->
	CurrentBeat = lists:nth(Offset + 1, TransferredBeats), 
	Cmd = "in_thread do; play chord(:" ++ atom_to_list(Note) ++ ", :" ++ atom_to_list(ChordType) 
++ ")[" ++ integer_to_list(Offset) ++ "], release:"  ++ float_to_list(CurrentBeat, [{decimals, 4}, compact]) ++ "; sleep " 
++ float_to_list(CurrentBeat, [{decimals, 4}, compact]) ++ ";" ++ "end;",
    write(Cmd),
	if
		ChordLength - 1 == 0 ->
			write_chord(0);
		true ->
			write_chord(ChordLength - 1, Note, ChordType, TransferredBeats, Offset + 1)
	end.

write_chord(0) ->
	ok.
  
%% use Eval command of Sonic Pi tool to play the rubato_pattern track 
%% it transfers the music file into Sonic Pi code and writes code into 'out.rb'
use_eval_play_rubato_pattern(Arg) ->
    RawPattern = Arg:rubato_pattern_track(),    
    
    %%extract each rubato_pattern tuple in rubato_pattern track and transfer it 
    lists:map(
        fun({Name, Mode, First, Last, Delay, Sample, Pattern, Effect, Envelope}) ->
            make_rubato_pattern(Name, Mode, First, Last, Delay, Sample, Pattern, Effect, Envelope)
        end, RawPattern).

make_rubato_pattern(Name, Mode, First, Last, Delay, Sample, Pattern, Effect, Envelope) ->
	%%check the validation of the arguments
    if   
        is_integer(Mode) andalso Mode > 0 -> 
           ok;
		%must be in no-loop mode
        true ->  
           io:format("ERROR: The 'mode' parameter in this method must be a positive integer. Please rewrite the method."),
		   erlang:halt(0)
    end,
	
    NumberFirst = transform(First),
	NumberLast =transform(Last),
    PatternLength = length(Pattern),
	
	%write iteration times  
           write("in_thread name: :" ++ Name),
		   if 
			   is_number(Delay) ->
				   	write(", delay:" ++ float_to_list(Delay, [{decimals, 3}, compact]));
			   true ->
				    ok
		   end,
		   write(" do; "),
		   write(integer_to_list(Mode) ++ ".times" ++ " do; "),
     
    %write FX (effect)
    write_effect(Effect),
    
	%%write envelope
    write_envelope(Envelope),
    
    %%write sample and sleep
	Increment = (NumberLast - NumberFirst) / (PatternLength - 1),


    lists:foldl(fun(P, NewDuration) -> 
		writePatternWithIncre(Sample, P, NewDuration),
		NewDuration + Increment end, NumberFirst, Pattern),

    %%end of FX
    write_effect_end(Effect),

    %%write end
	write_end(Mode).	


writePatternWithIncre(Sample, P, CurrentDuration) -> 
	 if 
        P == x ->
            Cmd = "sample :" ++ Sample ++ "; sleep " ++ float_to_list(CurrentDuration, [{decimals, 4}, compact]) ++"; ",
            write(Cmd);
        true ->
            Cmd = "sleep " ++ float_to_list(CurrentDuration, [{decimals, 4}, compact]) ++"; ",
            write(Cmd)
     end.

use_eval_bpm(Arg) ->
	%%write headline command of sonic-pi-tool
    writeHeadline(),
    
    %%write bmp as global variable
    Bmp = integer_to_list(Arg:beats_per_minute()),
    write("use_bpm " ++ Bmp ++ "; ").

%% use Eval command of Sonic Pi tool to play the pitch base track
%% it transfers the music file into Sonic Pi code and writes code into 'out.rb'
use_eval_play_pitch(Arg) ->
    RawSounds = Arg:pitch_based_track(),
    
    %%extract each sound tuple in sound track and transfer it 
    lists:map(
        fun({Soundname, Mode, Duration, Delay, Instrument, Sound, Effect, Envelope}) ->
            sound(Soundname, Mode, Duration, Delay, Instrument, Sound, Effect, Envelope)
        end, RawSounds).

    

%% transfer sound tuple into Sonic Pi code, each tuple contains a set of notes 
sound(Soundname, Mode, Duration, Delay, Instrument, Sound, Effect, Envelope) ->
	
	if   
		%write iteration times
        is_integer(Mode) andalso Mode > 0 -> 
           write("in_thread name: :" ++ Soundname),
		   if 
			   is_number(Delay) ->
				   	write(", delay:" ++ float_to_list(Delay, [{decimals, 3}, compact]));
			   true ->
				    ok
		   end,
		   write(" do; "),
		   write(integer_to_list(Mode) ++ ".times" ++ " do; ");
		%write live_loop
     true ->  
           write("live_loop :" ++ Soundname),
		   if 
			   is_number(Delay) ->
				   	write(", delay:" ++ float_to_list(Delay, [{decimals, 3}, compact]));
			   true ->
				    ok
		   end,
		   write(" do; ")
    end,



    %write FX (effect)
    write_effect(Effect),
    
    %write synth (instrument)
    write_synth(Instrument),
    
    %write envelope
    write_envelope(Envelope),

	if 
		is_integer(Mode) andalso Mode > 0 ->			
			%% if now in the no_loop mode, get the Beats items
		    BeatsList = lists:map(
						  fun({_, Beats}) ->
								  Beats
						  end, Sound),
			NumericBeatsList = numeric_beats(BeatsList),
		    %%write trimmed sound to the file
		    loop(1, NumericBeatsList, Duration, Sound);
		true ->
			%%write the whole sound list to file
			lists:map(
		    fun({Note, Beats}) ->
		        soundcontent(Note, Beats)
		    end, Sound)
	 end,

    
     %end of FX
    write_effect_end(Effect),
	 
	 %%write end
	write_end(Mode).


numeric_beats(BeatsList) ->
	 NumericBeatsList = lists:map(
		fun(Beat) ->
		  	transform(Beat)
     	end, BeatsList),
	NumericBeatsList. 

transform(Beat) ->
	case Beat of		 
         '_' -> New  = 0.25;
         '__' -> New  = 0.5;
         '___' -> New  = 1.0;
         '____' -> New  = 2.0;
         '_____' -> New  = 4.0
	 end,
     New.

%%find the index in the beats list where the duration limit is reached
loop(-1) ->
	ok.

loop(Count, BeatsList, Duration, Sound) when Duration > 0.0 ->
	NewList = lists:sublist(BeatsList, Count),
	Sum = lists:sum(NewList),
    if 
		    Sum > Duration ->
			Index = Count - 1,
			if 
				Index == 0 ->
					trim_sound_list(Index, Sound, Duration);
			    true ->
					PreList = lists:sublist(BeatsList, Count - 1),
					SumPreList = lists:sum(PreList),
					trim_sound_list(Index, Sound, Duration - SumPreList)
			end,

			loop(-1);
		   true ->
				if
					Count == length(Sound) ->
						trim_sound_list(Count, Sound, Duration),

						loop(-1);
					true ->
						loop(Count+1, BeatsList, Duration, Sound)
				end
	end.


%%trim sound list to fit the duration, write to the file
trim_sound_list(Index, Sound, Duration) ->
	Length = length(Sound),
	case Index of 
		0 ->      %%first beat exceeds the total duration
			FirstTuple = lists:nth(1, Sound),
			FirstNote = element(1, FirstTuple),
			NewSoundList = [{FirstNote, Duration}];
		Length ->
			NewSoundList = lists:sublist(Sound, Index);
		_ ->
			LastTuple = lists:nth(Index + 1, Sound),
			LastNote = element(1, LastTuple),
			NewSound = [{LastNote, Duration}],
			List1 = lists:sublist(Sound, Index),
			NewSoundList = lists:append(List1, NewSound)
	end,
	%write play and sleep
    lists:map(
    fun({Note, Beats}) ->
        soundcontent(Note, Beats)
    end, NewSoundList). 

%%tranfer the last envelop element in envelope list into Sonic Pi code 
last_envelope(Last_envelope) ->
    {EnvelopeName, EnvelopeValue} = Last_envelope,
    write(atom_to_list(EnvelopeName) ++ ": " ++ float_to_list(EnvelopeValue, [{decimals, 4}, compact]) ++ "; ").
    
%% transfer the envelope element into Sonic Pi code 
envelope(EnvelopeName, EnvelopeValue) ->
     write(atom_to_list(EnvelopeName) ++ ": " ++ float_to_list(EnvelopeValue, [{decimals, 4}, compact]) ++ ", ").

%% transfer one sound tuple, which has a note and its duration 
%%duration which is more than 1 beat needs to change the envelope attributes to stretch Beat 
soundcontent(Note, Beats) when Beats > 0.0 ->
	    
	if 
        is_float(Note) ->
           Cmd = "play " ++ float_to_list(Note, [{decimals, 4}, compact]);
        true ->
           Cmd = "play :" ++ atom_to_list(Note)
     end,
	
     case Beats of
		 
         '_' -> New_Cmd  = Cmd ++ "; sleep 0.25; ";
         '__' -> New_Cmd  = Cmd ++ "; sleep 0.5; ";
         '___' -> New_Cmd  = Cmd ++ "; sleep 1.0; ";
         '____' -> New_Cmd  = Cmd ++ ", release: 2.0; " ++ "sleep 2.0; ";
         '_____' -> New_Cmd  = Cmd ++ ", release: 4.0; " ++ "sleep 4.0; ";
		  _ -> New_Cmd = write_duration(Cmd, Beats)			 
     end,
     write(New_Cmd).

write_duration(Cmd, Beats) ->
	New_Cmd  = Cmd ++ ", release: "++ float_to_list(Beats, [{decimals, 4}, compact]) ++"; " ++ "sleep "++ float_to_list(Beats, [{decimals, 4}, compact]) ++"; ",
	New_Cmd.
			 	

%%write the command into output file 
write(Cmd) ->
    %% Get file handler and append
    {ok, Fh} = file:open("out.rb", [append]),
    file:write(Fh, Cmd).

%%write the head line of eval command 
writeHeadline() ->
    %% Get file handler and write
    {ok, Fh} = file:open("out.rb", [write]),
    
    %% Write the text
    N = "python sonic-pi-tool.py eval " ++ "\"",
    file:write(Fh, N).

%%write the tail line of eval command 
writeTail() ->
    %% Get file handler and append
    {ok, Fh} = file:open("out.rb", [append]),
    N = "\"",
    file:write(Fh, N).

%%run the eval command to play music 
play() ->
    {Status,Value} = file:open("out.rb",read),
    if Status =:= ok ->
            io:format("==== The Server Is Playing Your Melody  ====~n"),
            %%read file, read one line from file 
            OneLine = io:get_line(Value,""),
			io:format("Please see the transferred Sonic Pi code by your music file ~n~p ~n",[OneLine]),
            os:cmd (OneLine);
	        
        Status =/= ok ->
            io:format("open file error:cannot open out.rb!")
    end.


%%transfer pattern track into Sonic Pi code 
use_eval_play_pattern_track(Arg) ->
    RawTracks = Arg:sample_pattern_track(),
    
    %%extract each track from the list and deal with it 
    lists:map(
        fun({Name, Mode, Duration, Delay, Sample, Pattern, Effect, Envelope}) ->
            makeTrack(Name, Mode, Duration, Delay, Sample, Pattern, Effect, Envelope)
        end, RawTracks).

%%transfer each pattern track 
makeTrack(Name, Mode, Duration, Delay, Sample, Pattern, Effect, Envelope) ->
	if   
		%write iteration times
        is_integer(Mode) andalso Mode > 0 -> 
           write("in_thread name: :" ++ Sample),
		   if 
			   is_number(Delay) ->
				   	write(", delay:" ++ float_to_list(Delay, [{decimals, 3}, compact]));
			   true ->
				    ok
		   end,
		   write(" do; "),
		   write(integer_to_list(Mode) ++ ".times" ++ " do; ");

		%write live_loop
        true ->  
           write("live_loop :" ++ Name),
		   if 
			   is_number(Delay) ->
				   	write(", delay:" ++ float_to_list(Delay, [{decimals, 3}, compact]));
			   true ->
				    ok
		   end,
		   write(" do; ")
    end,
    
     
    %write FX (effect)
    write_effect(Effect),
    
    %%write envelope
    write_envelope(Envelope),
    
    %%write sample and sleep

	if 
		is_integer(Mode) andalso Mode > 0 ->	
			Len = length(Pattern),
			
			if 
				Duration >= Len ->
					
					lists:map(
				       fun(P) ->
				           makePattern(P, Sample)
				       end, Pattern);
				true ->
					Count = splitDuration(0, Duration),
					Difference = Duration - Count,
					NewPattern = lists:sublist(Pattern, Count),
					lists:map(
				       fun(P) ->
				           makePattern(P, Sample)
				       end, NewPattern),
					LastPattern = lists:last(Pattern),
				    writeLastPattern(LastPattern, Sample, Difference)				       					
			end;			
			
		true ->
			    %%write sample and sleep
		    lists:map(
		       fun(P) ->
		           makePattern(P, Sample)
		       end, Pattern)
	 end,

    
    %%end of FX
    write_effect_end(Effect),

    %%write end
	write_end(Mode).


%%return the integer part of the Duration
splitDuration(Count, Duration) ->
	if 
		Duration >= 1 ->
		   NewCount = Count + 1,
		   FinalCount = splitDuration(NewCount, Duration - 1);
	    true ->
		   FinalCount = Count
	end,
	FinalCount.


	

%%write sample name and sleep
%% the beat for sample is always one beat
makePattern(P, Sample) ->
     if 
        P == x ->
            Cmd = "sample :" ++ Sample ++ "; sleep 1; ",
            write(Cmd);
        true ->
            Cmd = "sleep 1; ",
            write(Cmd)
     end.

writeLastPattern(P, Sample, Duration) ->	
	if 
        P == x ->
            Cmd = "sample :" ++ Sample ++ "; sleep "++ float_to_list(Duration, [{decimals, 4}, compact]) ++ "; ",
            write(Cmd);
        true ->
            Cmd = "sleep "++ float_to_list(Duration, [{decimals, 4}, compact]) ++ "; ",
            write(Cmd)
     end.
	
%% transfer choose_play track into Sonic Pi code 
use_eval_play_choose(Arg) ->
    RawChooses = Arg:choose_play(),
    
    %%extract and deal with each choose tuple
    lists:map(
        fun({ChooseName, Mode, Delay, Instrument, Note, Beats}) ->
            makeChoose(ChooseName, Mode, Delay, Instrument, Note, Beats)
        end, RawChooses).

%% transfer each choose tuple into Sonic Pi code 
makeChoose(ChooseName, Mode, Delay, Instrument, Note, Beats) ->
      if   
		%write iteration times
        is_integer(Mode) andalso Mode > 0 -> 
           write("in_thread name: :" ++ ChooseName),
		   if 
			   is_number(Delay) ->
				   	write(", delay:" ++ float_to_list(Delay, [{decimals, 3}, compact]));
			   true ->
				    ok
		   end,
		   write(" do; "),
		   write(integer_to_list(Mode) ++ ".times" ++ " do; ");
		
		%write live_loop
        true ->  
           write("live_loop :" ++ ChooseName),
		   if 
			   is_number(Delay) ->
				   	write(", delay:" ++ float_to_list(Delay, [{decimals, 3}, compact]));
			   true ->
				    ok
		   end,
		   write(" do; ")
    end, 
    
    %%write synth
    write_synth(Instrument),
   
    
    %%write play
    TransferNote = lists:map(
       fun(N) ->
           makeNote(N)
       end, Note),
    WrittenNote = string:join(TransferNote, ","),
    write("play ["++ WrittenNote ++"].choose; "),

    %%write sleep
    TransferBeats = lists:map(
       fun(B) ->
           makeBeats(B)
       end, Beats),
    WrittenBeats = string:join(TransferBeats, ","),
    write("sleep ["++ WrittenBeats ++"].choose; "),
    
   	%%write end
	write_end(Mode).


%%tranfer the note type item in a choose_play tuple 
makeNote(N) ->	
	 if 
        is_float(N) ->
           New_N = float_to_list(N, [{decimals, 4}, compact]);
        true ->
           New_N = ":" ++ atom_to_list(N)
     end,
		 
    New_N.

%%tranfer the beats type item to their corresponding numeric beats
makeBeats(B) ->
    case B of 
         
         '_' -> New_B  = "0.25";
         '__' -> New_B  = "0.5";
         '___' -> New_B  = "1.0";
         '____' -> New_B  = "2.0";
         '_____' -> New_B  = "4.0"
         
    end,
    New_B.



