python sonic-pi-tool.py eval "use_bpm 60; in_thread name: :chord1 do; 1.times do; use_synth :beep; use_synth_defaults amplitude: 1.0; in_thread do; play chord(:e3, :minor)[0], release:1.0; sleep 1.0;end;in_thread do; play chord(:e3, :minor)[1], release:2.0; sleep 2.0;end;in_thread do; play chord(:e3, :minor)[2], release:4.0; sleep 4.0;end;end; end; in_thread name: :chord2, delay:4.0 do; 1.times do; use_synth :beep; use_synth_defaults amplitude: 1.0; in_thread do; play chord(:c4, :major)[0], release:1.0; sleep 1.0;end;in_thread do; play chord(:c4, :major)[1], release:2.0; sleep 2.0;end;in_thread do; play chord(:c4, :major)[2], release:4.0; sleep 4.0;end;end; end; "