// this file demonstrates writing your own pre- and post- processing synths,
// and contains an experiment in storing recordings in buffers,
// which can be later re-recorded into living loops

(
s.options.numInputBusChannels = 8;
s.options.numOutputBusChannels = 4;
s.options.hardwareBufferSize = 128;
s.options.sampleRate = 48000;
s.options.blockSize = 128;
// s.options.blockSize = 512;
// s.options.inDevice = "MacBook Pro Microphone";
// s.options.outDevice = "MacBook Pro Speakers";
// s.options.outDevice = "External Headphones";
s.options.inDevice = "K-Mix";
s.options.outDevice = "K-Mix";
s.options.numWireBufs = 512;
s.options.memSize = 65536 * 4;

// NdefMixer(s);

s.waitForBoot{
	~ll = LivingLooper.new();
	// ~ll.load("/Users/victor/living-looper/ll_gtr017norm_l4_z26.ts");
	// ~ll.load("/Users/victor/living-looper/ll_gtr-test_l4_z26.ts");
	// ~ll.load("/Users/victor/living-looper/ll_gtr-test-primes_l4_z26.ts");
	~ll.model_picker.valueAction_(1);

	// n = ~ll.ll.nLoops; // TODO: LL starts asynchronously but nLoops not available until nn~ model is loaded
	n = 4;

	MIDIIn.connectAll;
	// this MIDI mapping is for a SoftStep 2 controller in note mode,
	// and a Living Looper with 4 loops.
	// bottom row buttons 1-4: momentary switch to record each loop
	// top row buttons 6-9: reloop loops 1-4 (play the last recording back in)
	// pad 5 = toggle auto mode
	MIDIdef.noteOn(\softstep_on, { arg val, num, chan, src;
		var idx = num-60;
		var row = (idx/5).floor;

		(idx==4).if{
			// button 5
			\auto.postln;
			~ll.auto;
			// ~synth.set(\auto, 1);
		}{
			var loop = idx%5+1;
			(row==1).if{
				// upper row, buttons 6-9
				[\reloop, loop].postln;
				~reloop.(loop);
			}{
				// lower row, buttons 1-4
				[\record, loop].postln;
				~ll.record(loop);
				// save the recording in a buffer
				~bufrecord.(loop);
				// in case that buffer is playing, stop it
				~bufstop.(loop);
			}
		}
	});

	MIDIdef.noteOff(\softstep_off, { arg val, num, chan, src;
		// ~synth.set(\loop, 0)
		var idx = num-60;
		// lower row, buttons 1-4
		(idx<4).if{
			var loop = idx%5+1;
			[\end, loop].postln;
			~ll.end(loop);
			~bufend.(loop);
		}
	});

	// capture output to a bus
	~bus = Bus.audio(s, 4);
	~ll.setOutBus(~bus);

	Ndef(\pre, {
		var mix, verb;
		var dry = SoundIn.ar(0);
		dry
	});

	~bufs = n.collect{Buffer.alloc(s, s.options.sampleRate*10, 1)};
	s.sync;

	n.do{ |i|
		var rname = (\bufrecord_++(i+1)).asSymbol;
		var pname = (\bufplay_++(i+1)).asSymbol;
		Ndef(rname, {
			var in = \in.ar(0);
			var go = \go.kr(0);
			var phase, rate, end;
			phase = Phasor.ar(
				go, go, 0, BufFrames.kr(~bufs[i]), 0);
			end = Latch.kr(phase, 1-go);
				BufWr.ar(in*go, ~bufs[i], phase);
			end; //output of this Ndef is the loop end point (start is 0)
		});

		Ndef(pname, {
			var go = \go.kr(0);
			var end = Ndef(rname).kr;
			// go*LoopBuf.ar(1, ~bufs[i], rate:go,
			// gate:go, startLoop:0, endLoop:end, interpolation:4);
			//rate:0 here doesn't cause silent output because fuck you
			var time = Sweep.ar(go, SampleRate.ir);
			var env = ((end - time)*1e-2).clip(0,1);
			var out = env*PlayBuf.ar(1, ~bufs[i], rate:1, trigger:go);
			out
		});

		Ndef(rname).set(\in, Ndef(\pre));
		// Ndef(pname).set(
			// \end, Ndef((\bufrecord_++i).asSymbol));
	};

	~ll.setInBus(Ndef((\pre).asSymbol).bus.index);

	~bufrecordt = 0!n;
	~buflen = 0!n;
	~bufrecord = { |i|
		~bufrecordt[i-1] = Date.localtime.rawSeconds;
		Ndef((\bufrecord_++i).asSymbol).set(\go, 1)};
	~bufend = { |i|
		~buflen[i-1] = Date.localtime.rawSeconds - ~bufrecordt[i-1];
		~buflen.postln;
		Ndef((\bufrecord_++i).asSymbol).set(\go, 0)};
	~bufplay = { |i| Ndef((\bufplay_++i).asSymbol).set(\go, 1)};
	~bufstop = { |i| Ndef((\bufplay_++i).asSymbol).set(\go, 0)};
	// re-record the last recording made for this loop back into the loop
	~reloop = { |loop|
		// `loop` is 1-indexed
		// record buffer into loop
		~bufplay.(loop);
		~ll.record(loop);
		AppClock.play(Routine{
			// set loop input
			~ll.setInBus(Ndef((\bufplay_++loop).asSymbol).bus.index);
			// automatically stop loop record
			~buflen[loop-1].wait;
			~ll.end(loop);
			// ~ll.setInBus(nil);
			~ll.setInBus(Ndef((\pre).asSymbol).bus.index);
			~bufstop.(loop);
		});
	};

	// post-process the output in your own synth
	Ndef(\post, {
		var mix, verb;
		var dry = SoundIn.ar(0); // NOTE not tied to looper input channel
		var loops = In.ar(~bus, 4) * \amp.kr(1!4);
		// mix = Splay.ar(loops);
		var loops_del = DelayC.ar(loops, 1,
			\haasms.kr(30e-3)*[1.0, 0.7, 0.8, 0.9]);
		loops_del = HPF.ar(BLowPass.ar(
			loops_del, 3000), 100)*\haasgain.kr(0.15);
		verb = JPverb.ar(Mix(loops++[dry]),
			t60:0.5, damp:\verbdamp.kr(0.8), size:1.5, modDepth:0.05);
		mix = Mix(
			Pan2.ar(loops, [-1, -1/2, 1/2, 1])
			++ Pan2.ar(loops_del, [1/3, 2/3, -2/3, -1/3])
			++ [verb*\verbgain.kr(0.02)]
		);
		Limiter.ar(mix++[dry]++loops, \limit.kr(-3).dbamp);
	}).play;

})

// set to thru mode so you can hear the re-looping
~ll.thru

// start recording loop 1
(
~ll.record(1);
~bufrecord.(1);
)

// end recording loop 1
(
~ll.end(1);
~bufend.(1);
)

// re-create loop 1 from the last recording
~reloop.(1);


r = Recorder(s).record(bus:Ndef(\post).bus, numChannels:7)
r.stopRecording