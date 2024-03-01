//// run this line the first time after installing on macOS:
("/usr/bin/xattr -d -r com.apple.quarantine"+Platform.userExtensionDir.quote++"/LivingLooper/").runInTerminal
////

//// server setup
(
// Server.default.options.inDevice_("UMC404HD 192k");
// Server.default.options.outDevice_("UMC404HD 192k");

s.options.numInputBusChannels = 2;
s.options.numOutputBusChannels = 6; // stereo out + quad out for recording

s.options.sampleRate = 48000; // best sound if the sample rate matches RAVE
s.options.blockSize = 64;
s.options.hardwareBufferSize = 64;
s.options.numWireBufs = 256;

// this environment variable affects pytorch performance
"OMP_NUM_THREADS".setenv("1"); // (best setting for apple silicon)

s.waitForBoot{
	~bus = Bus.new(index:0, numChannels:6);
})
////

//// run the living looper
(
MIDIIn.connectAll;

// this MIDI mapping is for a SoftStep 2 controller in note mode,
// and a Living Looper with 4 loops.

// bottom row buttons 1-4: momentary switch to record each loop
// top row buttons 6-9: erase loops 1-4
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
			[\erase, loop].postln;
			~ll.erase(loop);
			// ~synth.set(\loop, -loop)
		}{
			// lower row, buttons 1-4
			[\record, loop].postln;
			~ll.record(loop);
			// ~synth.set(\loop, loop, \auto, 0);
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
	}
});

// make an LLGUI, which infers nLoops and nLatent from the filename
// choose the default 4-loop guitar model
~ll = LLGUI(Platform.userExtensionDir++"/LivingLooper/ll_gtr48lr_l4_z26.ts", 1);

// use .ar on the LLGUI *instance* to make a LivingLooper Ugen
~synth = {
	var loops, mix;
	var in = SoundIn.ar(0);
	loops = ~ll.ar(Mix.new(in));
	mix = Splay.ar(loops);
	Limiter.ar(mix++loops, 0.2);
}.play(outbus:~bus); //and make a synth

// now associate the LLGUI with the specific synth which it can control
~ll.map(~synth);

// print stuff when buttons are pressed
~ll.debug_(true);
)
////

s.record(bus:0, numChannels:8); //record main input, aux input, stereo mix, 4 loops

s.stopRecording

s.quit
