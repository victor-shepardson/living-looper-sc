(
// Server.default.options.inDevice_("BlackHole 16ch");
// Server.default.options.outDevice_("BlackHole 16ch");
Server.default.options.inDevice_("MacBook Pro Microph");
// Server.default.options.outDevice_("MacBook Pro Output");
Server.default.options.outDevice_("External Headphones");
// Server.default.options.inDevice_("K-Mix");
// Server.default.options.outDevice_("K-Mix");

Server.scsynth;
// Server.supernova;

s.options.sampleRate = 48000;//44100 // best sound if the sample rate matches RAVE
s.options.blockSize = 64;
s.options.hardwareBufferSize = 64;
// (to avoid dropouts)

"OMP_NUM_THREADS".setenv("1"); // this saves a lot of CPU on arm macs, at least

s.waitForBoot{
	~bus = Bus.new(index:0, numChannels:2);
}
)

s.quit

// if macOS complains about unknown binaries:
("xattr -d -r com.apple.quarantine"+Platform.userExtensionDir.quote++"/LivingLooper/").runInTerminal

(
MIDIIn.connectAll;
// MIDIdef.trace//(false)
MIDIdef.program(\softstep, { arg val, chan, src;
	// val.postln;
	~synth.set(\loop, val)
});

MIDIdef.noteOn(\softstep_on, { arg val, num, chan, src;
	var idx = num - 60;
	var mode = (idx/5).floor;
	idx = idx % 5;
	~synth.set(\loop, idx + 1)
});

MIDIdef.noteOff(\softstep_off, { arg val, num, chan, src;
	~synth.set(\loop, 0)
});

// MIDIdef.trace(false)

~synth = {
	// var in = (SoundIn.ar(0)*2).softclip;
	var in = SoundIn.ar(0);
    var out = //Limiter.ar(
       LivingLooper.new(
		"/Users/victor/RAVE/ll_gtr_latents.ts", 5,
			in,
            \loop.kr(0), // loop index
            \oneshot.kr(0), // loop mode
		);
		// [0]
// );
	// out!2
	// out.poll;
	out = (2*out).softclip;
	out = Splay.ar(out);//+Splay.ar(in);
	out

}.play(outbus:~bus);
)


~synth.set(\oneshot, 0)
~synth.set(\oneshot, 1)

~synth.set(\loop, 4)

~synth.set(\loop, 0)

(
Routine{
	~synth.set(\loop, 4);
	0.1.sleep;
	~synth.set(\loop, 0);
	}.play
)


s.quit

s.record
s.stopRecording
