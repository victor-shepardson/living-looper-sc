(
// Server.default.options.inDevice_("BlackHole 16ch");
// Server.default.options.outDevice_("BlackHole 16ch");
Server.default.options.inDevice_("MacBook Pro Microph");
Server.default.options.outDevice_("MacBook Pro Output");
// Server.default.options.inDevice_("K-Mix");
// Server.default.options.outDevice_("K-Mix");

Server.scsynth;

s.options.sampleRate = 44100;//48000; // the sample rate must match RAVE
s.options.blockSize = 64;//512;//16; // a low block size within scsynth is fine,
s.options.hardwareBufferSize = 64;//2048; // but the hardware buffer size needs to match RAVE
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
	~synth.set(\loop, (num - 59))
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
		// "/Users/victor/RAVE/ll_gtr_2loops.ts", 2,
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


~synth.set(\loop, 2)

~synth.set(\loop, 0)


s.quit

s.record
s.stopRecording
