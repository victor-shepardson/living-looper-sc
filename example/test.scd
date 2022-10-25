(
// Server.default.options.inDevice_("BlackHole 16ch");
// Server.default.options.outDevice_("BlackHole 16ch");
Server.default.options.inDevice_("Built-in Microph");
Server.default.options.outDevice_("Built-in Output");

// Server.supernova;
Server.scsynth;

s.options.sampleRate = 48000; // the sample rate must match RAVE
s.options.blockSize = 512;//16; // a low block size within scsynth is fine,
s.options.hardwareBufferSize = 2048; // but the hardware buffer size needs to match RAVE
// (to avoid dropouts)

"OMP_NUM_THREADS".setenv("1"); // jfc

s.waitForBoot{
	~bus = Bus.new(index:0, numChannels:2);
}
)

s.quit

// if macOS complains about unknown binaries:
("xattr -d -r com.apple.quarantine"+Platform.userExtensionDir.quote++"/LivingLooper/").runInTerminal

(
~synth = {

    var out = Limiter.ar(
        LivingLooper.new(
			"/Users/victor/RAVE/ll_test.ts", 3,
            SoundIn.ar(0), // input
            \loop.kr(0), // loop inddex
		)
		// [0]
	);
	// out!2
	Splay.ar(out);

}.play(outbus:~bus);
)


~synth.set(\loop, 1)

~synth.set(\loop, 0)


s.quit

s.record
s.stopRecording
