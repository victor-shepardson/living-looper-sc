(
// Server.default.options.inDevice_("Ultralight AVB");
// Server.default.options.outDevice_("Ultralight AVB");
// Server.default.options.inDevice_("BlackHole 16ch");
// Server.default.options.outDevice_("BlackHole 16ch");
// Server.default.options.inDevice_("MacBook Pro Microphone");
// Server.default.options.outDevice_("MacBook Pro Speakers");
// Server.default.options.outDevice_("External Headphones");
// Server.default.options.inDevice_("UMC404HD 192k");
// Server.default.options.outDevice_("UMC404HD 192k");

Server.scsynth;
// Server.supernova;

s.options.numInputBusChannels = 2;
s.options.numOutputBusChannels = 2;
s.options.sampleRate = 48000;//44100 // best sound if the sample rate matches RAVE
s.options.blockSize = 128;
s.options.hardwareBufferSize = 128;
s.options.numWireBufs = 256;

"OMP_NUM_THREADS".setenv("1"); // this saves a lot of CPU on arm macs, at least

// if you have downloaded the mac version from github:
// ("xattr -d -r com.apple.quarantine"+Platform.userExtensionDir.quote++"/LivingLooper/").runInTerminal;

s.waitForBoot{
	~bus = Bus.new(index:0, numChannels:2);
}
)

(

MIDIIn.connectAll;
// MIDIdef.trace//(false)

MIDIdef.program(\softstep, { arg val, chan, src;
	// val.postln;
	~synth.set(\loop, val)
});

// example MIDI for Softstep 2 controller (note mode):
// top row = erase
// pad 5 = auto
MIDIdef.noteOn(\softstep_on, { arg val, num, chan, src;
	var idx = num-60;
	var row = (idx/5).floor;

	(idx==4).if{
		\auto.postln;
		~synth.set(\auto, 1);
	}{
		idx = idx%5+1;
		(row==1).if{
			idx = idx * (-1);
			[\erase, idx].postln;
			~synth.set(\loop, idx);
		}{
			[\record, idx].postln;
			~synth.set(\loop, idx, \auto, 0);
		}
	}
});

MIDIdef.noteOff(\softstep_off, { arg val, num, chan, src;
	~synth.set(\loop, 0)
});

// make an LLGUI, which infers nLoops and nLatent from the filename
~ll = LLGUI(Platform.userExtensionDir++"/LivingLooper/ll_gtr_l4_z32.ts");

// use .ar on the LLGUI *instance* to make a LivingLooper Ugen
~synth = {
	// var out, z;
	var in = SoundIn.ar(0);
	var out = ~ll.ar(Mix.new(in));
	out.postln;
	out = Splay.ar(out);
	// out = Splay.ar(out)+Splay.ar(in);
	out

}.play(outbus:~bus); //and make an synth

// now associate the LLGUI with the specific synth which it can control
~ll.map(~synth);

// ~ll.debug_(true);
// OSCdef.trace;
)


s.quit

// s.record(bus:0, numChannels:4);
// s.stopRecording
