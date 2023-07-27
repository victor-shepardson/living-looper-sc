// LivingLooper : UGen {
// 	*new { |filename, input, loopIndex|
// 		var file_args = Array.with(filename.size, *filename.asList.collect(_.ascii));
// 		var input_args = [input, loopIndex];
// 		var inst = this.multiNew('audio', *(file_args++input_args));
// 		filename.isString.not.if{
// 			"ERROR: % first argument should be a String (the RAVE model filename)
// 			note that the filename does *not* support multichannel expansion"
// 			.format(this).postln;
// 		}
// 		^inst
// 	}
// 	checkInputs {
// 		/* TODO */
// 		^this.checkValidInputs;
// 	}
// }

LivingLooper : MultiOutUGen {
	// TODO: is there any way to have a dynamic number of outputs,
	// i.e. not determined until the synth is created?

	// TODO: named arguments

	*new { |filename, nLoops ...input_args|
		var file_args = Array.with(
			filename.size, *filename.asList.collect(_.ascii));
		filename.isString.not.if{
			"ERROR: % first argument should be a String (the RAVE model filename)
			note that the filename does *not* support multichannel expansion"
			.format(this).postln;
		};
		nLoops.isInteger.not.if{
			"ERROR: % second argument should be an Integer (the number of loops)
			note that this does *not* support multichannel expansion"
			.format(this).postln;
		};
		^this.multiNew('audio', nLoops, *(file_args++input_args));
	}
	checkInputs {
		/* TODO */
		^this.checkValidInputs;
	}
	init { arg nLoops ...theInputs;
		inputs = theInputs;
		channels = nLoops.collect{ |i|
			OutputProxy('audio', this, i)
		};
		^ channels
	}
}