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
		// TODO: for input_args which are nil, make a named control
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

LLGUI {
	var <>synth, <nLoops;

	var <>debug = false;
	var <window;
	var <loopButtons;
	var <eraseButtons;
	var <autoButton;
	var <oneshotButton;


	*new { |...args| 
		^super.newCopyArgs(*args).init;
	}

	init {
		// synth: synth containing a LivingLooper UGen

		// GUI elements
		window = Window.new(bounds:Rect(200,200,200,50)).front;
		
		// loop buttons start / end recording
		loopButtons = nLoops.collect{ |i| 
			Button().states_([[(i+1).asString],["rec",Color.red]])
		};
		// erase buttons 
		eraseButtons = nLoops.collect{
			Button().states_([["erase"]])
		};

		autoButton = Button().states_([["auto"], ["auto", Color.red]]);
		oneshotButton = CheckBox().string_("one shot");

		// GUI layout
		window.layout_(
			HLayout(
				VLayout(
					HLayout( *eraseButtons ),
					HLayout( *loopButtons ),
				),
				VLayout(
					oneshotButton,
					autoButton,
				),
			)
		);

		// GUI functions

		//
		loopButtons.do{ |b,i| b.mouseDownAction_{ 
			// auto mode off
			(autoButton.value > 0).if{autoButton.valueAction_(0)};

			(b.value==0).if{
				debug.if{["LLGUI: loop", i+1].postln};
				// start recording
				synth!?(_.set(\loop, i+1));
				// any recordings stop
				loopButtons.do{ |b| b.value_(0)};
			}{
				debug.if{["LLGUI: loop end"].postln};
				// end recording
				synth!?(_.set(\loop, 0));
			}
		}};

		eraseButtons.do{ |b,i| b.mouseDownAction_{ 
			// erase loop
			debug.if{["LLGUI: erase", i+1].postln};
			synth!?(_.set(\loop, -1-i));
			// any recordings stop
			loopButtons.do{ |b| b.value_(0)};
		}};

		autoButton.action_{
			(autoButton.value==1).if{
				debug.if{["LLGUI: auto mode", 2].postln};
				synth!?(_.set(\auto, 2))
			}{
				debug.if{["LLGUI: auto off"].postln};
				synth!?(_.set(\auto, 0))
			};
		};

		oneshotButton.action_{
			oneshotButton.value.if{
				debug.if{["LLGUI: oneshot on"].postln};
				synth!?(_.set(\oneshot, 1))
			}{
				debug.if{["LLGUI: oneshot off"].postln};
				synth!?(_.set(\oneshot, 0))
			}
		};

	}
}