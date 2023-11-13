LLInfo {
	var <filename;
	var <nLoops, <nLatent, <file_args;

	*new { |...args| 
		^super.newCopyArgs(*args).init;
	}

	init {
		var parts;
		file_args = Array.with(
			filename.size, *filename.asList.collect(_.ascii));

		// split filename for nLoops nLatent
		parts = filename.split($.).wrapAt(-2).split($_);
		nLoops = parts.wrapAt(-2)[1..].asInteger;
		nLatent = parts.wrapAt(-1)[1..].asInteger;

		filename.isString.not.if{
			"ERROR: % first argument should be a String (the torchscript filename)
			note that the filename does *not* support multichannel expansion"
			.format(this).postln;
		};
	}
}


LivingLooper : MultiOutUGen {
	var <>nLoops, <>nLatent;

	*new { |filename ...input_args|
		var info = LLInfo(filename);
		var file_args = info.file_args;
		var nLatent = info.nLatent;
		var nLoops = info.nLoops;

		nLoops.isInteger.not.if{
			"ERROR: % second argument should be an Integer (the number of loops)
			note that this does *not* support multichannel expansion"
			.format(this).postln;
		};
		^this.multiNew('audio', nLoops, nLatent, *(file_args++input_args));
	}
	checkInputs {
		/* TODO */
		^this.checkValidInputs;
	}
	init { arg nLoops, nLatent ...theInputs;
		var audio, latents;
		this.nLoops = nLoops; this.nLatent = nLatent;
		inputs = theInputs;
		audio = nLoops.collect{ |i|
			OutputProxy('audio', this, i)
		};
		// pack latents into audio buffers
		// assumes that block size >= nLatent
		latents = nLoops.collect{ |l|
			OutputProxy('audio', this, (nLoops + l))
		};
		// assigning channels apparently is important to MultiOutUGen:
		channels = audio ++ latents;
		^ [audio, latents]
	}
}


LLGUI {
	var <filename;
	var <l0_sign;

	var <>synth;//, <>ugen;
	var <>nLoops, <>nLatent;

	var <>debug = false;
	var <window;
	var <loopButtons;
	var <eraseButtons;
	var <autoButton;
	var <thruButton;
	var <displays;
	var <latents;

	ar { |input|
		var out, zs; 
		var mx;
		# out, zs = LivingLooper(
			filename,
			input,
			\loop.kr(0), // loop index
			\thru.kr(0), // loop mode
			\auto.kr(0) // auto trigger mode
			);

		// zs are packed in audio signals;
		// use zs as its own trigger
		mx = Mix.new(zs.abs);
		// TODO: check node id to support multiple instances
		nLatent.do{ |zi|
			// var trig = DelayN.ar(mx, 0.1, SampleDur.ir*zi);
			// SendReply.ar(trig, "/living_looper_monitor", zs, zi)
			SendReply.ar(mx, "/living_looper_monitor", zs, zi);
			mx = Delay1.ar(mx);
		};
		// zs.scope;
		^ out
	}

	*new { |...args| 
		^super.newCopyArgs(*args).init;
	}

	init {
		var info = LLInfo(filename);
		nLatent = info.nLatent;
		nLoops = info.nLoops;
	}

	map { |synth|
		// synth: synth containing a LivingLooper UGen
		this.synth = synth;

		// GUI elements
		window = Window.new(bounds:Rect(200,250,1000,300))
			.background_(Color(0.2,0.1,0.2))
			.front;
		
		// loop buttons start / end recording
		loopButtons = nLoops.collect{ |i| 
			Button().states_([
				[(i+1).asString,Color(0.8,0.8,0.8),Color(0.3,0.2,0.3)],
				["rec",Color(1,0.3,0.3),Color(0.2,0.1,0.2)]])
		};
		// erase buttons 
		eraseButtons = nLoops.collect{
			Button().states_([["erase",Color(0.8,0.8,0.8),Color(0.1,0.05,0.1)]])
		};

		// TODO: possibly draw all loops into one userview so they can overlap?
		displays = nLoops.collect{ |loop_idx|
			UserView()
				// .background_(Color.rand)
				.background_(Color.black)
				// .animate_(true)
				.resize_(5)
				.clearOnRefresh_(false)
				// // bars
				// .drawFunc_({ |view|
				// 	var bounds = view.bounds.width@view.bounds.height;
				// 	var num = latents[loop_idx].size;
				// 	// Pen.color = Color(0,0,0,0.3);
				// 	Pen.color = Color(0.2,0.1,0.2,0.25);
				// 	Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
				// 	latents[loop_idx].do{ |item,i|
				// 		var v = i+1 / (num+2);
				// 		Pen.color = Color(
				// 			(i*2pi/3).sin+1/2,
				// 			(i*2pi/4).cos+1/2,
				// 			(i*2pi/5).sin+1/2);
				// 		Pen.moveTo((0.5@v)*bounds);
				// 		Pen.lineTo(((item/8 + 0.5)@v)*bounds);
				// 		Pen.width = 3;
				// 		Pen.stroke;
				// 	}
				// })
				// // pendulum
				.drawFunc_({ |view|
					var bounds = view.bounds.width@view.bounds.height;
					var pt = 0@0;
					// TODO: export should rectify latents,
					// polarity of first latent is hardcoded here
					// var mag = 0 - latents[loop_idx][0];
					var mag = latents[loop_idx][0] * (l0_sign?1);
					// var mag = latents[loop_idx].squared.sum.sqrt/3-2;
					var ndrop = (latents[loop_idx].size - 1) % 4;
					mag = (mag.exp+1).log.sqrt()/2;
					Pen.color = Color(0,0,0,0.3);
					Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
					latents[loop_idx].drop(1).drop(0-ndrop).clump(4).do{ |item,i|
						var new_pt;
						Pen.color = Color(
							(item[2]).sin+1/2,
							(i/8*6).cos+1/2,
							(item[3]).sin+1/2);
						Pen.moveTo(pt+1/2*bounds);
						new_pt = Point(item[0],item[1])/3/(i/3+1)*mag + pt;
						new_pt = new_pt / (new_pt.abs+1);
						Pen.lineTo(new_pt+1/2*bounds);
						pt = new_pt;
						Pen.width = bounds.x/10 * mag/(i/3+1);
						Pen.stroke;
					}
				})
				// // fixed colors
				// .drawFunc_({ |view| 
				// 	var bounds = view.bounds.width@view.bounds.height;
				// 	var pt = 0@0;
				// 	// TODO: export should rectify latents,
				// 	// polarity of first latent is hardcoded here
				// 	// var mag = 0 - latents[loop_idx][0];
				// 	var mag = latents[loop_idx][0] * (l0_sign?1);
				// 	// var mag = latents[loop_idx].squared.sum.sqrt/3-2;
				// 	var ndrop = (latents[loop_idx].size - 1) % 4;
				// 	mag = (mag.exp+1).log.sqrt()/2;
				// 	Pen.color = Color(0,0,0,0.3);
				// 	Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
				// 	latents[loop_idx].drop(1).drop(0-ndrop).clump(2).do{ |item,i|
				// 		var new_pt;
				// 		Pen.color = Color(
				// 			(i*2pi/3).cos+1/2,
				// 			(i*2pi/4).cos+1/2,
				// 			(i*2pi/5).cos+1/2);
				// 		Pen.moveTo(pt+1/2*bounds);
				// 		new_pt = Point(item[0],item[1])/3/(i/3+1)*mag + pt;
				// 		new_pt = new_pt / (new_pt.abs+1);
				// 		Pen.lineTo(new_pt+1/2*bounds);
				// 		pt = new_pt;
				// 		Pen.width = bounds.x/10 * mag/(i/3+1);
				// 		Pen.stroke;
				// 	}
				// })
				// // blocks
				// .drawFunc_({ |view|
				// 	var bounds = view.bounds.width@view.bounds.height;
				// 	var pt = 0@0;
				// 	// TODO: export should rectify latents,
				// 	// polarity of first latent is hardcoded here
				// 	// var mag = 0 - latents[loop_idx][0];
				// 	var mag = latents[loop_idx][0] * (l0_sign?1);
				// 	// var mag = latents[loop_idx].squared.sum.sqrt/3-2;
				// 	var ndrop = (latents[loop_idx].size - 1) % 4;
				// 	mag = (mag.exp+1).log.sqrt()/2;
				// 	Pen.color = Color(0,0,0,0.3);
				// 	Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
				// 	latents[loop_idx].drop(1).drop(0-ndrop).clump(2).do{ |item,i|
				// 		var start, end, len;
				// 		Pen.color = Color(
				// 			(i*2pi/3).cos+1/2,
				// 			(i*2pi/4).cos+1/2,
				// 			(i*2pi/5).cos+1/2);
				// 		len = 1/(i+3);
				// 		start = Point(item[0],item[1])/3*mag;
				// 		end = start + (len@0);
				// 		start = start - (len@0);
				// 		Pen.moveTo(start+1/2*bounds);
				// 		Pen.lineTo(end+1/2*bounds);
				// 		Pen.width = bounds.x/10 * mag/(i/3+1);
				// 		Pen.stroke;
				// 	}
				// })
				;
		};

		autoButton = Button().states_([["auto"], ["auto", Color.red]]);
		thruButton = CheckBox().string_("thru");

		// GUI layout
		window.layout_(
			HLayout(
				VLayout(
					thruButton,
					autoButton,
				),
				*nLoops.collect{ |i| [VLayout(
						displays[i],
						eraseButtons[i],
						loopButtons[i]
				), stretch:1]},
			)
		);

		latents = (0!nLatent)!nLoops;

		OSCdef(\living_looper_monitor, { |msg|
			var latent_idx = msg[2];
			var values = msg[3..3+nLoops-1];
			// msg.postln;
			// [latent_idx, values].postln;
			values.do{ |v,loop_idx|
				latents[loop_idx][latent_idx] = v;
			};
			(latent_idx==(nLatent-1)).if{
				{displays.do(_.refresh)}.defer;
			};

		}, '/living_looper_monitor');

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

		thruButton.action_{
			thruButton.value.if{
				debug.if{["LLGUI: thru on"].postln};
				synth!?(_.set(\thru, 1))
			}{
				debug.if{["LLGUI: thru off"].postln};
				synth!?(_.set(\thru, 0))
			}
		};

	}
}