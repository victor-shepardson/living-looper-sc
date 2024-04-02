LivingLooper {
	*load { |name, filename|

		var sources = (
			vrs_guitar_latest: "http://localhost:8000/ll_gtr48lr_l4_z26.ts"
		);

		filename.isNil.if{
			var url = sources.at(name);
			url.notNil.if{
				var modelDir = PathName(PathName(
					LivingLooper.filenameSymbol.asString
					).parentPath).parentPath +/+ "models";
				var cond = Condition.new;
				filename = modelDir.postln +/+ (name++".ts");
				"downloading % from % to %".format(name, url, filename).postln;
				forkIfNeeded{
					Download(
						url,
						filename,
						finishedFunc: {cond.test=true; cond.signal},
						errorFunc: {Error("download '%' failed".format(url)).throw},
						progressFunc: { |bt, br| "%: % of % bytes".format(name, br, bt).postln; }
					);
					// \waiting.postln;
					cond.wait;
					// \done.postln;
				};
			}{
				"Living Looper model "++name++"not recognized".postln;
				"available models: vrs_guitar_latest";
			};
		};
		"loading % from %".format(name, filename).postln;
		NN.load(name, filename);
		NN(name).describe;
	}

	*ar { |name, input, loop=0, thru=0, auto=0, blockSize=0|
		var out, zs; 
		var method = NN(name, \forward_with_latents);
		var out_zs = method.ar(
			input,
			blockSize,
			debug:1,
			attributes:[
				loop_index: loop, // loop index
				thru: thru, // loop mode
				auto: auto // auto trigger mode
			]);
		out = out_zs.at((0..method.numOutputs/2-1));
		zs = out_zs.at((method.numOutputs/2..method.numOutputs-1));

		^ [out, zs]
	}
}

LLGUI {
	var <name;
	var <l0_sign;

	var <>id;

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

	ar { |input, blockSize=0|
		var out, zs; 
		var mx;
		# out, zs = LivingLooper.ar(
			name, input, 
			loop:\loop.kr(0), thru:\thru.kr(0), auto:\auto.kr(0),
			blockSize:blockSize);
		// zs are packed in audio signals;
		// use zs as its own trigger
		mx = Mix.new(zs.abs);
		// TODO: check node id to support multiple instances
		nLatent.do{ |zi|
			// var trig = DelayN.ar(mx, 0.1, SampleDur.ir*zi);
			// SendReply.ar(trig, "/living_looper_monitor", zs, zi)
			SendReply.ar(mx, "/living_looper_monitor_"++this.id, zs, zi);
			mx = Delay1.ar(mx);
		};
		// zs.scope;
		^ out
	}

	*new { |...args| 
		^super.newCopyArgs(*args).init;
	}

	init {
		nLatent = NN(name, \encode).numOutputs;
		nLoops = NN(name, \forward).numOutputs;
		id = 99999999999.rand;
	}

	//// programmatic access to GUI actions
	erase { |idx| 
		(idx>0).if{
			this.eraseButtons[idx-1].mouseDownAction.defer;
		}{
			("ERROR: LLGUI: can't erase loop" + idx).postln;
		}
	}

	record { |idx| 
		(idx>0).if{
			{
				this.autoButton.valueAction_(0);
				this.loopButtons[idx-1].value_(0);
				this.loopButtons[idx-1].mouseDownAction.();
				this.loopButtons[idx-1].value_(1);
			}.defer;
		}{
			("ERROR: LLGUI: can't record loop" + idx).postln;
		}
	}

	end { |idx|
		(idx>0).if{
			{
				this.autoButton.valueAction_(0);
				this.loopButtons[idx-1].value_(1);
				this.loopButtons[idx-1].mouseDownAction.();
				this.loopButtons[idx-1].value_(0);
			}.defer;
		}{
			("ERROR: LLGUI: can't end loop" + idx).postln;
		}
	}

	auto {
		{this.autoButton.valueAction_(1-this.autoButton.value)}.defer;
	}

	thru {
		{this.thruButton.valueAction_(1-this.thruButton.value)}.defer;
	} 
	//////

	map { |synth|
		// synth: synth containing a LivingLooper UGen
		this.synth = synth;

		// GUI elements
		window = Window.new(bounds:Rect(200,250,1200,300))
			.background_(Color(0.2,0.1,0.2))
			.front;
		
		// loop buttons start / end recording
		loopButtons = nLoops.collect{ |i| 
			Button().states_([
				[(i+1).asString,Color(0.8,0.8,0.8),Color(0.3,0.2,0.3)],
				["rec",Color(1,0.3,0.3),Color(0.2,0.1,0.2)]])
				.toolTip_("start/end recording loop"+(i+1));

		};
		// erase buttons 
		eraseButtons = nLoops.collect{ |i|
			Button().states_([["erase",Color(0.8,0.8,0.8),Color(0.1,0.05,0.1)]])
				.toolTip_("erase loop"+(i+1));
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
					mag = (mag.exp+1).log.sqrt();
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

		autoButton = Button()
			.states_([["auto"], ["auto", Color.red]])
			.toolTip_("automatically record loops in response to input");
		thruButton = Button()
			.states_([["thru"], ["thru", Color.red]])
			.toolTip_("hear processed input while recording a loop");
		// thruButton = CheckBox().string_("thru");

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

		OSCdef(\living_looper_monitor_++this.id, { |msg|
			// [msg[1],this.synth.nodeID].postln;
			// (msg[1]==this.synth.nodeID).if{
			var latent_idx = msg[2];
			var values = msg[3..3+nLoops-1];
			// [latent_idx, values].postln;
			values.do{ |v,loop_idx|
				latents[loop_idx][latent_idx] = v;
			};
			(latent_idx==(nLatent-1)).if{
				{displays.do(_.refresh)}.defer;
			};
			// }
		}, '/living_looper_monitor_'++this.id);

		// GUI functions

		//
		loopButtons.do{ |b,i| b.mouseDownAction_{ 
			// \hello.postln;
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
			displays[i].clearDrawing;
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
			(thruButton.value==1).if{
				debug.if{["LLGUI: thru on"].postln};
				synth!?(_.set(\thru, 1))
			}{
				debug.if{["LLGUI: thru off"].postln};
				synth!?(_.set(\thru, 0))
			}
		};

	}
}