LLTheme {
	var <color_bg, <color_fg, <color_dark, <color_text, <color_alert, <color_highlight;
	*new { |...args|
		^super.newCopyArgs(*args).init;
	}

	init {
		color_bg = color_bg ? Color(0.2,0.1,0.2);
		color_fg = color_fg ? Color(0.3,0.2,0.3);
		color_dark = color_dark ? Color(0.1,0.05,0.1);
		color_text = color_text ? Color(0.8,0.8,0.8);
		color_alert = color_alert ? Color(1,0.3,0.3);
		color_highlight = color_highlight ? Color(0.5,0.5,0.9);
	}
}

LLServerControl {
	var <>server;
	var <on_boot;
	var <theme;
	var <boot_button, <rate_box, <hblock_box, <cblock_box, <indevice_drop, <outdevice_drop;

	*new { |...args|
		^super.newCopyArgs(*args).init;
	}

	init {
		server = server ? Server.default;
		theme = theme ? LLTheme.new;
		boot_button = Button.new
		.states_([
			["boot server",theme.color_text,theme.color_fg],
			["quit server",theme.color_alert,theme.color_bg]])
		.value_(server.serverRunning.asInteger)
		.toolTip_("boot server")
		.action_{
			(boot_button.value==1).if{
				server.options.sampleRate = rate_box.value.postln;
				server.options.hardwareBufferSize = hblock_box.value.postln;
				server.options.blockSize = cblock_box.value.postln;
				server.options.inDevice = indevice_drop.item.postln;
				server.options.outDevice = outdevice_drop.item.postln;
				server.waitForBoot{
					on_boot.value
				};
			}{
				server.quit;
			}
		};

		rate_box = NumberBox()
		.valueAction_(48000)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.normalColor_(theme.color_text)
		.typingColor_(theme.color_alert)
		.toolTip_("set sampling rate (requires server reboot)");


		hblock_box = NumberBox()
		.valueAction_(128)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.normalColor_(theme.color_text)
		.typingColor_(theme.color_alert)
		.toolTip_("set hardware block size (requires server reboot)");

		cblock_box = NumberBox()
		.valueAction_(128)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.normalColor_(theme.color_text)
		.typingColor_(theme.color_alert)
		.toolTip_("set supercollider control block size (requires server reboot)");

		indevice_drop = PopUpMenu()
		.items_(ServerOptions.inDevices)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.toolTip_("set input device (requires server reboot)")
		;

		outdevice_drop = PopUpMenu()
		.items_(ServerOptions.outDevices)
		.background_(theme.color_bg)
		.stringColor_(theme.color_text)
		.toolTip_("set output device (requires server reboot)")
		;

		^this
	}

	label { |item, text|
		^VLayout(
			StaticText().string_(text).stringColor_(theme.color_text),
			item
		)
	}

	gui {
		^VLayout(
			HLayout(
				this.label(indevice_drop, "input device"),
				this.label(outdevice_drop, "output device")
			),
			HLayout(
				this.label(rate_box, "sampling rate"),
				this.label(hblock_box, "hardware block"),
				this.label(cblock_box, "control block"),
			),
			boot_button,
		)
	}
}

// TODO excessive this
LLMIDIMapper {
	var <theme;
	var <>toggle; //Button to enable MIDI mapping
	var <>save_button;
	var <>load_button; //Buttons to open file dialog
	var <>buttons; //Dictionary of name -> Button
	var <>map; //Dictionary of MIDI info -> name
	var <>target; //current target of MIDI mapping

	*new { |...args|
		^super.newCopyArgs(*args).init;
	}

	gui {
		var label = StaticText()
		.string_("MIDI map:").stringColor_(theme.color_text);
		^HLayout(label, this.toggle, this.save_button, this.load_button)
	}

	button { |name, parent, bounds|
		^LLMIDIButton(this, name, parent, bounds);
	}

	get_text { |key, text|
		var miditext = "%:% ch:% port:%".format(*key);
		^ text ++ " (%)".format(miditext);
	}

	set_states {
		// reset all
		this.buttons.do{ |button| 
			var extra_text = (this.toggle.value==1).if{" (...)"}{""};
			button.states_(button.initial_states.collect{ |state| 
				[state[0]++extra_text] ++ state[1..]
			}, ephemeral:true)
		};

		// then set all in map
		this.map.pairsDo{ |key, name|
			var button = this.buttons.at(name);
			button.states_(button.initial_states.collect{ |state| 
				[this.get_text(key, state[0])] ++ state[1..]
			}, ephemeral:true)
		}
	}

	add { |key, target|
		var button, miditext;
		// map MIDI to name
		this.map.add(key -> target);
		this.map.postln;
		// modify button text
		this.set_states;
	}

	match { |key|
		var name = this.map.at(key);
		^this.buttons.at(name);
	}

	init {
		theme = theme ? LLTheme.new;
		this.buttons = Dictionary.new;
		this.map = Dictionary.new;

		// global MIDI map toggle -- visually changes all MIDIButtons
		this.toggle = Button()
		.states_([
			["start", theme.color_text, theme.color_fg],
			["done", theme.color_text, theme.color_highlight]])
		.action_{
			// update button appearances
			this.set_states;
		};

		this.save_button = Button()
		.states_([["save", theme.color_text, theme.color_fg]])
		.action_{
			Dialog.savePanel({ |path|
				this.map.writeArchive(path);
			})
		};

		this.load_button = Button()
		.states_([["load", theme.color_text, theme.color_fg]])
		.action_{
			Dialog.openPanel({ |path|
				"loading...".postln;
				this.map = Object.readArchive(path).postln;
				this.set_states;
			})
		};

		// TODO: handlers for other msgTypes
		MIDIdef(\LLMIDIMapperOn, { |val, num, chan, src|
			var key = [\note, num, chan, src];
			// noteOn can trigger MIDI mapping
			// noteOn sets button to state 1
			{
				(this.toggle.value==1).if{
					// when mapping is toggled on, a MIDI handler associates 
					// incoming messages type/value/chan/src with the target button
					this.add(key, this.target);
				}{
					// when toggled off, a MIDI handler dispatches mapped message type/value/src to buttons
					var button = this.match(key);
					button.notNil.if{
						button.valueAction_(1);
					}
				}
			}.defer;
		}, msgType:\noteOn).permanent_(true);

		MIDIdef(\LLMIDIMapperOff, { |val, num, chan, src|
			var key = [\note, num, chan, src];
			// noteOff sets button to state 0
			{
				var button = this.match(key);
				button.notNil.if{
					button.valueAction_(0);
				}
			}.defer;
		}, msgType:\noteOff).permanent_(true);

		MIDIdef(\LLMIDIMapperControl, { |val, num, chan, src|
			var key = [\control, num, chan, src];
			// controlChange can trigger MIDI mapping
			// controlChange toggles button 
			{
				(this.toggle.value==1).if{
					// when mapping is toggled on, a MIDI handler associates 
					// incoming messages type/value/chan/src with the target button
					this.add(key, this.target);
				}{
					// when toggled off, a MIDI handler dispatches mapped message type/value/src to buttons
					var button = this.match(key);
					button.notNil.if{
						button.valueAction_(button.value + 1 % button.states.size);
					}
				}
			}.defer;
		}, msgType:\control).permanent_(true);

		MIDIdef(\LLMIDIMapperProgram, { |num, chan, src|
			var key = [\program, num, chan, src];
			// controlChange can trigger MIDI mapping
			// controlChange toggles button 
			{
				(this.toggle.value==1).if{
					// when mapping is toggled on, a MIDI handler associates 
					// incoming messages type/value/chan/src with the target button
					this.add(key, this.target);
				}{
					// when toggled off, a MIDI handler dispatches mapped message type/value/src to buttons
					var button = this.match(key);
					button.notNil.if{
						button.valueAction_(button.value + 1 % button.states.size);
					}
				}
			}.defer;
		}, msgType:\program).permanent_(true);
	}


}

LLMIDIButton : Button {
	var <>mapper; 
	var <>name;
	var <>initial_states;

	*new { |mapper, name, parent, bounds| 
		var inst = super.new(parent, bounds).mapper_(mapper).name_(name);
		// [parent, bounds, mapper, name].postln;
		mapper.buttons.add(name -> inst);
		^inst
	}

	states_{ |states, ephemeral=false| 
		ephemeral.not.if{this.initial_states = states};
		^super.states_(states)
	}

	action_{ |fn|
		^super.action_{ |...args|
			// when toggled on, buttons are disabled, 
			// and clicking a button registers it as the target of MIDI mapping
			(this.mapper.toggle.value==1).if{
				// undo the press
				this.value = this.value - 1 % this.states.size;
				this.mapper.target = this.name;
				"mapping '%'...".format(this.name).postln;
			}{
				fn.(*args)
			}
		}
	}

}

LivingLooper {
	*load { |name, source, forceDownload=false|
		var filename;

		// var sources = (
			// vrs_guitar_latest: "http://localhost:8000/ll_gtr48lr_l4_z26.ts"
		// );

		var sources = thisProcess.interpreter.executeFile(PathName(
				LivingLooper.filenameSymbol.asString
				).parentPath +/+ "sources.scd");

		var url = sources.at(source);
		url.notNil.if{
			// get model by name from remote source
			var modelDir = PathName(PathName(
				LivingLooper.filenameSymbol.asString
				).parentPath).parentPath +/+ "models";
			var cond = Condition.new;
			// filename = modelDir.postln +/+ (name++".ts");
			filename = modelDir +/+ PathName(url).fileName;
			(forceDownload || File.exists(filename).not).if{
				"downloading % from % to %".format(name, url, filename).postln;
				forkIfNeeded{
					Download(
						url,
						filename,
						finishedFunc: {cond.test=true; cond.signal},
						errorFunc: {Error("download '%' failed".format(url)).throw},
						progressFunc: { |bt, br| "%: % of % bytes".format(name, br, bt).postln; }
					);
					cond.wait;
				}
			}
		}{
			// assume source is a local filename
			File.exists(source).if{
				filename = source;
			}{
				Error(
					"Living Looper model '"++source++"' not recognized\n"
					++"check your file path or try one of these "
					++"available models: %".format(sources.keys)
					).throw;
			}
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

LLPendulum {
	*draw { |view, latents, l0_sign|
		var bounds = view.bounds.width@view.bounds.height;
		var pt = 0@0;
		var mag = latents[0] * (l0_sign?1);
		var ndrop = (latents.size - 1) % 4;
		mag = (mag.exp+1).log.sqrt();
		Pen.color = Color(0,0,0,0.3);
		Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
		latents.drop(1).drop(0-ndrop).clump(4).do{ |item,i|
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
	}
}

LLBars {
	*draw { |view, latents, l0_sign| 
		var bounds = view.bounds.width@view.bounds.height;
		var num = latents.size;
		Pen.color = Color(0.2,0.1,0.2,0.25);
		Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
		latents.do{ |item,i|
			var v = i+1 / (num+2);
			Pen.color = Color(
				(i*2pi/3).sin+1/2,
				(i*2pi/4).cos+1/2,
				(i*2pi/5).sin+1/2);
			Pen.moveTo((0.5@v)*bounds);
			Pen.lineTo(((item/8 + 0.5)@v)*bounds);
			Pen.width = 3;
			Pen.stroke;
		}
	}
}

// // fixed colors
// LLPendulum2 {
// *draw {
// 	var bounds = view.bounds.width@view.bounds.height;
// 	var pt = 0@0;
// 	// TODO: export should rectify latents,
// 	// polarity of first latent is hardcoded here
// 	// var mag = 0 - latents[0];
// 	var mag = latents[0] * (l0_sign?1);
// 	// var mag = latents.squared.sum.sqrt/3-2;
// 	var ndrop = (latents.size - 1) % 4;
// 	mag = (mag.exp+1).log.sqrt()/2;
// 	Pen.color = Color(0,0,0,0.3);
// 	Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
// 	latents.drop(1).drop(0-ndrop).clump(2).do{ |item,i|
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
// }}
// // blocks
// LLBlocks {
// *draw {
// 	var bounds = view.bounds.width@view.bounds.height;
// 	var pt = 0@0;
// 	// TODO: export should rectify latents,
// 	// polarity of first latent is hardcoded here
// 	// var mag = 0 - latents[0];
// 	var mag = latents[0] * (l0_sign?1);
// 	// var mag = latents.squared.sum.sqrt/3-2;
// 	var ndrop = (latents.size - 1) % 4;
// 	mag = (mag.exp+1).log.sqrt()/2;
// 	Pen.color = Color(0,0,0,0.3);
// 	Pen.fillRect(Rect(0,0,view.bounds.width,view.bounds.height));
// 	latents.drop(1).drop(0-ndrop).clump(2).do{ |item,i|
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
// }}

LLGUI {
	var <name;
	var <l0_sign;
	var <theme;

	var <>id;

	var <>synth;
	var <>nLoops, <>nLatent;

	var <>debug = false;
	var <window;
	var <loopButtons;
	var <eraseButtons;
	var <autoButton;
	var <thruButton;
	var <displays;
	var <latents;
	var <mapper;

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
		theme = theme ? LLTheme.new;
	}

	//// programmatic access to GUI actions
	erase { |idx| 
		(idx>0).if{
			this.eraseButtons[idx-1].action.defer;
		}{
			("ERROR: LLGUI: can't erase loop" + idx).postln;
		}
	}

	record { |idx| 
		(idx>0).if{
			{
				this.autoButton.valueAction_(0);
				this.loopButtons[idx-1].value_(0);
				this.loopButtons[idx-1].action.();
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
				this.loopButtons[idx-1].action.();
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
		this.make_window.layout_(this.gui);
		^ this
	}

	make_window {
		// GUI elements
		window = Window.new(bounds:Rect(200,250,1200,300))
			.background_(theme.color_bg)
			.front;
		^ window;
	}

	gui {
		mapper = LLMIDIMapper.new;

		// loop buttons start / end recording
		loopButtons = nLoops.collect{ |i| 
			mapper.button(\loop++i.asSymbol).states_([
				["record "++(i+1).asString, theme.color_text, theme.color_fg],
				["play "++(i+1), theme.color_alert, theme.color_bg]])
				.toolTip_("start/end recording loop"+(i+1));

		};
		// erase buttons 
		eraseButtons = nLoops.collect{ |i|
			mapper.button(\erase++i.asSymbol).states_([
				["erase "++i.asString,theme.color_text,theme.color_dark]])
				.toolTip_("erase loop"+(i+1));
		};

		// TODO: possibly draw all loops into one userview so they can overlap?
		displays = nLoops.collect{ |loop_idx|
			UserView.new
			// .background_(Color.rand)
			.background_(Color.black)
			// .animate_(true)
			.resize_(5)
			.clearOnRefresh_(false)
			// .drawFunc_(LLBars.draw(_, latents[loop_idx], l0_sign))
			.drawFunc_(LLPendulum.draw(_, latents[loop_idx], l0_sign))
			;
		};

		autoButton = mapper.button(\auto)
			.states_([
				["auto", theme.color_text, theme.color_fg],
				["auto", theme.color_alert, theme.color_bg]])
			.toolTip_("automatically record loops in response to input");
		thruButton = mapper.button(\thru)
			.states_([
				["thru", theme.color_text, theme.color_fg], 
				["thru", theme.color_alert, theme.color_bg]])
			.toolTip_("hear processed input while recording a loop");
		// thruButton = CheckBox().string_("thru");

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
		loopButtons.do{ |b,i| b.action_{ 
			// \hello.postln;
			// auto mode off
			(autoButton.value > 0).if{autoButton.valueAction_(0)};

			(b.value==1).if{
				debug.if{["LLGUI: loop", i+1].postln};
				// start recording
				synth!?(_.set(\loop, i+1));
				// any other recording stops internally -- 
				// make buttons reflect this
				loopButtons.do{ |b| b.value_(0)};
				b.value_(1);
			}{
				debug.if{["LLGUI: loop end"].postln};
				// end recording
				synth!?(_.set(\loop, 0));
			}
		}};

		eraseButtons.do{ |b,i| b.action_{ 
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

		// GUI layout
		^ VLayout(
			HLayout(
				thruButton,
				autoButton,
				mapper.gui
			),
			HLayout(
				
				*nLoops.collect{ |i| [VLayout(
						displays[i],
						eraseButtons[i],
						loopButtons[i]
				), stretch:1]},
			),
		);

	}
}

// LLGUI plus a server panel and default synth
// TODO: model picker
// TODO: channel routing options
LLStandalone {
	var window;
	var server_control;

	*new { |...args|
		^super.newCopyArgs(*args).init;
	}

	init {
		server_control = LLServerControl.new(Server.default, {
			// runs when server booted
			var ll, synth;
			LivingLooper.load(\test, \vrs_guitar_latest);
			ll = LLGUI(\test);
			ll.synth = {
				var in = SoundIn.ar(0);
				var out = ll.ar(in, blockSize:2048);
				Splay.ar(out);
			}.play;
			window.layout.add(ll.gui, stretch:1);
			window.setInnerExtent(1200, 500);
		});

		window = Window.new(bounds:Rect(200,250,1200,150))
		.background_(Color(0.2,0.1,0.2))
		.layout_(VLayout(
			[HLayout(
				[StaticText()
					.string_("Living Looper v1.0.0b")
					.stringColor_(Color(0.8,0.8,1))
					.font_(Font("Helvetica", 60)),
					stretch:1],
				[server_control.gui, 
					stretch:0],
			), stretch:0]))
		.front;

	}
}